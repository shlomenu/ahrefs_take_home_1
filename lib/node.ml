open MoreLabels

module T = struct
  include Domainslib.Task

  let split pool f_1 f_2 =
    let t_1 = async pool f_1 in
    let t_2 = async pool f_2 in
    await pool t_1 ; await pool t_2
end

module Mutex = struct
  include Mutex

  let with_lock lk f =
    Fun.protect ~finally:(fun () -> unlock lk) (fun () -> lock lk ; f ())
end

type role =
  | Client of Unix.sockaddr (* client of server at (1) *)
  | Server of (Unix.sockaddr * Unix.sockaddr)
(* server at (1) with client at (2) *)

module SpanDigestMap = Map.Make (struct
  type t = Mtime.Span.t * Digest.t

  let compare (s_a, d_a) (s_b, d_b) =
    match Mtime.Span.compare s_a s_b with
    | c when c <> 0 ->
        c
    | _ ->
        Digest.compare d_a d_b
end)

type t =
  { out_acks: bool SpanDigestMap.t ref
  ; out_acks_lk: Mutex.t
  ; in_acks: bool SpanDigestMap.t ref
  ; in_acks_lk: Mutex.t
  ; logs_oc: Out_channel.t
  ; max_msg_len: int
  ; enc: Config.encoding
  ; role: role
  ; comms_ic: In_channel.t
  ; comms_oc: Out_channel.t
  ; comms_oc_lk: Mutex.t
  ; clock: Mtime_clock.counter
  ; n_domains: int }

let establish_chat (config : Config.t) (my_inet_addr : Unix.inet_addr)
    (role : [`Client | `Server]) : t =
  let role, in_descr, out_descr =
    match role with
    | `Server ->
        let sock = Unix.socket ~cloexec:true Unix.PF_INET6 Unix.SOCK_STREAM 0 in
        let my_sockaddr = Unix.ADDR_INET (my_inet_addr, config.port) in
        Unix.bind sock my_sockaddr ;
        Unix.listen sock 1 ;
        let in_descr, client_sockaddr = Unix.accept ~cloexec:true sock in
        let out_descr = Unix.dup ~cloexec:true in_descr in
        (Server (my_sockaddr, client_sockaddr), in_descr, out_descr)
    | `Client ->
        let sock = Unix.socket Unix.PF_INET6 Unix.SOCK_STREAM 0 in
        let my_sockaddr = Unix.ADDR_INET (my_inet_addr, config.port) in
        Unix.connect sock my_sockaddr ;
        (Client my_sockaddr, sock, Unix.dup sock)
  in
  { in_acks= ref SpanDigestMap.empty
  ; in_acks_lk= Mutex.create ()
  ; out_acks= ref SpanDigestMap.empty
  ; out_acks_lk= Mutex.create ()
  ; logs_oc=
      (let logs_oc, reporter = File_reporter.make config.log_file in
       Logs.set_reporter reporter ;
       let reporting_lk = Mutex.create () in
       Logs.set_reporter_mutex
         ~lock:(fun () -> Mutex.lock reporting_lk)
         ~unlock:(fun () -> Mutex.unlock reporting_lk) ;
       Logs.set_level ~all:true
         (if config.debug then Some Logs.Debug else Some Logs.Info) ;
       logs_oc )
  ; max_msg_len= config.max_msg_len
  ; enc= config.enc
  ; role
  ; comms_ic= Unix.in_channel_of_descr in_descr
  ; comms_oc= Unix.out_channel_of_descr out_descr
  ; comms_oc_lk= Mutex.create ()
  ; clock= Mtime_clock.counter ()
  ; n_domains= config.n_domains }

let mark ~(f : bool option -> bool option) (lk : Mutex.t)
    (acks : bool SpanDigestMap.t ref) (key : SpanDigestMap.key) =
  Mutex.with_lock lk (fun () -> acks := SpanDigestMap.update !acks ~key ~f)

let mark_unacked =
  mark ~f:(function
    | None ->
        Some false
    | Some false ->
        failwith "redundant call to mark_unacked"
    | data ->
        data )

let mark_out_unacked node = mark_unacked node.out_acks_lk node.out_acks

let mark_in_unacked node = mark_unacked node.in_acks_lk node.in_acks

let mark_acked =
  mark ~f:(function
    | None | Some false ->
        Some true
    | Some true ->
        failwith "redundant call to mark_acked" )

let mark_out_acked node = mark_acked node.out_acks_lk node.out_acks

let mark_in_acked node = mark_acked node.in_acks_lk node.in_acks

let read_outgoing node =
  Fun.flip Option.map
    (Message.create stdin node.max_msg_len node.clock node.enc) (fun msg ->
      Logs.info (fun m ->
          m "created message M-%f-%s from console input with contents: %s"
            (Mtime.Span.to_float_ns msg.time)
            (Digest.to_hex msg.id)
            (Message.Body.to_string msg.body)
            ~tags:(File_reporter.stamp_of_counter node.clock) ) ;
      msg )

let read_incoming node =
  let msg = Message.read node.comms_ic in
  Logs.info (fun m ->
      m "read message Y-%f-%s from communication channel with contents: %s"
        (Mtime.Span.to_float_ns msg.time)
        (Digest.to_hex msg.id)
        (Message.Body.to_string msg.body)
        ~tags:(File_reporter.stamp_of_counter node.clock) ) ;
  msg

let send_outgoing node msg =
  Mutex.with_lock node.comms_oc_lk (fun () -> Message.write node.comms_oc msg)

let send_acknowledgement node msg = send_outgoing node {msg with body= Ack}

let rec handle_outgoing pool node =
  match read_outgoing node with
  | Some msg ->
      T.split pool
        (fun () -> handle_outgoing pool node)
        (fun () ->
          send_outgoing node msg ;
          mark_out_unacked node (Message.to_key msg) )
  | None ->
      handle_outgoing pool node

let rec handle_incoming pool node =
  let msg = read_incoming node in
  let msg_key = Message.to_key msg in
  T.split pool
    (fun () -> handle_incoming pool node)
    (fun () ->
      match msg.body with
      | Ack ->
          mark_out_acked node msg_key
      | _ ->
          T.split pool
            (fun () -> mark_in_unacked node msg_key)
            (fun () ->
              send_acknowledgement node msg ;
              mark_in_acked node msg_key ) )

let report_unacknowledged node =
  SpanDigestMap.iter !(node.in_acks) ~f:(fun ~key:(s, d) ~data ->
      if not data then
        Logs.warn (fun m ->
            m
              "acknowledgement of incoming message Y-%f-%s was not sent prior \
               to shutdown"
              (Mtime.Span.to_float_ns s) (Digest.to_hex d)
              ~tags:(File_reporter.stamp_of_counter node.clock) ) ) ;
  SpanDigestMap.iter !(node.out_acks) ~f:(fun ~key:(s, d) ~data ->
      if not data then
        Logs.warn (fun m ->
            m
              "acknowledgement of outgoing message M-%f-%s was not received \
               prior to shutdown"
              (Mtime.Span.to_float_ns s) (Digest.to_hex d)
              ~tags:(File_reporter.stamp_of_counter node.clock) ) )

let converse (node : t) : unit =
  let pool = T.setup_pool ~num_domains:(node.n_domains - 1) () in
  Sys.catch_break true ;
  Fun.protect
    ~finally:(fun () ->
      report_unacknowledged node ;
      Out_channel.close node.comms_oc ;
      Out_channel.close node.logs_oc ;
      T.teardown_pool pool )
    (fun () ->
      T.run pool (fun () ->
          T.split pool
            (fun () -> handle_outgoing pool node)
            (fun () -> handle_incoming pool node) ) )
