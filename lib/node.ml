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

module Role = struct
  type t =
    | Client of Unix.sockaddr (* client of server at (1) *)
    | Server of (Unix.sockaddr * Unix.sockaddr)
  (* server at (1) with client at (2) *)

  let to_prefix ~(src : [`Outgoing | `Incoming]) (role : t)
      (body : Message.Body.t) =
    match (src, role, body) with
    | `Outgoing, Server _, (Bin _ | Text _) ->
        "S"
    | `Outgoing, Client _, (Bin _ | Text _) ->
        "C"
    | `Outgoing, _, Ack ->
        failwith "outgoing acknowledgement"
    | `Incoming, Server _, Ack ->
        "S"
    | `Incoming, Server _, _ ->
        "C"
    | `Incoming, Client _, Ack ->
        "C"
    | `Incoming, Client _, _ ->
        "S"
end

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
  ; enc: Config.encoding
  ; role: Role.t
  ; comms_ic: In_channel.t
  ; comms_oc: Out_channel.t
  ; comms_oc_lk: Mutex.t
  ; clock: Mtime_clock.counter
  ; n_domains: int }

let establish_chat (config : Config.t) (my_inet_addr : Unix.inet_addr)
    (role : [`Client | `Server]) : t =
  Logs.set_reporter @@ Reporter.make Format.err_formatter ;
  let reporting_lk = Mutex.create () in
  Logs.set_reporter_mutex
    ~lock:(fun () -> Mutex.lock reporting_lk)
    ~unlock:(fun () -> Mutex.unlock reporting_lk) ;
  Logs.set_level ~all:true
    (if config.debug then Some Logs.Debug else Some Logs.Info) ;
  let clock = Mtime_clock.counter () in
  let role, in_descr, out_descr =
    match role with
    | `Server ->
        let sock = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
        let my_addrs = ExtUnix.All.Ioctl.siocgifconf ~sock in
        let my_sockaddr = Unix.ADDR_INET (my_inet_addr, config.port) in
        Unix.bind sock my_sockaddr ;
        Unix.listen sock 1 ;
        Logs.info (fun m ->
            m "listening at port %d on: %s." config.port
              ( String.concat "; "
              @@ ListLabels.map ~f:(fun x ->
                     Printf.sprintf "%s (%s)" (fst x) (snd x) )
              @@ ListLabels.filter my_addrs ~f:(fun x -> fst x <> "lo") )
              ~tags:(Reporter.stamp clock) ) ;
        let[@warning "-8"] ( in_descr
                           , ( Unix.ADDR_INET (their_inet_addr, their_port) as
                             client_sockaddr ) ) =
          Unix.accept ~cloexec:true sock
        in
        Logs.info (fun m ->
            m "accepted connection from %s outgoing from port %d"
              (Unix.string_of_inet_addr their_inet_addr)
              their_port ~tags:(Reporter.stamp clock) ) ;
        let out_descr = Unix.dup ~cloexec:true in_descr in
        (Role.Server (my_sockaddr, client_sockaddr), in_descr, out_descr)
    | `Client ->
        let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        let my_sockaddr = Unix.ADDR_INET (my_inet_addr, config.port) in
        Unix.connect sock my_sockaddr ;
        Logs.info (fun m ->
            m "connected to %s at port %d"
              (Unix.string_of_inet_addr my_inet_addr)
              config.port ~tags:(Reporter.stamp clock) ) ;
        (Role.Client my_sockaddr, sock, Unix.dup sock)
  in
  { in_acks= ref SpanDigestMap.empty
  ; in_acks_lk= Mutex.create ()
  ; out_acks= ref SpanDigestMap.empty
  ; out_acks_lk= Mutex.create ()
  ; enc= config.enc
  ; role
  ; comms_ic= Unix.in_channel_of_descr in_descr
  ; comms_oc= Unix.out_channel_of_descr out_descr
  ; comms_oc_lk= Mutex.create ()
  ; clock
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
  Fun.flip Option.map (Message.create stdin stdout node.clock node.enc)
    (fun msg ->
      Logs.info (fun m ->
          m "created message %s-%s from console input with contents: %s"
            (Role.to_prefix ~src:`Outgoing node.role msg.body)
            (Message.to_identifier msg)
            (Message.Body.to_string msg.body)
            ~tags:(Reporter.stamp node.clock) ) ;
      msg )

let read_incoming node =
  let msg = Message.read node.comms_ic in
  Logs.info (fun m ->
      m "read message %s-%s from communication channel with contents: %s"
        (Role.to_prefix ~src:`Incoming node.role msg.body)
        (Message.to_identifier msg)
        (Message.Body.to_string msg.body)
        ~tags:(Reporter.stamp node.clock) ) ;
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
              ~tags:(Reporter.stamp node.clock) ) ) ;
  SpanDigestMap.iter !(node.out_acks) ~f:(fun ~key:(s, d) ~data ->
      if not data then
        Logs.warn (fun m ->
            m
              "acknowledgement of outgoing message M-%f-%s was not received \
               prior to shutdown"
              (Mtime.Span.to_float_ns s) (Digest.to_hex d)
              ~tags:(Reporter.stamp node.clock) ) )

let converse (node : t) : unit =
  Logs.debug (fun m ->
      m "conversation opened" ~tags:(Reporter.stamp node.clock) ) ;
  let pool = T.setup_pool ~num_domains:(node.n_domains - 1) () in
  Sys.catch_break true ;
  Fun.protect
    ~finally:(fun () ->
      report_unacknowledged node ;
      Out_channel.close node.comms_oc ;
      T.teardown_pool pool )
    (fun () ->
      T.run pool (fun () ->
          T.split pool
            (fun () ->
              Logs.debug (fun m ->
                  m "beginning to read STDIN for messages"
                    ~tags:(Reporter.stamp node.clock) ) ;
              handle_outgoing pool node )
            (fun () ->
              Logs.debug (fun m ->
                  m "beginning to read from socket for messages"
                    ~tags:(Reporter.stamp node.clock) ) ;
              handle_incoming pool node ) ) )
