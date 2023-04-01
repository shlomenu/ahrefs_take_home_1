type encoding = Bin | Text

type t =
  { log_file: string
  ; max_msg_len: int
  ; enc: encoding
  ; my_inet_addr: Unix.inet_addr option
  ; port: int
  ; role: [`Client | `Server] option
  ; n_domains: int
  ; debug: bool }

let default : t =
  { log_file= "ahrefs_take_home_1.log"
  ; max_msg_len= Sys.max_string_length
  ; enc= Bin
  ; my_inet_addr= None
  ; port= 9000
  ; role= None
  ; n_domains= 2
  ; debug= false }

let read_cli () : t =
  let config = ref default in
  let unclear_role_err_msg =
    "either -s or -c must be provided; duplicate or conflicting flags received."
  in
  Arg.parse
    [ ( "-o"
      , Arg.String (fun log_file -> config := {!config with log_file})
      , "The file in which events (msg sends/receives) and acknowledgements of \
         receipt will be logged.  This file is the only location in which this \
         information will appear." )
    ; ( "--max-msg-len"
      , Arg.Int
          (fun v ->
            config :=
              {!config with max_msg_len= min (Int.abs v) !config.max_msg_len} )
      , "The size of the buffer into which messages will be stored.  Defaults \
         to Sys.max_string_length.  May be lowered to reduce memory \
         consumption." )
    ; ( "--decode-text"
      , Arg.Unit (fun () -> config := {!config with enc= Text})
      , "By default, no codec is applied to the contents of messages before \
         logging.  If this argument is passed, message contents will be \
         decoded as text rather than left as bytes." )
    ; ( "-a"
      , Arg.String
          (fun v ->
            config :=
              {!config with my_inet_addr= Some (Unix.inet_addr_of_string v)} )
      , "For servers, the IP address at which a client may connect; for \
         clients, the IP address at which they expect to be able to connect to \
         the server" )
    ; ( "-p"
      , Arg.Int (fun port -> config := {!config with port})
      , "For servers, the port at which connections may be received; for \
         clients, the port at which they expect to connect at the supplied IP \
         address" )
    ; ( "-s"
      , Arg.Unit
          (fun () ->
            match !config.role with
            | None ->
                config := {!config with role= Some `Server}
            | Some _ ->
                failwith unclear_role_err_msg )
      , "Run the program in server mode." )
    ; ( "-c"
      , Arg.Unit
          (fun () ->
            match !config.role with
            | None ->
                config := {!config with role= Some `Client}
            | Some _ ->
                failwith unclear_role_err_msg )
      , "Run the server in client mode" )
    ; ( "--threads"
      , Arg.Int
          (fun v ->
            config := {!config with n_domains= min 8 (max !config.n_domains v)}
            )
      , "The number of green threads to use; at least two will be used in any \
         case so that message sending and receiving can occur in parallel." )
    ; ( "--debug"
      , Arg.Unit (fun () -> config := {!config with debug= true})
      , "If provided, debug-level messages will appear alongside informative \
         messages in the log file." ) ]
    (fun _ ->
      failwith
        "undefined anonymous argument provided: this program does not accept \
         or require any anonymous arguments." )
    "A simple one-on-one chat server/client." ;
  !config

let validate config =
  match (config.my_inet_addr, config.role) with
  | Some my_inet_addr, Some role ->
      Some (my_inet_addr, role)
  | _ ->
      None

let validate_exn config =
  match (config.my_inet_addr, config.role) with
  | Some my_inet_addr, Some role ->
      (my_inet_addr, role)
  | None, None ->
      failwith
        "IP address and port not provided: please specify via the -a and -p \
         flags"
  | None, Some _ ->
      failwith "IP address not provided: please specify via the -a flag."
  | Some _, None ->
      failwith "port not provided: please specify via the -p flag."
