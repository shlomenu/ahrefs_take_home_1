type encoding = Bin | Text

type t =
  { log_file: string
  ; enc: encoding
  ; my_inet_addr: Unix.inet_addr option
  ; port: int
  ; role: [`Client | `Server] option
  ; n_domains: int
  ; debug: bool }

let default : t =
  { log_file= "ahrefs_take_home_1.log"
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
    [ ( "--decode-text"
      , Arg.Unit (fun () -> config := {!config with enc= Text})
      , "By default, no codec is applied to the contents of messages before \
         logging.  If this argument is passed, message contents will be \
         decoded as text rather than left as bytes." )
    ; ( "-a"
      , Arg.String
          (fun v ->
            config :=
              {!config with my_inet_addr= Some (Unix.inet_addr_of_string v)} )
      , "For clients, the IP address at which they expect to be able to \
         connect to the server; for servers, optionally provides the address \
         at which connections will be accepted." )
    ; ( "-p"
      , Arg.Int (fun port -> config := {!config with port})
      , "For clients, the port at which they expect to connect at the supplied \
         IP address; for servers, the port at which connections may be \
         received." )
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
      , "The number of green threads to use; bounded between two and eight." )
    ; ( "--debug"
      , Arg.Unit (fun () -> config := {!config with debug= true})
      , "If provided, debug-level messages will appear alongside info-level \
         messages in logs." ) ]
    (fun _ ->
      failwith
        "undefined anonymous argument provided: this program does not accept \
         or require any anonymous arguments." )
    "A simple one-on-one chat server/client." ;
  !config

let validate config =
  match (config.my_inet_addr, config.role) with
  | None, Some (`Server as role) ->
      Some (Unix.inet_addr_any, role)
  | Some my_inet_addr, Some role ->
      Some (my_inet_addr, role)
  | _ ->
      None

let validate_exn config =
  match (config.my_inet_addr, config.role) with
  | None, Some (`Server as role) ->
      (Unix.inet_addr_any, role)
  | None, Some `Client ->
      failwith
        "IP address of server must be provided via -a flag when operating in \
         client mode"
  | Some my_inet_addr, Some role ->
      (my_inet_addr, role)
  | _, None ->
      failwith
        "please specify either server mode by passing -s or client mode by \
         passing -c."
