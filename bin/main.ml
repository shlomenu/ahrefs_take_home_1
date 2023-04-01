let () =
  let config = Chat.Config.read_cli () in
  let my_inet_addr, role = Chat.Config.validate_exn config in
  Chat.Node.converse @@ Chat.Node.establish_chat config my_inet_addr role
