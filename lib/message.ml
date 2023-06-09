module Body = struct
  type t = Ack | Bin of bytes | Text of string

  let type_descr = function Ack -> "ack" | Bin _ -> "bin" | Text _ -> "text"

  let to_content = function
    | Ack ->
        None
    | Bin b ->
        Some b
    | Text t ->
        Some (Bytes.of_string t)

  let to_length_and_content body =
    Option.map (fun b -> (Bytes.length b, b)) (to_content body)

  let to_string = function
    | Ack ->
        "<acknowledgement>"
    | Bin _ ->
        "<no-decoding>"
    | Text t ->
        t
end

type t = {id: Digest.t; time: Mtime.span; body: Body.t}

let to_key msg = (msg.time, msg.id)

let to_identifier msg =
  Printf.sprintf "%f-%s"
    (Mtime.Span.to_float_ns msg.time)
    (Digest.to_hex msg.id)

let create (ic : In_channel.t) (oc : Out_channel.t)
    (clock : Mtime_clock.counter) (enc : Config.encoding) : t option =
  Out_channel.output_string oc "msg: " ;
  Out_channel.flush oc ;
  Option.bind (In_channel.input_line ic) (fun s ->
      if String.length s > 0 then
        Some
          { id= Digest.string s
          ; time= Mtime_clock.count clock
          ; body=
              ( match enc with
              | Bin ->
                  Body.Bin (Bytes.of_string s)
              | Text ->
                  Body.Text s ) }
      else None )

let read (ic : In_channel.t) : t =
  let id = Digest.input ic in
  let time_str = Option.get @@ In_channel.input_line ic in
  let time = Option.get @@ Mtime.Span.of_float_ns @@ Float.of_string time_str in
  match Option.get @@ In_channel.input_line ic with
  | "ack" ->
      {id; time; body= Ack}
  | ("bin" | "text") as type_descr ->
      let len_str = Option.get @@ In_channel.input_line ic in
      let len = int_of_string len_str in
      let b = Bytes.create len in
      Option.get @@ In_channel.really_input ic b 0 len ;
      { id
      ; time
      ; body=
          ( match type_descr with
          | "bin" ->
              Bin b
          | "text" ->
              Text (Bytes.to_string b)
          | type_descr ->
              failwith ("unrecognized body type description " ^ type_descr) ) }
  | type_descr ->
      failwith ("unrecognized body type description " ^ type_descr)

let write (oc : Out_channel.t) (msg : t) : unit =
  Digest.output oc msg.id ;
  Out_channel.output_string oc
    ((Float.to_string @@ Mtime.Span.to_float_ns msg.time) ^ "\n") ;
  Out_channel.output_string oc (Body.type_descr msg.body ^ "\n") ;
  ( match Body.to_length_and_content msg.body with
  | Some (len, content) ->
      Out_channel.output_string oc (Int.to_string len ^ "\n") ;
      Out_channel.output_bytes oc content
  | None ->
      () ) ;
  Out_channel.flush oc
