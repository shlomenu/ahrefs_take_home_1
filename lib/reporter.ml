type t = Logs.reporter

let stamp_tag : Mtime.span Logs.Tag.def =
  Logs.Tag.def "stamp" ~doc:"Relative monotonic time stamp" Mtime.Span.pp

let stamp c = Logs.Tag.(add stamp_tag (Mtime_clock.count c) empty)

let make ppf : Logs.reporter =
  { report=
      (fun _ level ~over k msgf ->
        msgf
        @@ fun ?header ?tags fmt ->
        Option.map (Logs.Tag.find stamp_tag) tags
        |> Option.map (fun s -> Mtime.Span.to_float_ns (Option.get s))
        |> Option.value ~default:0.
        |> Format.kfprintf
             (fun _ -> over () ; k ())
             ppf
             ("%a[%0+4.0fus] @[" ^^ fmt ^^ "@]@.")
             Logs.pp_header (level, header) ) }
