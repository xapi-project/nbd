
let with_command ~command ~strict test =
  match Sys.command ("command -v " ^ command) with
  | 0 -> test ()
  | _ ->
    if strict then failwith ("Command " ^ command ^ " not present")
    else Printf.printf "!!! Skipping test because command %s is not available" command

let script name ~requires (cli, strict) =
  with_command
    ~command:requires
    ~strict
    (fun () ->
       Alcotest.(check int)
         name
         0
         (Sys.command (name ^ " " ^ cli))
    )

let cli =
  let doc = "Path to nbd CLI should be first command-line argument" in
  Cmdliner.Arg.(required & opt ~vopt:None (some string) None & info ["cli"] ~docv:"CLI" ~doc)

let strict =
  let doc = {|If present, the test will fail when the required program is not
              installed. Otherwise the test will simply be skipped.|}
  in
  Cmdliner.Arg.(value & flag & info ["strict"] ~env:(env_var "STRICT") ~doc)

let opts = Cmdliner.Term.(const (fun cli strict -> (cli, strict)) $ cli $ strict)

let () =
  Alcotest.run_with_args
    "Nbd CLI interoperability tests"
    opts
    [ ("compatibility with qemu-img",
       ["data copying", `Slow, script "./test-qemu.sh" ~requires:"qemu-img"]
      )
    ; ("compatibility with nbd-client",
       ["listing exports", `Slow, script "./test-nbd-client.sh" ~requires:"nbd-client"]
      )
    ]
