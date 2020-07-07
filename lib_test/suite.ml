let () =
  Lwt_main.run @@
  Alcotest_lwt.run
    "Lwt Nbd library test suite"
    [ Protocol_test.tests
    ; Client_server_test.tests
    ];
  Alcotest.run "Synchronous Nbd library test suite" [ Mux_test.tests ]
