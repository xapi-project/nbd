
let () =
  Alcotest.run
    "Nbd library test suite"
    [ Mux_test.tests
    ; Protocol_test.tests
    ]
