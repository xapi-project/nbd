open OUnit

let tests =
  "Nbd library test suite" >:::
    [ Mux_test.tests
    ; Protocol_test.tests
    ]

let () =
  OUnit2.run_test_tt_main (ounit2_of_ounit1 tests)
