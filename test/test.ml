let stub_test () =
  Alcotest.(check bool) "same bool" true true

let () =
  Alcotest.run "Stub"
  [
    ( "stub",
      [
        Alcotest.test_case "stub" `Quick stub_test;
      ]
    );
  ]
