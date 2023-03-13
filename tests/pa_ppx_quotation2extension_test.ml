(**pp -syntax camlp5o *)
open OUnit2

let hd x = 2

let loc = Ploc.dummy

let test_simple ctxt =
  ()
  ; assert_equal ~cmp:Reloc.eq_expr <:expr< 1 >> <:expr< 1 >>

let suite = "Test pa_ppx_quotation2extension" >::: [
      "simple"   >:: test_simple
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

