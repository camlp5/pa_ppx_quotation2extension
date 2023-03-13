(**pp -syntax camlp5o *)
open OUnit2

let hd x = 2

let loc = Ploc.dummy

let test_simple ctxt =
  ()
  ; assert_equal ~cmp:Reloc.eq_expr <:expr< 1 >> <:expr< 1 >>

  ; assert_equal ~cmp:Reloc.eq_expr <:expr< 1 >> [%expr {| 1 |}]
  ; assert_equal () (match [%expr {| 1 |}] with [%expr {| 1 |}] -> ())

  ; assert_equal () (match <:expr< 1 >> with <:expr:< 1 >> -> ignore(loc) ; ())
  ; assert_equal () (match [%expr.loc {| 1 |}] with [%expr.loc {| 1 |}] -> ignore(loc) ; ())

let suite = "Test pa_ppx_quotation2extension" >::: [
      "simple"   >:: test_simple
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

