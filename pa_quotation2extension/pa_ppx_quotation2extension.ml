(**pp -syntax camlp5o -package camlp5.extfun,camlp5.parser_quotations *)
(* camlp5o *)
(* pa_string.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Pa_ppx_utils
open Pa_passthru
open Ppxutil

let has_quotation ename =
  match Quotation.find ename with
      _ -> true
    | exception Not_found -> false

let rewrite_expr_extension arg = function
  <:expr:< [%$attrid:(_,ename)$ $str:payload$ ] >> as z ->
   if has_quotation ename then
     Pcaml.handle_expr_quotation loc (ename, payload)
   else
     z
| e -> Fmt.(raise_failwithf (MLast.loc_of_expr e) "pa_ppx_quotation2extension: payload of a [%%<extension> ...] expr-extension must be a single string: %a"
            Pp_MLast.pp_expr e)

let rewrite_patt_extension arg = function
  <:patt:< [%$attrid:(_,ename)$ $str:payload$ ] >> as z ->
   if has_quotation ename then
     Pcaml.handle_patt_quotation loc (ename, payload)
   else
     z
| e -> Fmt.(raise_failwithf (MLast.loc_of_patt e) "pa_ppx_quotation2extension: payload of a [%%<extension> ...] patt-extension must be a single string: %a"
            Pp_MLast.pp_patt e)

let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
              expr = extfun ef.expr with [
                <:expr:< [%$attrid:_$ $str:_$ ] >> as z ->
                fun arg fallback ->
                Some (rewrite_expr_extension arg z)
              ] } in

let ef = EF.{ (ef) with
              patt = extfun ef.patt with [
                  <:patt:< [%$attrid:_$ $str:_$ ] >> as z ->
                  fun arg fallback ->
                  Some (rewrite_patt_extension arg z)
              ] } in

  Pa_passthru.(install { name = "pa_quotation2extension"; ef =  ef ; pass = None ; before = [] ; after = [] })
;;

install();;
