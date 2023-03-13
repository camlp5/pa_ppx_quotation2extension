#include "version.inc"

let top = try Sys.getenv "TOP" with Not_found -> "" ;;
let destdir = ref (top ^ "/local-install") ;;

Arg.(parse [
         "-destdir", (Arg.String (fun s -> destdir := s)),
         ("<path>    Set the destination directory (default: " ^
	    !destdir ^ ")")
       ]
       (fun _ -> failwith "mk_meta: no anonymous args supported")
     "mk_meta -destdir <dir>")
;;

Fmt.(pf stdout {|
# Specifications for the "pa_ppx_quotation2extension" preprocessor:
requires = "camlp5,fmt,pa_ppx.base,camlp5.parser_quotations,pa_ppx_regexp.runtime"
version = "%s"
description = "pa_ppx_quotation2extension: pa_ppx_quotation2extension rewriter"

# For linking
package "link" (
requires = "camlp5,fmt,pa_ppx.base.link,camlp5.parser_quotations.link,pa_ppx_regexp.runtime"
archive(byte) = "pa_ppx_quotation2extension.cmo"
archive(native) = "pa_ppx_quotation2extension.cmx"
)

# For the toploop:
archive(byte,toploop) = "pa_ppx_quotation2extension.cmo"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,pa_ppx.base,camlp5.parser_quotations,pa_ppx_regexp.runtime"
  archive(syntax,preprocessor,-native) = "pa_ppx_quotation2extension.cmo"
  archive(syntax,preprocessor,native) = "pa_ppx_quotation2extension.cmx"

|} version)
