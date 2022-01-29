(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Type-checking of the module language and typed ast hooks

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

(* Should be in Envaux, but it breaks the build of the debugger *)
val initial_env:
  loc:Location.t -> safe_string:bool ->
  initially_opened_module:string option ->
  open_implicit_modules:string list -> Env.t

