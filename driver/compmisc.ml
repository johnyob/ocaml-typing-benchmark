(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*       Fabrice Le Fessant, EPI Gallium, INRIA Paris-Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Return the initial environment in which compilation proceeds. *)

let init_path ?(dir="") () =
  let dirs =
    if !Clflags.use_threads then "+threads" :: !Clflags.include_dirs
    else
      !Clflags.include_dirs
  in
  let dirs =
    !Compenv.last_include_dirs @ dirs @ Config.flexdll_dirs @
    !Compenv.first_include_dirs
  in
  let exp_dirs =
    List.map (Misc.expand_directory Config.standard_library) dirs in
  Load_path.init (dir :: List.rev_append exp_dirs (Clflags.std_include_dir ()));
  Env.reset_cache ()

(* Note: do not do init_path() in initial_env, this breaks
   toplevel initialization (PR#8227) *)

let initial_env () =
  Ident.reinit();
  Types.Uid.reinit();
  let initially_opened_module =
    if !Clflags.nopervasives then
      None
    else
      Some "Stdlib"
  in
  Typemod.initial_env
    ~loc:(Location.in_file "command line")
    ~safe_string:(Config.safe_string || not !Clflags.unsafe_string)
    ~initially_opened_module
    ~open_implicit_modules:(List.rev !Clflags.open_modules)
