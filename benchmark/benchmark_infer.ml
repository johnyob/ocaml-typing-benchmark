open Core_bench
open Ocaml_typing_benchmark_parsing
open Ocaml_typing_benchmark_typing
open Ocaml_typing_benchmark_driver
open Parsetree

let infer ~env exp = Typecore.type_expression env exp

let initial_env () =
  Compmisc.init_path ();
  Compmisc.initial_env ()


let pexp pexp_desc =
  { pexp_desc
  ; pexp_attributes = []
  ; pexp_loc_stack = []
  ; pexp_loc = Location.none
  }


let t1 =
  Bench.Test.create
    ~name:"constant: int"
    (let env = initial_env () in
     let exp = pexp (Pexp_constant (Pconst_integer ("1", None))) in
     fun () -> infer ~env exp)


let prim_eq = Longident.Lident "=" |> Location.mknoloc
let prim_add = Longident.Lident "+" |> Location.mknoloc
let prim_sub = Longident.Lident "-" |> Location.mknoloc
let prim_div = Longident.Lident "/" |> Location.mknoloc
let prim_mul = Longident.Lident "*" |> Location.mknoloc

let t2 =
  Bench.Test.create
    ~name:"primitives"
    (let env = initial_env () in
     let exp =
       let lhs =
         let rhs =
           let lhs =
             pexp
               (Pexp_apply
                  ( pexp
                      (Pexp_apply
                         ( pexp (Pexp_ident prim_div)
                         , [ ( Nolabel
                             , pexp (Pexp_constant (Pconst_integer ("2", None)))
                             )
                           ] ))
                  , [ Nolabel, pexp (Pexp_constant (Pconst_integer ("1", None)))
                    ] ))
           in
           let rhs =
             pexp
               (Pexp_apply
                  ( pexp
                      (Pexp_apply
                         ( pexp (Pexp_ident prim_mul)
                         , [ ( Nolabel
                             , pexp (Pexp_constant (Pconst_integer ("0", None)))
                             )
                           ] ))
                  , [ Nolabel, pexp (Pexp_constant (Pconst_integer ("1", None)))
                    ] ))
           in
           pexp
             (Pexp_apply
                ( pexp
                    (Pexp_apply (pexp (Pexp_ident prim_sub), [ Nolabel, lhs ]))
                , [ Nolabel, rhs ] ))
         in
         pexp
           (Pexp_apply
              ( pexp
                  (Pexp_apply
                     ( pexp (Pexp_ident prim_add)
                     , [ ( Nolabel
                         , pexp (Pexp_constant (Pconst_integer ("1", None))) )
                       ] ))
              , [ Nolabel, rhs ] ))
       in
       pexp
         (Pexp_apply
            ( pexp (Pexp_apply (pexp (Pexp_ident prim_eq), [ Nolabel, lhs ]))
            , [ Nolabel, pexp (Pexp_constant (Pconst_integer ("12", None))) ] ))
     in
     fun () -> infer ~env exp)


let add_term env =
  let rec_flag, type_decls =
    Lexing.from_string
      "type 'a term = \n\
      \        | Int : int -> int term\n\
      \        | Succ : int term -> int term\n\
      \        | Bool : bool -> bool term\n\
      \        | If : bool term * 'a term * 'a term -> 'a term\n\
      \        | Pair : 'a term * 'b term -> ('a * 'b) term\n\
      \        | Fst : ('a * 'b) term -> 'a term\n\
      \        | Snd : ('a * 'b) term -> 'b term"
    |> Parse.type_decls
  in
  Typedecl.transl_type_decl env rec_flag type_decls


let t3 =
  Bench.Test.create
    ~name:"gadt - eval"
    (let env = initial_env () in
     let _, env = add_term env in
     let exp =
       Lexing.from_string
         "let fst (x, y) = x in\n\
         \      let snd (x, y) = y in\n\
         \      let rec eval : type a. a term -> a = fun t -> match t with\n\
         \        | Int n -> n\n\
         \        | Bool b -> b\n\
         \        | Succ t -> (eval t) + 1\n\
         \        | If (t1, t2, t3) -> if eval t1 then eval t2 else eval t3\n\
         \        | Pair (t1, t2) -> (eval t1, eval t2)\n\
         \        | Fst t -> fst (eval t)\n\
         \        | Snd t -> snd (eval t)\n\
         \      in ()"
       |> Parse.expression
     in
     fun () -> infer ~env exp)


let tests = [ t1; t2; t3 ]
let command = Bench.make_command tests