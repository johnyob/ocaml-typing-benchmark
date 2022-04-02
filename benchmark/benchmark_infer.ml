open Core
open Core_bench
open Ocaml_typing_benchmark_parsing
open Ocaml_typing_benchmark_typing
open Ocaml_typing_benchmark_driver
open Parsetree

let infer ~env exp = Typecore.type_expression env exp

let initial_env () =
  Clflags.principal := true;
  Clflags.recursive_types := true;
  Compmisc.init_path ();
  Compmisc.initial_env ()




let pexp pexp_desc =
  { pexp_desc
  ; pexp_attributes = []
  ; pexp_loc_stack = []
  ; pexp_loc = Location.none
  }


let ppat ppat_desc =
  { ppat_desc
  ; ppat_attributes = []
  ; ppat_loc_stack = []
  ; ppat_loc = Location.none
  }


let pvb ppat_desc pexp_desc =
  { pvb_loc = Location.none
  ; pvb_attributes = []
  ; pvb_expr = pexp pexp_desc
  ; pvb_pat = ppat ppat_desc
  }


let add_type_decl type_decl env =
  let rec_flag, type_decls = Lexing.from_string type_decl |> Parse.type_decls in
  Typedecl.transl_type_decl env rec_flag type_decls |> snd


let add_list = add_type_decl {| type 'a list = Nil | Cons of 'a * 'a list |}

let add_term =
  add_type_decl
    "type 'a term = \n\
    \        | Int : int -> int term\n\
    \        | Succ : int term -> int term\n\
    \        | Bool : bool -> bool term\n\
    \        | If : bool term * 'a term * 'a term -> 'a term\n\
    \        | Pair : 'a term * 'b term -> ('a * 'b) term\n\
    \        | Fst : ('a * 'b) term -> 'a term\n\
    \        | Snd : ('a * 'b) term -> 'b term"


let t4 =
  Bench.Test.create
    ~name:"gadt - eval"
    (let env = initial_env () in
     let env = add_term env in
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


let create_test_infer_exp ~name ?(env = initial_env ()) exp =
  
  Bench.Test.create
    ~name
    (let exp' = Lexing.from_string exp |> Parse.expression in
     fun () -> 
      (* print_endline exp; *)
      infer ~env exp')


let t1' =
  create_test_infer_exp
    ~name:"gcd"
    "let mod' = fun (m : int) (n : int) -> 0 \n\
    \ in \n\
    \ let rec gcd = fun m n -> \n\
    \   if n = 0 then m else gcd n (mod' m n)\n\
    \ in \n\
    \ gcd 55 200"


let t2' =
  create_test_infer_exp
    ~name:"fact"
    "let rec fact = fun n ->\n\
    \   if n = 0 then 1 else n * fact (n - 1)\n\
    \ in\n\
    \ fact 1000"


let t3' = create_test_infer_exp ~name:"arith" "1 + (2 / 1 - 0 * 1) = 12"

let t4' =
  create_test_infer_exp
    ~name:"making change"
    ~env:(add_list (initial_env ()))
    "let le = fun (m : int) (n : int) -> true in\n\
    \ let raise = fun () -> Nil in\n\
    \ let rec change = fun till amt -> \n\
    \   match (till, amt) with\n\
    \   | (_, 0) -> Nil\n\
    \   | (Nil, _) -> raise ()\n\
    \   | (Cons (c, till), amt) ->\n\
    \       if le amt c then change till amt\n\
    \       else Cons (c, change (Cons (c, till)) (amt - c))\n\
    \ in ()"


let t5' =
  create_test_infer_exp
    ~name:"map"
    ~env:(add_list (initial_env ()))
    "let rec map = fun t f ->\n\
    \   match t with\n\
    \   | Nil -> Nil\n\
    \   | Cons (x, t) -> Cons (f x, map t f)\n\
    \ in\n\
    \ let f = fun x -> x + 1 in\n\
    \ map Nil f"


let t6' =
  create_test_infer_exp
    ~name:"iter"
    ~env:(add_list (initial_env ()))
    "let rec iter = fun t f ->\n\
    \   match t with\n\
    \   | Nil -> Nil\n\
    \   | Cons (x, t) -> f x; iter t f\n\
    \ in\n\
    \ iter Nil (fun _ -> ())"


let add_tree =
  add_type_decl {| type 'a tree = Empty_tree | Node of 'a tree * 'a * 'a tree |}


let add_option = add_type_decl {| type 'a option = None | Some of 'a |}

let t7' =
  create_test_infer_exp
    ~name:"lookup (tree)"
    ~env:((initial_env ()) |> add_tree |> add_option)
    "let le = fun (m : int) (n : int) -> true\n\
    \ in let rec lookup = fun t key ->\n\
    \  match t with\n\
    \  | Empty_tree -> None\n\
    \  | Node (l, (key', x), r) -> \n\
    \      if key = key' then Some x else \n\
    \      if le key key' then lookup l key else\n\
    \      lookup r key\n\
    \ in ()"


let t8' =
  create_test_infer_exp
    ~name:"insertion sort"
    ~env:((initial_env ()) |> add_list)
    "let leq = fun (m : int) (n : int) -> true in\n\
    \ let rec insert = fun x t ->\n\
    \   match t with\n\
    \   | Nil -> Cons (x, Nil)\n\
    \   | Cons (y, t) -> \n\
    \       if leq x y then Cons (x, Cons (y, t))\n\
    \       else Cons (y, insert x t)\n\
    \ in \n\
    \ let rec insertion_sort = fun t ->\n\
    \   match t with\n\
    \   | Nil -> Nil\n\
    \   | Cons (x, t) -> insert x (insertion_sort t)\n\
    \ in\n\
    \ ()"


let t9' =
  create_test_infer_exp
    ~name:"is_even, is_odd"
    "let rec is_even = fun n -> \n\
    \   if n = 0 then true else is_odd (n - 1)\n\
    \ and is_odd = fun n -> \n\
    \   if n = 1 then true else is_even (n - 1)\n\
    \ in\n\
    \ ()"


let add_perfect_tree =
  add_type_decl
    {| type 'a perfect_tree = Empty | Cons of 'a * ('a * 'a) perfect_tree |}


let t10' =
  create_test_infer_exp
    ~name:"perfect_tree length"
    ~env:((initial_env ()) |> add_perfect_tree)
    "let rec length : type a. a perfect_tree -> int = \n\
    \  fun t -> \n\
    \    match t with\n\
    \    | Empty -> 0\n\
    \    | Cons (_, t) -> 1 + 2 * length t\n\
    \ in ()"


let t11' =
  create_test_infer_exp
    ~name:"term eval"
    ~env:((initial_env ()) |> add_term)
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


let add_key =
  add_type_decl
    {| 
      type 'a key = 
        | String : int -> string key
        | Bool : int -> bool key
    |}


let add_elem =
  add_type_decl {|
      type elem = Elem : 'value key * 'value -> elem
    |}


let add_elem_mapper =
  add_type_decl
    {|
      type elem_mapper = { f : 'a. 'a key -> 'a -> 'a }
    |}


let t12' =
  create_test_infer_exp
    ~name:"dependent associative list map_elem"
    ~env:((initial_env ()) |> add_key |> add_elem |> add_list |> add_elem_mapper)
    {|
      let rec map = fun t f ->
        match t with
        | Nil -> Nil
        | Cons (x, t) -> Cons (f x, map t f)
      in
      let map_elem = fun (Elem (key, value)) mapper ->
        Elem (key, mapper.f key value)
      in
      let map_assoc_list = fun t mapper ->
        map t (fun elem -> map_elem elem mapper)
      in () 
    |}


let tests = [ t1'; t2'; t3'; t4'; t5'; t6'; t7'; t8'; t9'; t10'; t11'; t12' ]
let command = Bench.make_command tests
let id = Longident.Lident "id" |> Location.mknoloc

let def_id ~in_ =
  pexp
    (Pexp_let
       ( Nonrecursive
       , [ pvb
             (Ppat_var (Location.mknoloc "id"))
             (Pexp_fun
                ( Nolabel
                , None
                , ppat (Ppat_var (Location.mknoloc "x"))
                , pexp (Pexp_ident (Longident.Lident "x" |> Location.mknoloc))
                ))
         ]
       , in_ ))


let id_app_stress_test =
  Bench.Test.create_indexed
    ~name:"id app - stress test"
    ~args:[ 1; 5; 10; 50; 100; 200; 500; 1000; 2000 ]
    (fun n ->
      Staged.stage (fun () ->
          let env = initial_env () in
          let rec loop = function
            | 0 -> pexp (Pexp_ident id)
            | n ->
              pexp
                (Pexp_apply (loop (n - 1), [ Nolabel, pexp (Pexp_ident id) ]))
          in
          infer ~env (def_id ~in_:(loop n))))


let id_let_stress_test =
  let rec loop n =
    match n with
    | 0 ->
      pexp (Pexp_construct (Longident.Lident "()" |> Location.mknoloc, None))
    | n -> def_id ~in_:(loop (n - 1))
  in
  Bench.Test.create_indexed
    ~name:"id let - stress test"
    ~args:[ 1; 5; 10; 50; 100; 200; 500; 1000; 2000 ]
    (fun n -> Staged.stage (fun () -> infer ~env:(initial_env ()) (loop n)))

let stress_tests = [ id_app_stress_test; id_let_stress_test ]
let stress_command = Bench.make_command stress_tests 
