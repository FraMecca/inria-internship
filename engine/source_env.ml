open Ast

module ConstructorMap = Map.Make(String)

type type_env = (type_decl * constructor_decl) ConstructorMap.t

let build_type_env type_decls =
  let add_type_decl env type_decl =
    let add_constructor env constructor =
      ConstructorMap.add constructor.constructor_name (type_decl, constructor) env in
    List.fold_left add_constructor env type_decl.constructors in
  List.fold_left add_type_decl ConstructorMap.empty type_decls

let constructor_arity type_env : constructor -> int = function
  | Nil | Unit | Int _ | Bool _ | String _ -> 0
  | Cons -> 2
  | Tuple n -> n
  | Variant name ->
     let constructor_decl = ConstructorMap.find name type_env |> snd in
     List.length constructor_decl.args

type constructor_repr =
  | Int of int
  | Tag of int

type type_repr_env = constructor_repr ConstructorMap.t

let build_type_repr_env type_decls =
  let add_type_decl env type_decl =
    let add_constructor (next_int, next_tag, env) constructor =
      let next_int, next_tag, repr =
        if constructor.args = [] then
          (next_int + 1, next_tag, Int next_int)
        else
          (next_int, next_tag + 1, Tag next_tag)
      in
      (next_int, next_tag,
       ConstructorMap.add constructor.constructor_name repr env) in
    let (_, _, env) = List.fold_left add_constructor (0, 0, env) type_decl.constructors in
    env in
  List.fold_left add_type_decl ConstructorMap.empty type_decls
