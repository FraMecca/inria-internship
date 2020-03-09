open Ast
open Merge_accessors

module Domain = Target_sym_engine.Domain

type source_sym_values = Source_sym_engine.sym_value list
type target_sym_values = Merge_accessors.sym_value list

type source_canonical_form =
  | Int of int
  | NonSingleton of accessor
  | Block of int * source_canonical_form list

type target_canonical_form =
  | Int of int
  | NonSingleton of accessor
  | Block of int * target_canonical_form list
  | Tag of accessor * int (* We don't know about other fields *)

let compare_sym_value find_constructor_of find_domain_of (src, tgt): bool =
  let rec canonical_form_of_source_sym_value : Source_sym_engine.sym_value -> source_canonical_form = function
    | SAccessor acc ->
      begin
        match find_domain_of acc with
        | Some pi -> if Domain.is_int_singleton pi then
            Int (Domain.get_int_singleton pi)
          else
            NonSingleton acc
        | None -> assert false
      end
    | SCons (constructor, rest) ->
      let rest' = List.map canonical_form_of_source_sym_value rest in
      match constructor with
      | Int i -> assert (rest' = []); Int i
      | Bool false -> assert (rest' = []); Int 0
      | Bool true -> assert (rest' = []); Int 1
      | String _ -> failwith "not implemented"
      | Nil -> Block (0, rest')
      | Cons | Tuple _ -> Block (0, rest')
      | Variant v ->
        let open Source_env in
        match (fun v : Source_env.constructor_repr -> find_constructor_of v) v with
        | Int i -> assert (rest' = []); Int i
        | Tag t -> Block (t, rest')
  in
  let rec canonical_form_of_target_sym_value : Merge_accessors.sym_value -> target_canonical_form = function
    | VConstant i -> Int i
    | VConstructor {tag=t; args=rest} ->
      Block (t, List.map canonical_form_of_target_sym_value rest)
    | VAccessor acc ->
      (* match AcMap.find_opt acc input_space with *)
      match find_domain_of acc with
      | Some pi when Domain.is_int_singleton pi ->
        Int (Domain.get_int_singleton pi)
      | Some pi when Domain.is_tag_singleton pi ->
        Tag (acc, (Domain.get_tag_singleton pi))
      | Some _ ->
        NonSingleton acc
      | None -> assert false
  in
  let rec compare_canonical_form_ : source_canonical_form * target_canonical_form -> bool = function
    | (NonSingleton s, NonSingleton t) -> s = t (*TODO: DISCUSS: is it correct to ignore pi (domain) *)
    | (Int s, Int t) -> s = t
    | (Block (s, rest), Block (t, rest')) -> s = t &&
                                             List.combine rest rest'
                                             |> List.for_all compare_canonical_form_
    | (_, Tag _) -> assert false (* TODO: DISCUSS: nail down canonical form: Tag *)
    | _ -> false
  in
  compare_canonical_form_ ((canonical_form_of_source_sym_value src), (canonical_form_of_target_sym_value tgt))

let compare_sym_values find_constructor_of find_accessor_of sym_values target_values =
  List.combine sym_values target_values
  |> List.for_all (compare_sym_value find_constructor_of find_accessor_of)
