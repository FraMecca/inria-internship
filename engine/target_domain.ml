(* efficient unions-of-intervals using the Diet library *)
module IntSet = Diet.Int

type t = { tag: IntSet.t; int: IntSet.t }

module Set = struct
  let set interv = IntSet.add interv IntSet.empty
  let point n = set (IntSet.Interval.make n n)
  let interval low high = set (IntSet.Interval.make low high)

  let lt n = interval min_int (n - 1)
  let le n = interval min_int n
  let ge n = interval n max_int
  let gt n = interval (n+1) max_int

  let empty = IntSet.empty
  let full = interval min_int max_int
  let negate set = IntSet.diff full set
  let is_empty set = IntSet.is_empty set
  let is_full set = IntSet.is_empty (negate set)

  let equal = IntSet.equal

  let union = IntSet.union
  let inter = IntSet.inter

  let shift n set =
    let open IntSet in
    let on_interval interv acc =
      (* We have to be careful of arithmetic overflows.

         Our intervals are ranges of fixed-width numbers and may
         typically have min_int as the lower bound, max_int as the
         upper bound, or both (an interval for the full range). When
         shifting by a constant integer, an overflow or underflow
         will thus frequently occur.

         If both bounds overflow, the result is correct. For
         example, if the interval fragment is
           [max_int - 3; max_int]
         and we shift by 4, we get the interval
           [min_int; min_int+3]
         which is the right result. (Note: OCaml specifies
         a wraparound/modulo semantics for overflows.)

         We are in trouble if only one bound overflows; for example,
         shifting
           [max_int - 3; max_int]
         by just 2 would give the interval
           [max_int - 1; min_int + 1]
         which is ill-formed -- the lower bound that is larger than
         the upper bound. Instead we detect this case and split the
         result in two intervals,
           [max_int - 1; max_int]   [min_int; min_int + 1]
      *)
      let a, b = Interval.(x interv, y interv) in
      let a', b' = a + n, b + n in
      let return intervals = List.fold_right add intervals acc in
      if a' <= b' then
        (* the bounds are in the right order: no overflow, or two overflows *)
        return [Interval.make a' b']
      else
        (* b' overflowed or a' underflowed *)
        return [Interval.make min_int b';
                Interval.make a' max_int]
    in
    IntSet.fold on_interval set IntSet.empty

  let to_string set =
    let on_interval interv acc =
      let low, high = IntSet.Interval.x interv, IntSet.Interval.y interv in
      let str =
        if low = high then string_of_int low
        else Printf.sprintf "[%s; %s]"
            (if low = min_int then "-∞" else string_of_int low)
            (if high = max_int then "+∞" else string_of_int high)
      in str :: acc
    in
    if is_empty set then "∅"
    else if is_full set then "_"
    else
      IntSet.fold on_interval set []
      |> List.rev
      |> String.concat " "
end

let int set = {int = set; tag = Set.empty}
let tag set = {int = Set.empty; tag = set}

let full = {int = Set.full; tag = Set.full}
let empty = {int = Set.empty; tag = Set.empty}
let is_int_singleton dom = IntSet.cardinal dom.int = 1 && IntSet.is_empty dom.tag
let get_int_singleton dom =
  assert (is_int_singleton dom); IntSet.fold_individual (fun x _ -> x) dom.int 0
let is_tag_singleton dom = IntSet.cardinal dom.tag = 1 && IntSet.is_empty dom.tag
let get_tag_singleton dom =
  assert (is_tag_singleton dom); IntSet.fold_individual (fun x _ -> x) dom.tag 0

let is_empty dom = Set.is_empty dom.int && Set.is_empty dom.tag

let equal dom dom' = Set.equal dom.int dom'.int && Set.equal dom.tag dom'.tag

let negate dom = {
  int = Set.negate dom.int;
  tag = Set.negate dom.tag;
}
let union dom1 dom2 = {
  int = Set.union dom1.int dom2.int;
  tag = Set.union dom1.tag dom2.tag;
}
let inter dom1 dom2 = {
  int = Set.inter dom1.int dom2.int;
  tag = Set.inter dom1.tag dom2.tag;
}

let isin n = int (Set.interval 0 n)
let isout n = negate (isin n)
let isnot n = negate (int (Set.point n))

let to_string {int; tag} =
  let show_int set = "Int " ^ Set.to_string set in
  let show_tag set = "Tag " ^ Set.to_string set in
  if Set.is_empty int && Set.is_empty tag then "false"
  else if Set.is_full int && Set.is_full tag then "true"
  else if Set.is_empty int then show_tag tag
  else if Set.is_empty tag then show_int int
  else Printf.sprintf "%s ∨ %s" (show_int int) (show_tag tag)

let bprint buf domain =
  Printf.bprintf buf "%s" (to_string domain)
