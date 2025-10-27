(* Project Euler #26: Reciprocal Cycles *)

let strip_2_5 n =
  (* remove factors 2 and 5 *)
  let rec drop n p = if n mod p = 0 then drop (n / p) p else n in
  drop (drop n 2) 5

(* build remainders until the first repeat (prevents infinite recursion) *)
let build_remainders n rem =
  let rec go rem seen acc =
    if rem = 0 then List.rev acc
    else if List.mem rem seen then List.rev (rem :: acc)  (* stop on repeat *)
    else
      let rem' = (rem * 10) mod n in
      go rem' (rem :: seen) (rem :: acc)
  in
  go rem [] []

let rec find_pos r i = function
  (* first index of r or None *)
  | [] -> None
  | x :: xs -> if x = r then Some i else find_pos r (i + 1) xs

let cycle_len_rec d =
  let n = strip_2_5 d in
  if n = 1 then 0
  else
    let rems = build_remainders n 1 in
    match List.rev rems with
    | [] | [_] -> 0
    | last :: rev_tail ->
        (match find_pos last 0 (List.rev rev_tail) with
         | None -> 0
         | Some j ->
             (* cycle length = (#rems - 1) - first_index(last) *)
             List.length rems - 1 - j)

(* 2. tail recursion with visited map (unchanged) *)
let cycle_len_tail d =
  let n = strip_2_5 d in
  if n = 1 then 0
  else
    let rec loop rem step seen =
      if rem = 0 then 0
      else
        match List.assoc_opt rem seen with
        | Some pos -> step - pos
        | None -> loop ((rem * 10) mod n) (step + 1) ((rem, step) :: seen)
    in
    loop 1 0 []

(* 3. modular pipeline: generate → filter → fold *)
let argmax_by f a b = if f a >= f b then a else b

let answer_fold n_max =
  List.init (n_max - 1) (fun i -> i + 2)  (* 2..n_max *)
  |> List.filter (fun d -> strip_2_5 d <> 1)
  |> List.fold_left (fun best d -> argmax_by cycle_len_tail best d) 2

(* 4. map-based (map d -> (d,len) then fold) *)
let answer_map n_max =
  List.init (n_max - 1) (fun i -> i + 2)
  |> List.map (fun d -> (d, cycle_len_tail d))
  |> List.fold_left
       (fun (bd, bl) (d, l) -> if l > bl then (d, l) else (bd, bl))
       (2, cycle_len_tail 2)
  |> fst

(* 5. for/while (imperative) *)
let answer_for n_max =
  let best_d = ref 2 and best_l = ref (cycle_len_tail 2) in
  for d = 3 to n_max do
    let l = cycle_len_tail d in
    if l > !best_l then (best_l := l; best_d := d)
  done;
  !best_d

let answer_while n_max =
  let d = ref 2 in
  let best_d = ref 2 and best_l = ref (cycle_len_tail 2) in
  while !d <= n_max do
    let l = cycle_len_tail !d in
    if l > !best_l then (best_l := l; best_d := !d);
    incr d
  done;
  !best_d

(* 6. lazy Seq *)
let rec seq_from i () = Seq.Cons (i, seq_from (i + 1))

let answer_seq n_max =
  seq_from 2
  |> Seq.take_while (fun d -> d <= n_max)
  |> Seq.fold_left
       (fun best d ->
         let l_best = cycle_len_tail best in
         let l_d = cycle_len_tail d in
         if l_d > l_best then d else best)
       2

