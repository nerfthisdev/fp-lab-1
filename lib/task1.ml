(* Euler #5: LCM of 1..n *)
let rec gcd a b = if b = 0 then abs a else gcd b (a mod b)
let lcm a b = a / gcd a b * b
let ints_1_to n = if n <= 0 then [] else List.init n (fun i -> i + 1)

(* 1. simple recursion *)
let rec lcm_list_rec = function
  | [] -> 1
  | [ x ] -> x
  | x :: y :: rest ->
      let l = lcm x y in
      lcm_list_rec (l :: rest)

let answer_rec n = lcm_list_rec (ints_1_to n)

(* 2. tail recursion *)
let lcm_list_tail nums =
  let rec loop acc = function [] -> acc | x :: xs -> loop (lcm acc x) xs in
  match nums with [] -> 1 | x :: xs -> loop x xs

let answer_tail n = lcm_list_tail (ints_1_to n)

(* 3. modular: generate → filter → fold *)
let answer_fold n =
  let gen = ints_1_to n in
  let filtered = List.filter (fun x -> x > 1) gen in
  List.fold_left lcm 1 filtered

(* 4. using map + prime powers *)
let primes_up_to n =
  if n < 2 then []
  else
    let a = Array.make (n + 1) true in
    a.(0) <- false;
    a.(1) <- false;
    let limit = int_of_float (sqrt (float_of_int n)) in
    for p = 2 to limit do
      if a.(p) then
        let rec strike m =
          if m <= n then (
            a.(m) <- false;
            strike (m + p))
        in
        strike (p * p)
    done;
    let acc = ref [] in
    for i = n downto 2 do
      if a.(i) then acc := i :: !acc
    done;
    !acc

let max_power_leq n p =
  let rec loop acc = if acc * p <= n then loop (acc * p) else acc in
  loop p

let answer_map_primepowers n =
  primes_up_to n |> List.map (max_power_leq n) |> List.fold_left ( * ) 1

(* 5. for-loop *)
let answer_for n =
  let acc = ref 1 in
  for i = 2 to n do
    acc := lcm !acc i
  done;
  !acc

(* 6. while-loop *)
let answer_while n =
  let acc = ref 1 in
  let i = ref 2 in
  while !i <= n do
    acc := lcm !acc !i;
    incr i
  done;
  !acc

(* 7. lazy seq *)
let rec seq_from i () = Seq.Cons (i, seq_from (i + 1))

let answer_seq n =
  seq_from 1 |> Seq.take_while (fun x -> x <= n) |> Seq.fold_left lcm 1
