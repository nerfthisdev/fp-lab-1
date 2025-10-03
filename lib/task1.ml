(* Euler #5: LCM of 1..n *)

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

(* regular rec *)
let rec lcm_list_rec = function
  | [] -> 1
  | [ x ] -> x
  | x :: y :: rest ->
      let lcm_xy = x * y / gcd x y in
      lcm_list_rec (lcm_xy :: rest)

(* tail rec *)
let lcm_list_tail nums =
  let rec loop acc = function
    | [] -> acc
    | x :: xs -> loop (acc * x / gcd acc x) xs
  in
  match nums with [] -> 1 | x :: xs -> loop x xs

let ints_1_to n = List.init n (fun i -> i + 1)
let answer_rec n = lcm_list_rec (ints_1_to n)
let answer_tail n = lcm_list_tail (ints_1_to n)
