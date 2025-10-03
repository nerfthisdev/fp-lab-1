let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let rec lcm_list_rec = function
  | [] -> 1
  | [ x ] -> x
  | x :: y :: rest -> lcm_list_rec ((x * y / gcd x y) :: rest)

(* Способ 2: Хвостовая рекурсия для НОК списка чисел *)
let lcm_list_tail numbers =
  let rec helper acc = function
    | [] -> acc
    | x :: rest -> helper (acc * x / gcd acc x) rest
  in
  match numbers with [] -> 1 | x :: rest -> helper x rest

(* Создаём список чисел от 1 до 20 *)
let numbers = List.init 20 (fun x -> x + 1)

(* Запускаем оба способа *)
let () =
  let result1 = lcm_list_rec numbers in
  let result2 = lcm_list_tail numbers in
  Printf.printf "Обычная рекурсия: %d\n" result1;
  Printf.printf "Хвостовая рекурсия: %d\n" result2
