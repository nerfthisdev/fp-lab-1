open Fp_lab_1

let () =
  let n = 20 in
  let a_rec = Task1.answer_rec n in
  let a_tail = Task1.answer_tail n in
  let a_fold = Task1.answer_fold n in
  let a_map = Task1.answer_map_primepowers n in
  let a_for = Task1.answer_for n in
  let a_while = Task1.answer_while n in
  let a_seq = Task1.answer_seq n in

  Printf.printf "Task 1 (Euler #5). n=%d\n" n;
  Printf.printf "  rec              : %d\n" a_rec;
  Printf.printf "  tail-rec         : %d\n" a_tail;
  Printf.printf "  filter+fold      : %d\n" a_fold;
  Printf.printf "  map(prime-powers): %d\n" a_map;
  Printf.printf "  for-loop         : %d\n" a_for;
  Printf.printf "  while-loop       : %d\n" a_while;
  Printf.printf "  Seq (lazy)       : %d\n" a_seq

let () =
  let n_max = 1000 in
  let a_rec = Task2.cycle_len_rec 7 in
  let a_tail7 = Task2.cycle_len_tail 7 in
  let d_fold = Task2.answer_fold n_max in
  let d_map = Task2.answer_map n_max in
  let d_for = Task2.answer_for n_max in
  let d_while = Task2.answer_while n_max in
  let d_seq = Task2.answer_seq n_max in

  Printf.printf "Task 2 (Euler #26). n_max=%d\n" n_max;
  Printf.printf "  cycle_len_rec(7)  = %d\n" a_rec;
  Printf.printf "  cycle_len_tail(7) = %d\n" a_tail7;
  Printf.printf "  answer_fold       = %d\n" d_fold;
  Printf.printf "  answer_map        = %d\n" d_map;
  Printf.printf "  answer_for        = %d\n" d_for;
  Printf.printf "  answer_while      = %d\n" d_while;
  Printf.printf "  answer_seq        = %d\n" d_seq
