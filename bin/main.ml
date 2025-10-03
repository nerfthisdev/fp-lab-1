open Fp_lab_1

let () =
  let n = 20 in
  let a1 = Task1.answer_rec n in
  let a2 = Task1.answer_tail n in
  Printf.printf "Task 1 (Euler #5). n=%d\n" n;
  Printf.printf "  rec : %d\n" a1;
  Printf.printf "  tail: %d\n" a2;
  if a1 = a2 then Printf.printf "  OK: answers match.\n"
  else Printf.printf "  MISMATCH!\n"
