open Fp_lab_1


let task1_n = 20
let task1_answer = 232792560

let test_task1_answer_rec () =
  Alcotest.(check int) "rec" task1_answer (Task1.answer_rec task1_n)

let test_task1_answer_tail () =
  Alcotest.(check int) "tail" task1_answer (Task1.answer_tail task1_n)

let test_task1_answer_fold () =
  Alcotest.(check int) "fold" task1_answer (Task1.answer_fold task1_n)

let test_task1_answer_map () =
  Alcotest.(check int) "map" task1_answer (Task1.answer_map_primepowers task1_n)

let test_task1_answer_for () =
  Alcotest.(check int) "for" task1_answer (Task1.answer_for task1_n)

let test_task1_answer_while () =
  Alcotest.(check int) "while" task1_answer (Task1.answer_while task1_n)

let test_task1_answer_seq () =
  Alcotest.(check int) "seq" task1_answer (Task1.answer_seq task1_n)


let task2_n_max = 1000
let task2_expected_d = 983
let task2_expected_len_7 = 6

let test_task2_cycle_len_rec () =
  Alcotest.(check int) "cycle_len_rec(7)" task2_expected_len_7
    (Task2.cycle_len_rec 7)

let test_task2_cycle_len_tail () =
  Alcotest.(check int) "cycle_len_tail(7)" task2_expected_len_7
    (Task2.cycle_len_tail 7)

let test_task2_answer_fold () =
  Alcotest.(check int) "fold" task2_expected_d
    (Task2.answer_fold task2_n_max)

let test_task2_answer_map () =
  Alcotest.(check int) "map" task2_expected_d
    (Task2.answer_map task2_n_max)

let test_task2_answer_for () =
  Alcotest.(check int) "for" task2_expected_d
    (Task2.answer_for task2_n_max)

let test_task2_answer_while () =
  Alcotest.(check int) "while" task2_expected_d
    (Task2.answer_while task2_n_max)

let test_task2_answer_seq () =
  Alcotest.(check int) "seq" task2_expected_d
    (Task2.answer_seq task2_n_max)

let () =
  let open Alcotest in
  run "Euler Labs" [
    (* Task 1 suite *)
    ( "Task1 – Euler #5",
      [
        test_case "rec" `Quick test_task1_answer_rec;
        test_case "tail" `Quick test_task1_answer_tail;
        test_case "fold" `Quick test_task1_answer_fold;
        test_case "map" `Quick test_task1_answer_map;
        test_case "for" `Quick test_task1_answer_for;
        test_case "while" `Quick test_task1_answer_while;
        test_case "seq" `Quick test_task1_answer_seq;
      ] );
    (* Task 2 suite *)
    ( "Task2 – Euler #26",
      [
        test_case "cycle_len_rec(7)" `Quick test_task2_cycle_len_rec;
        test_case "cycle_len_tail(7)" `Quick test_task2_cycle_len_tail;
        test_case "fold" `Slow test_task2_answer_fold;
        test_case "map" `Slow test_task2_answer_map;
        test_case "for" `Slow test_task2_answer_for;
        test_case "while" `Slow test_task2_answer_while;
        test_case "seq" `Slow test_task2_answer_seq;
      ] );
  ]

