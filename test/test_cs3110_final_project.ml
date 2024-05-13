open OUnit2
open Chess.Board

let test_initial_board _ =
  let initial_board = initialize_board () in
  assert_equal 32
    (List.length initial_board)
    ~msg:"Initial board must contain 32 pieces"

let suite = "Chess Tests" >::: [ "test_initial_board" >:: test_initial_board ]
let () = run_test_tt_main suite
