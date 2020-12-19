open OUnit2
open Level 
open Gamestate 
open Bullet

let get_board_height_test 
    (name : string) 
    (input : Gamestate.t) 
    (expected : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected (Gamestate.get_board_height input) 
        ~printer: string_of_int)

let get_board_width_test
    (name : string)
    (input : Gamestate.t)
    (expected : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected (Gamestate.get_board_width input) 
        ~printer: string_of_int)

let initial_state = Gamestate.init_state 20 30
let initial_state_2 = Gamestate.init_state 20 30
let left_state = Gamestate.init_state 20 30 |> Gamestate.move_player_left
let right_state = Gamestate.init_state 20 30 |> Gamestate.move_player_right
let left_bound = Gamestate.init_state 20 1
let right_bound = Gamestate.init_state 20 1

let left_boundary_state_part_1 = 
  left_bound |> Gamestate.move_player_left
let left_boundary_state_part_2 = 
  left_bound |> Gamestate.move_player_left

let right_boundary_state_part_1 = 
  right_bound |> Gamestate.move_player_right 
let right_boundary_state_part_2 = 
  right_bound |> Gamestate.move_player_right

let stay_in_place_part_1 = 
  initial_state_2 |> Gamestate.move_player_right 
let stay_in_place_part_2 = 
  initial_state_2 |> Gamestate.move_player_left

let state_tests = 
  [
    get_board_height_test "check 20 x 30 board's height" initial_state 20;
    get_board_width_test "check 20 x 30 board's width" initial_state 30; 
  ]

let enemy_A0 = Enemy.init_enemy 20 30 1 3
let enemy_A1 = Enemy.init_enemy 20 30 1 3
let enemy_A2 = Enemy.init_enemy 20 30 1 3
let enemy_A3 = Enemy.init_enemy 20 30 1 3
let enemy_A4 = Enemy.init_enemy 20 30 1 3
let enemy_B = Enemy.init_enemy 20 30 4 5

let enemy_A_up =  
  enemy_A1 |> Enemy.move_up; 
  enemy_A1
let enemy_A_up2 = 
  enemy_A2 |> Enemy.move_up; 
  enemy_A2 |> Enemy.move_up; 
  enemy_A2

let enemy_A_down = 
  enemy_A3 |> Enemy.move_down; 
  enemy_A3
let enemy_A_down2 = 
  enemy_A4 |> Enemy.move_down; 
  enemy_A4 |> Enemy.move_down;
  enemy_A4

let enemy_A_left = 
  enemy_A1 |> Enemy.move_left
let enemy_A_left4 = 
  enemy_A4 |> Enemy.move_left; 
  enemy_A4 |> Enemy.move_left; 
  enemy_A4 |> Enemy.move_left; 
  enemy_A4 |> Enemy.move_left

let enemy_A_right = 
  enemy_A2 |> Enemy.move_right
let enemy_A_right2 = 
  enemy_A3 |> Enemy.move_right; 
  enemy_A3 |> Enemy.move_right

let get_enemy_pos_row_test
    (name : string)
    (en : Enemy.t)
    (e : int) : test =  
  name >:: (fun _ -> 
      assert_equal e (Enemy.get_pos_row en) 
        ~printer: string_of_int)

let get_enemy_pos_col_test
    (name : string)
    (en : Enemy.t)
    (e : int) : test =  
  name >:: (fun _ -> 
      assert_equal e (Enemy.get_pos_col en) 
        ~printer: string_of_int)

let enemy_pos_tests = [
  get_enemy_pos_row_test "testing row of enemy_A" enemy_A0 1;
  get_enemy_pos_row_test "testing row of enemy_B" enemy_B 4;
  get_enemy_pos_row_test "testing one row up en A" enemy_A1 0;
  get_enemy_pos_row_test "test two row up en A" enemy_A2 0;
  get_enemy_pos_row_test "test one row down en A" enemy_A3 2;
  get_enemy_pos_row_test "test two row down en A" enemy_A4 3;

  get_enemy_pos_col_test "test col enemy A" enemy_A0 3;
  get_enemy_pos_col_test "test col enemy B" enemy_B 5;
  get_enemy_pos_col_test "test one left enemy A" enemy_A1 2;
  get_enemy_pos_col_test "test one right en A" enemy_A2 4;
  get_enemy_pos_col_test "test two right en A" enemy_A3 5;
  get_enemy_pos_col_test "test four left en A" enemy_A4 0;
]

let get_bullet_pos_row_test 
    (name : string)
    (bullet : Bullet.t)
    (x : int) : test = 
  name >:: (fun _ ->
      assert_equal x (Bullet.get_row bullet)
        ~printer: string_of_int)

let get_bullet_pos_col_test
    (name : string)
    (bullet : Bullet.t)
    (y : int) : test = 
  name >:: (fun _ ->
      assert_equal y (Bullet.get_col bullet)
        ~printer: string_of_int)

let bullet_init_1 = Bullet.init_bullet 20 19 30
let bullet_init_2 = Bullet.init_bullet 20 19 30
let bullet_init_3 = Bullet.init_bullet 20 19 30
let bullet_init_4 = Bullet.init_bullet 20 19 30
let bullet_init_5 = Bullet.init_bullet 20 19 30

let remove_init = Bullet.remove bullet_init_2

let move_up_1 = 
  bullet_init_3 |> Bullet.move_up;
  bullet_init_3

let count_1 = Bullet.update_counter bullet_init_1

let move_up_5 = 
  bullet_init_4 |> Bullet.move_up; 
  bullet_init_4 |> Bullet.move_up; 
  bullet_init_4 |> Bullet.move_up; 
  bullet_init_4 |> Bullet.move_up; 
  bullet_init_4 |> Bullet.move_up;
  bullet_init_4

let move_up_10 = 
  bullet_init_5 |> Bullet.move_up; 
  bullet_init_5 |> Bullet.move_up; 
  bullet_init_5 |> Bullet.move_up; 
  bullet_init_5 |> Bullet.move_up; 
  bullet_init_5 |> Bullet.move_up; 
  bullet_init_5 |> Bullet.move_up; 
  bullet_init_5 |> Bullet.move_up; 
  bullet_init_5 |> Bullet.move_up;
  bullet_init_5 |> Bullet.move_up;
  bullet_init_5 |> Bullet.move_up; 
  bullet_init_5

let bullet_counter = 10

let bullet_tests = [
  get_bullet_pos_row_test 
    "initial row pos" 
    bullet_init_1 19;

  get_bullet_pos_col_test 
    "initial col pos" 
    bullet_init_1 30;

  get_bullet_pos_row_test 
    "removed initial row pos" 
    bullet_init_2 ~-1;

  get_bullet_pos_col_test 
    "removed initial col pos" 
    bullet_init_2 ~-1;

  get_bullet_pos_row_test 
    "initial row pos move up 1" 
    move_up_1 18;

  get_bullet_pos_col_test 
    "initial col pos move up 1" 
    move_up_1 30;

  get_bullet_pos_row_test 
    "initial row pos move up 5" 
    move_up_5 14;

  get_bullet_pos_col_test 
    "initial col pos move up 5" 
    move_up_5 30;

  get_bullet_pos_row_test 
    "initial row pos move up 10" 
    move_up_10 9;

  get_bullet_pos_col_test 
    "initial col pos move up 10" 
    move_up_10 30;
]

let player_pos_test
    (name : string)
    (input : Player.t)
    (expected : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected (Player.get_pos input) 
        ~printer: string_of_int)

let p_move_l_4 = Player.init_player 30
let player_move_left_4 = 
  p_move_l_4 |> Player.move_left;
  p_move_l_4 |> Player.move_left;
  p_move_l_4 |> Player.move_left;
  p_move_l_4 |> Player.move_left;
  p_move_l_4

let p_move_r_3 = Player.init_player 30
let player_move_right_3 = 
  p_move_r_3 |> Player.move_right;
  p_move_r_3 |> Player.move_right;
  p_move_r_3 |> Player.move_right;
  p_move_r_3

let p_move_l_3_r_2 = Player.init_player 30
let player_move_left_3_right_2 =
  p_move_l_3_r_2 |> Player.move_left;
  p_move_l_3_r_2 |> Player.move_left;
  p_move_l_3_r_2 |> Player.move_left;
  p_move_l_3_r_2 |> Player.move_right;
  p_move_l_3_r_2 |> Player.move_right;
  p_move_l_3_r_2

let p_move_r_b = Player.init_player 10
let player_move_right_bound = 
  p_move_r_b |> Player.move_right;
  p_move_r_b

let p_move_l_b = Player.init_player 10
let player_move_left_bound = 
  p_move_l_b |> Player.move_left;
  p_move_l_b |> Player.move_left;
  p_move_l_b |> Player.move_left;
  p_move_l_b |> Player.move_left;
  p_move_l_b |> Player.move_left;
  p_move_l_b |> Player.move_left;
  p_move_l_b

let player_pos_tests = [
  player_pos_test
    "player initialized on width 30 board, move left 4 times to col 11."
    p_move_l_4 11;
  player_pos_test
    "player initialized on width 30 board, move right 3 times to col 18."
    p_move_r_3 18;
  player_pos_test
    "player initialized on width 30 board, move left 3 times, move right \
     2 times to col 14."
    p_move_l_3_r_2 14;
  player_pos_test
    "player initialized on width 10 board, try moving beyond right bound, but \
     stuck at right bound of 5 because ship is length 5."
    p_move_r_b 5;
  player_pos_test
    "player initialized on width 10 board, try moving 6 times left, but \
     stuck at left bound of 0."
    p_move_l_b 0;
]

let suite = 
  "test suite for space invaders" >::: List.flatten [
    state_tests;
    enemy_pos_tests;
    bullet_tests;
    player_pos_tests
  ]

let _ = run_test_tt_main suite