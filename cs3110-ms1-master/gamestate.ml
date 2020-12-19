type horz_dir = Left | Right

type t = {
  board_height : int;
  board_width : int;
  player : Player.t;
  bullet_reload_frames : int;
  e_bullet_reload_frames : int;
  mutable bullet_lst : Bullet.t list;
  mutable e_bullet_lst : Bullet.t list;
  mutable enemy_lst : Enemy.t list;
  mutable bullet_cooldown : int;
  mutable e_bullet_cooldown : int;
  mutable can_fire : bool;
  mutable e_can_fire : bool;
  mutable enemy_dir : horz_dir;
  mutable level : int;
  mutable lives : int
}

let gen_enemy_row stagger curr_row horz_space res h w = 
  let rec gen_enemy_row_aux curr_col res = 
    if curr_col >= w - 2 then res (* no enemy spawn @ last 2 col *)
    else gen_enemy_row_aux (curr_col + horz_space) 
        (Enemy.init_enemy h w curr_row curr_col :: res) in

  if stagger then gen_enemy_row_aux 2 res (* no spawn @ first 2 col *)
  else gen_enemy_row_aux (horz_space / 2 + 2) res

let gen_enemies lvl h w  = 
  let vert_space = h / 3 - lvl in (* /3 b/c don't want excess space on lvl 0 *)
  let horz_space = w / 3 - lvl in

  let rec gen_enemies_aux stagger curr_row res = 
    if curr_row >= h - 4 then res (* -4 to leave space b/t enemy + player *)
    else gen_enemies_aux (not stagger) (curr_row + vert_space) 
        (gen_enemy_row stagger curr_row horz_space res h w) in

  gen_enemies_aux true 0 []

let init_state h w = {
  board_height = h;
  board_width = w;
  player = Player.init_player w;
  bullet_reload_frames = 50;
  e_bullet_reload_frames = 200;
  bullet_lst = [];
  e_bullet_lst = [];
  level = 0;
  enemy_lst = gen_enemies 0 h w;
  bullet_cooldown = 50;
  e_bullet_cooldown = 200;
  can_fire = true;
  e_can_fire = true;
  enemy_dir = Right;
  lives = 3;
}

let get_board_height gs =
  gs.board_height

let get_board_width gs = 
  gs.board_width

let move_player_left gs = 
  Player.move_left gs.player

let move_player_right gs = 
  Player.move_right gs.player

let enemies_on_row r gs = 
  List.filter (fun e -> Enemy.get_pos_row e = r) gs.enemy_lst

let bullets_on_row r gs = 
  List.filter (fun b -> Bullet.get_row b = r) gs.bullet_lst

let e_bullets_on_row r gs = 
  List.filter (fun b -> Bullet.get_row b = r) gs.e_bullet_lst

let shared_cols e_col b_col = 
  List.filter (fun e -> List.mem e b_col) e_col

let shared_enemy_player_blt_cols b_col e_b_col = 
  List.filter (fun b -> List.mem b e_b_col) b_col

let remove_enemy r c gs = 
  gs.enemy_lst <- List.filter (
      fun e -> not(Enemy.get_pos_row e = r && 
                   Enemy.get_pos_col e = c)
    ) gs.enemy_lst        

let remove_bullet r c gs = 
  gs.bullet_lst <- List.filter (
      fun b -> not(Bullet.get_row b = r &&
                   Bullet.get_col b = c)
    ) gs.bullet_lst

let remove_e_bullet r c gs = 
  gs.e_bullet_lst <- List.filter (
      fun b -> not(Bullet.get_row b = r &&
                   Bullet.get_col b = c)
    ) gs.e_bullet_lst

let lose_life gs = 
  if gs.lives = 1 then exit 0
  else gs.lives <- gs.lives - 1

let maybe_lose_life gs col = 
  let player_pos = Player.get_pos gs.player in
  if col >= player_pos && 
     col < player_pos + Player.get_player_width gs.player then begin
    lose_life gs;
    remove_e_bullet (gs.board_height - 1) col gs
  end
  else () (* don't remove bullet yet, allow print to run first *)

let e_blt_last_row gs = List.filter (
    fun e_b -> Bullet.get_row e_b = gs.board_height - 1
  ) gs.e_bullet_lst

let check_and_remove_e_bullets gs = 
  remove_e_bullet ~-1 ~-1 gs; (* remove shadow realm bullets *)

  (* process bullets that collided with player. *)
  let rec process_e_blts = function
    | h :: t -> maybe_lose_life gs (Bullet.get_col h)
    | _ -> () in

  process_e_blts (e_blt_last_row gs)

let gen_next_level gs = 
  let h = get_board_height gs in
  let w = get_board_width gs in

  if gs.level = 11 then () (* 10 levels *)
  else begin
    gs.level <- gs.level + 1;
    gs.enemy_lst <- gen_enemies (gs.level + 1) h w
  end

let process_interaction gs = 
  let remove_bullet_and_enemies r col_lst = 
    match col_lst with
    | [] -> ()
    | h::t -> remove_enemy r h gs; remove_bullet r h gs; 
      remove_e_bullet r h gs in

  for i = 0 to gs.board_height - 1 do 
    let remove_player_enemy_blt_cols = shared_enemy_player_blt_cols
        (List.map(fun b -> Bullet.get_col b) (bullets_on_row i gs))
        (List.map(fun e_b -> Bullet.get_col e_b) (e_bullets_on_row i gs)) in
    remove_bullet_and_enemies i remove_player_enemy_blt_cols;
    let remove_cols = shared_cols 
        (List.map(fun e -> Enemy.get_pos_col e) (enemies_on_row i gs)) 
        (List.map(fun b -> Bullet.get_col b) (bullets_on_row i gs)) in
    remove_bullet_and_enemies i remove_cols
  done;

  (* Clear any bullets in the shadow realm. *)
  remove_bullet ~-1 ~-1 gs;
  check_and_remove_e_bullets gs;

  (* Move to next level if no enemies. *)
  if List.length gs.enemy_lst = 0 then gen_next_level gs
  else ()

let fire_bullet gs =
  if (gs.can_fire = true) then begin
    gs.can_fire <- false;
    gs.bullet_lst <- 
      Bullet.init_bullet gs.board_height (gs.board_height - 1)
        (Player.get_pos gs.player + Player.get_player_width gs.player / 2) 
      :: gs.bullet_lst; 
  end
  else ()

let find_rand_enemy_last_row gs = 
  let rand_enemy_idx = Random.int (List.length gs.enemy_lst) in
  let rand_enemy = List.nth gs.enemy_lst rand_enemy_idx in
  let rand_enemy_col = Enemy.get_pos_col rand_enemy in
  let enemies_on_col = List.filter (fun e -> 
      Enemy.get_pos_col e = rand_enemy_col) gs.enemy_lst in
  let cmp e1 e2 = Enemy.get_pos_row e2 - Enemy.get_pos_col e1 in
  let enemies_on_col_sort = List.sort cmp enemies_on_col in
  List.hd enemies_on_col_sort

let e_fire_bullet gs = 
  if (gs.e_can_fire = true && List.length gs.enemy_lst > 0) then begin
    let e = find_rand_enemy_last_row gs in
    gs.e_can_fire <- false;
    gs.e_bullet_lst <-
      Bullet.init_bullet gs.board_height (Enemy.get_pos_row e + 1)
        (Enemy.get_pos_col e) :: gs.e_bullet_lst;
  end
  else ()

let update_bullet_cooldown gs =
  if (gs.bullet_cooldown - 1 = 0) then begin
    gs.bullet_cooldown <- gs.bullet_reload_frames;
    gs.can_fire <- true 
  end
  else gs.bullet_cooldown <- gs.bullet_cooldown - 1

let update_e_bullet_cooldown gs = 
  if (gs.e_bullet_cooldown - 1 = 0) then begin
    gs.e_bullet_cooldown <- gs.e_bullet_reload_frames;
    gs.e_can_fire <- true  
  end
  else gs.e_bullet_cooldown <- gs.e_bullet_cooldown - 1

let calc_enemy_horz_dir gs = 
  let enemy_lst_right_b = 
    List.filter (fun e -> Enemy.get_pos_col e = gs.board_width - 1) 
      gs.enemy_lst in
  let num_enemy_right_b = 
    List.length enemy_lst_right_b in
  let enemy_lst_left_b = 
    List.filter (fun e -> Enemy.get_pos_col e = 0) 
      gs.enemy_lst in
  let num_enemy_left_b = 
    List.length enemy_lst_left_b in

  if(gs.enemy_dir = Right && num_enemy_right_b > 0) then begin
    List.iter Enemy.move_down gs.enemy_lst; 
    Left end
  else if (gs.enemy_dir = Left && num_enemy_left_b > 0) then begin
    List.iter Enemy.move_down gs.enemy_lst;
    Right end
  else gs.enemy_dir


let update_counters gs = 
  update_bullet_cooldown gs;
  update_e_bullet_cooldown gs;
  let _ = List.map(fun b -> Bullet.update_counter b; b) 
      gs.bullet_lst in
  let _ = List.map(fun b -> Bullet.update_e_blt_counter b; b)
      gs.e_bullet_lst in
  gs.enemy_dir <- calc_enemy_horz_dir gs;
  let _  = List.map (fun e -> Enemy.update_counter e (gs.enemy_dir = Right); e) 
      gs.enemy_lst in ()

(* Permitted by Professor Clarkson to use from source: 
     http://pleac.sourceforge.net/pleac_ocaml/userinterfaces.html*)
let clear =
  try
    let proc = Unix.open_process_in "clear" in
    try
      let chars = input_line proc in
      ignore (Unix.close_process_in proc);
      chars
    with e -> ignore (Unix.close_process_in proc); ""
  with _ -> ""

let print_top_horz_border w = 
  print_string " ";
  for i = 0 to w - 1 do
    print_string "_"
  done;
  print_string"\n"

let print_bottom_horz_border w = 
  print_string "|";
  for i = 0 to w - 1 do
    print_string "_"
  done;
  print_string "|";
  print_string"\n"

let rec print_spaces n = 
  if n = 0 then ()
  else begin 
    print_string " ";
    print_spaces (n - 1)
  end

let print_last_row w gs = 
  let e_blt_last_row = e_blt_last_row gs in
  let p_width = Player.get_player_width gs.player in
  let p_string = Player.get_player_string gs.player in
  print_string "|";
  let i = ref 0 in
  while (!i < w) do 
    if !i = Player.get_pos gs.player then begin
      print_string p_string;
      i := !i + p_width - 1;
    end
    else if (List.length e_blt_last_row > 0 && 
             !i = Bullet.get_col (List.hd e_blt_last_row)) then print_string "*"
    else print_string " ";

    i := !i + 1
  done;
  print_string "|\n"

let print_lives gs = 
  print_string ("Lives remaining: " ^ string_of_int gs.lives ^ "\n\n")

let print_bullet_and_enemies curr_row w gs = 
  let enemy_cols = 
    List.map (fun e -> Enemy.get_pos_col e) (enemies_on_row curr_row gs) in
  let bullet_cols = 
    List.map (fun b -> Bullet.get_col b) (bullets_on_row curr_row gs) in
  let e_bullet_cols = 
    List.map (fun b -> Bullet.get_col b) (e_bullets_on_row curr_row gs) in
  print_string "|";
  for i = 0 to w - 1 do
    if List.mem i enemy_cols then print_string "W"
    else if List.mem i bullet_cols then print_string "^"
    else if List.mem i e_bullet_cols then print_string "*"
    else print_string " "
  done;
  print_string "|\n"

let print_game gs = 
  print_string clear;
  print_lives gs;
  for i = ~-1 to gs.board_height do 
    if i = ~-1 then print_top_horz_border gs.board_width
    else if i = gs.board_height then print_bottom_horz_border gs.board_width
    else if i = gs.board_height - 1 then print_last_row gs.board_width gs
    else print_bullet_and_enemies i gs.board_width gs
  done;
  flush stdout
