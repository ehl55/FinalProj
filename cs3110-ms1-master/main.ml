let rec process_input gs = 
  let input = input_char stdin in 

  match input with
  | 'D' -> begin
      Gamestate.move_player_left gs;
      process_input gs
    end
  | 'C' -> begin
      Gamestate.move_player_right gs;
      process_input gs
    end
  | ' ' -> begin
      Gamestate.fire_bullet gs;
      process_input gs
    end
  | _ -> begin 
      process_input gs (*Stationary*)
    end

(* Permitted by Professor Clarkson to use from StackOverflow *)
let auto_process_input gs = 
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false } in
  let res = process_input gs in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res

let rec process_game_state gs = 
  Gamestate.print_game gs;
  Unix.sleepf 0.01; (*sec delay between frames*)
  Gamestate.e_fire_bullet gs;
  Gamestate.update_counters gs;
  Gamestate.process_interaction gs;
  process_game_state gs

let welcome_message = 
"
 ========================================
||                                      ||
||                                      ||
||                                      ||
||                                      ||
||                                      ||
||                                      || 
||                                      ||
||      Welcome to Space Invaders!      || 
||   Please press e to start the game   ||
||     Press any other key to exit      ||
||                                      ||
||                                      ||
||                                      ||
||                                      ||
||                                      ||     
||                                      ||                                     
 ======================================== 
"

let get_start_char () =
    let termio = Unix.tcgetattr Unix.stdin in
    let () =
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
            { termio with Unix.c_icanon = false } in
    let res = input_char stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    res
    
let start_input = 
  print_string welcome_message;
  flush stdout;
  let input = get_start_char () in
  match input with 
  | 'e' -> true 
  |'E' -> true
  | _ -> false

let start_game input = 
  while input do 
    let h = 20 in
    let w = 40 in
    let gs = Gamestate.init_state h w in
    let main_thread = Thread.create process_game_state gs in
    let user_input_thread = Thread.create auto_process_input gs in 
    Thread.join main_thread;
    Thread.join user_input_thread
  done

let main () = 
  start_game start_input

let _ = main ()