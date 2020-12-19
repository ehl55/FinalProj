type t = {
  board_width : int;
  board_height : int;
  mutable pos_row : int;
  mutable pos_col : int;
  mutable counter : int;
}

let interval = 50

let init_enemy h w r c = {
  board_height = h;
  board_width = w;
  pos_row = r;
  pos_col = c;
  counter = interval;
}

let get_pos_row en = 
  en.pos_row

let get_pos_col en = 
  en.pos_col

let move_left en = 
  en.pos_col <- 
    if en.pos_col <> 0 then en.pos_col - 1
    else 0

let move_right en = 
  en.pos_col <- 
    if en.pos_col <> en.board_width - 1 then en.pos_col + 1
    else en.board_width - 1

let move_down en = 
  en.pos_row <- 
    if en.pos_row <> en.board_height - 1 then en.pos_row + 1
    else en.board_height - 1

let move_up en = 
  en.pos_row <- 
    if en.pos_row <> 0 then en.pos_row - 1
    else 0

let update_counter en dir_right = 
  if en.counter - 1 = 0 then begin
    en.counter <- interval;
    if dir_right then move_right en
    else move_left en
  end
  else en.counter <- en.counter - 1

