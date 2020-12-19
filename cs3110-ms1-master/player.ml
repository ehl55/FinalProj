type t = {
  mutable pos : int;
  board_width : int;
  ship_width : int;
  ship_string : string;
}

let init_player w = {
  pos = w / 2;
  board_width = w;
  ship_width = 5;
  ship_string = "|_^_|";
}

let get_pos pl = 
  pl.pos

let move_left pl = 
  pl.pos <- begin
    if pl.pos = 0 then 0 
    else pl.pos - 1
  end

let move_right pl = 
  pl.pos <- begin
    if pl.pos + pl.ship_width >= pl.board_width then pl.pos
    else pl.pos + 1
  end

let get_player_string pl = 
  pl.ship_string

let get_player_width pl = 
  pl.ship_width