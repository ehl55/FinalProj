type t = {
  board_height : int;
  mutable location: (int * int);
  mutable counter: int
}

let interval = 15

let init_bullet h r c = {
  board_height = h;
  location = (r, c); 
  counter = interval
}

let get_row b = 
  fst b.location

let get_col b = 
  snd b.location

let remove b =
  b.location <- (-1, -1)

let move_up b =
  if fst b.location = 0 then remove b 
  else b.location <- (fst b.location - 1, snd b.location)

let update_counter b =
  if b.counter - 1 = 0 then begin 
    b.counter <- interval;
    move_up b
  end
  else b.counter <- b.counter - 1

let move_down b = 
  if fst b.location = b.board_height (* NOT -1. *) then remove b 
  else b.location <- (fst b.location + 1, snd b.location)

let update_e_blt_counter b = 
  if b.counter - 1 = 0 then begin
    b.counter <- interval;
    move_down b
  end
  else b.counter <- b.counter - 1