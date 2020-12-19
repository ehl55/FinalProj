(** The abstract type representing a single enemy. *)
type t 

(** [init_enemy h w r c] initializes an enemy at row [r] and col [c] in a 
    board with height [h] and width [w]. *)
val init_enemy : int -> int -> int -> int -> t

(** [get_pos e] returns the current row of the enemy. *)
val get_pos_row : t -> int

(** [get_pos e] returns the current col of the enemy. *)
val get_pos_col : t -> int

(** [move_left e] moves the enemy object left by 1 unit, 
    unless the enemy is at the left boundary. *)
val move_left : t -> unit

(** [move_right e] moves the enemy object right by 1 unit, 
    unless the enemy is at the right boundary. *)
val move_right : t -> unit

(** [move_down e] moves the enemy object down by 1 unit, 
    unless the enemy is at the bottom boundary. *)
val move_down : t -> unit

(** [move_up e] moves the enemy object up by 1 unit, 
    unless the enemy is at the upper boundary. *)
val move_up : t -> unit

(** [update_counter e dir_right] decrements an enemy's counter by 1. 
    Once this counter reaches 0, the corresponding movement function is called 
    based on the value of [dir_right]. At this point, the counter is also reset
    to a positive integer. *)
val update_counter : t -> bool -> unit

