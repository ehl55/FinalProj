(** The abstract type representing the player's ship. *)
type t 

(** [init_player w] initializes the player ship's left hand side at the 
    center column. *)
val init_player : int -> t

(** [get_pos pl] returns the current col of the player. Note, the coordinate
    represents the left hand side coordinate of the ship. *)
val get_pos : t -> int

(** [move_left pl] moves the player ship's position left by 1, 
    unless the ship is at the left boundary. *)
val move_left : t -> unit

(** [move_right pl] moves the player ship's position right by 1,
    unless the ship is at the right boundary. *)
val move_right : t -> unit

(** [get_player_string pl] returns the string representation of the 
    player's ship. *)
val get_player_string : t -> string

(** [get_player_width pl] returns the width in chars of the player's ship. *)
val get_player_width : t -> int