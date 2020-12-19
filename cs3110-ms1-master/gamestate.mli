(** The abstract type of values representing the game state. *)
type t 

(** [init_state h w] is the initial state of the game. It should
    have the player's ship located in the center of the board. The board
    dimensions will be h x w. *)
val init_state : int -> int -> t

(** [get_board_height gs] returns the board height. *)
val get_board_height : t -> int

(** [get_board_width gs] returns the board width. *)
val get_board_width : t -> int

(** [move_player_left gs] moves the player to the left by a unit, unless
    the player is at the left boundary. *)
val move_player_left : t -> unit

(** [move_player_right gs] moves the player to the right by a unit, unless
    the player is at the right boundary. *)
val move_player_right : t -> unit

(** [update_counters gs] updates bullet and enemy counters, and moves said 
    object if the corresponding counter reaches 0, at which point the counter 
    is reset. *)
val update_counters : t -> unit

(** [process_bullet_interaction gs] checks for and processes a bullet 
    interaction with a player or enemy object. *)
val process_interaction : t -> unit 

(** [fire_bullet gs] fires a bullet from the center of the player's ship. *)
val fire_bullet : t -> unit

(** [e_fire_bullet gs] finds a random column with enemies, and fires a bullet
    from the enemy closest to the player. *)
val e_fire_bullet : t -> unit

(** [print_game gs] prints the gamestate [gs]. *)
val print_game : t -> unit