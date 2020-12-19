(** The abstract type representing a bullet. *)
type t 

(** [interval] is the number of frames that must pass before [move_up]
    is called on the bullet. *)
val interval : int

(** [init_bullet h r c] initializes an bullet at specified row [r] and 
    column [c] on a board of height [h]. *)
val init_bullet : int -> int -> int -> t

(** [get_row b] returns the current row of the bullet. *)
val get_row : t -> int

(** [get_col b] returns the current col of the bullet. *)
val get_col : t -> int

(** [remove b] deletes the bullet [b] from the board by moving it to (-1,-1) *)
val remove : t -> unit

(** [move_up b] moves the bullet object up by 1 unit, unless
    it reaches the top of the screen. If it reaches the top, then the
    bullet is deleted.*)
val move_up : t -> unit

(** [update_counter b] decrements a bullet's counter by 1. Once this counter
    reaches 0, the [move_up b] is called and the counter is reset to a
    positive integer. *)
val update_counter : t -> unit

(** [move_down b]  moves the bullet object down by 1 unit, unless
    it reaches the bottom of the screen. If it reaches the bottom, then the
    bullet is deleted. *)
val move_down : t -> unit

(** [update_e_blt_counter b] decrements an enemy bullet's counter by 1. 
    Once this counter reaches 0, the [move_down b] is called and the counter 
    is reset to a positive integer. *)
val update_e_blt_counter : t -> unit