(** The engine of this game

    Initializes graphics, gain inputs from user, render new graphics
    based on new state, and repeat. *)

(* type representing the directions the player can move *)
type direction =
  | Right
  | Left
  | Up
  | Down

(* Renders the scene using current game state *)

(* The input by the user as an direction *)
