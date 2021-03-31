(* an abstract type representing n by m texture map *)
type t

(* [from_image file_name] is an image file converted to type t*)
val from_image : string -> t

(* [get_color (x,y)] is the rgb triple stored in nearest pixel to (x,y)
   :requires x,y in [0,1] *)
val get_color : float * float -> float * float * float
