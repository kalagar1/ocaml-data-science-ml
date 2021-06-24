(** K-means clustering algorithm*)

type point = { tag : int; pos : float * float }
(** type point represents a point with a position [pos] and tag [tag]*)

val construct : (float * float) list -> point array
(** [construct lst] takes in a list of pairs x * y containing the (x, y)
    coordinates and returns an array of points*)

val to_list : point array -> (float * float) list
(** [to_list pts] takes in an array of points and returns a list of
    pairs x * y. This is the reverse of [construct]. *)

val classify : point array -> int -> int -> point array * point array
(** [classify pts n iter] takes in an array of points containing the
    data, number of clusters desired n, max number of iterations iter,
    and returns a pair of points containing the centroids as the first
    element and classified points as the second. *)
