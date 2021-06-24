(** Perceptron algorithm *)

val train : Matrix.t -> float -> Matrix.t -> int -> Matrix.t
(**[train mat alpha actual n] takes in the concatenated columns of x and
   y as mat, the learning rate as alpha, the tags of the features as
   actual (e.g. 0 or 1), and the number of iterations as n. Function
   returns the weights as output.*)
