(** Statistics functions*)

val sum : float list -> float
(** [sum x] computes the sum of the elements of x*)

val mean : float list -> float
(** [mean x] computes the mean of x *)

val percentile : float list -> float -> float
(** [percentile x p] computes the p_th percentile of x*)

val median : float list -> float
(** [median x] computes the median of x *)

val q1 : float list -> float
(** [q1 x] computes the first quartile of x*)

val q3 : float list -> float
(** [q3 x ] computes the third quartile of x*)

val var : float list -> float
(** [var x] computes the variance of x*)

val std : float list -> float
(** [std x] computes the standard deviation of x *)
