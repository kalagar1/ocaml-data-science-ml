(** Representation of matrices

    This module represents matrices and houses key matrix functions*)

type t = { dimensions : int * int; matrix : float list list }
(** type [t] represents a matrix with dimensions [dimensions] and values
    represented by the float list list [matrix]*)

exception InvalidDimensions of string
(** Raised when a matrix function cannot be applied due to different
    dimensions*)

val dim : t -> int * int
(** [dim m] returns the dimensions of matrix m *)

val matrix : t -> float list list
(** [matrix m] returns the the float list list representation of matrix
    m*)

val fill : int -> int -> float -> t
(** [fill m n x] returns a m x n matrix filled with float x *)

val zero : int -> int -> t
(** [empty m n] produces an m x n matrix of 0's *)

val eye : int -> t
(** [eye n] produces an n x n identity matrix *)

val transpose : t -> t
(** [transpose m] transposes the matrix m*)

val mult : t -> t -> t
(** [mult m1 m2] produces a new matrix of the product of m1 and m2 *)

val rref : t -> t
(** [echelon m] returns the reduced form of matrix m*)

val construct : float list list -> t
(** [construct m] takes in a float list list m and returns matrix.
    Throws InvalidDimensions if rows are not of the same length *)

val lu_decomp : t -> t * t
(** [decomp m] produces a pair of matrices containing the
    LU-decomposition of matrix m*)

val concat : t -> t -> t
(** [concat m1 m2] concatenates m1 and m2 and returns the resulting
    matrix [m1 m2]*)

val invert : t -> t
(** [invert m] returns the inverse of matrix m. Throws InvalidDimensions
    if m is not a square matrix. *)

val pinv : t -> t
(** [pinv m] returns the pseudoinverse of matrix m*)

val det : t -> float
(** [det m] returns the determinant of matrix m *)

val magnitude : t -> float
(** [magnitude m] takes in a matrix of type t and returns its magnitude
    Requires: m is a vector *)

val normalize : t -> t
(** [normalize m] returns a normalized vector if m is a vector. m / (det
    m) is returned otherwise. *)

val eigen : t -> bool -> float * t
(** [eigen mat dom] returns a pair float * t containing the eigenvector
    and its associated eigenvalue. If dom is true then function returns
    the dominant eigenvector, else the smallest. *)

val elem_pow : t -> float -> t
(** [elem_pow m r] raises each entry of m to the power of r and returns
    the resulting matrix.*)

val scale : t -> float -> t
(** [scale m c] takes in m and scales it by factor of c*)

val op : t -> t -> (float -> float -> float) -> t
(** [op m1 m2 f] applies the operator f to pair of elements in m1 m2 and
    returns a matrix containing \[\[f a1 b1; f a2 b2; ...\]; ...\] *)

val dot : t -> t -> float
(** [dot v1 v2] returns the dot product between two vectors. If v1 and
    v2 are two dimensional matrices instead, performs [mult v1 v2]*)

val elem_f : t -> (float -> float) -> t
(** [elem_f m f] applies the element-wise function f to matrix m and
    returns the resulting matrix *)
