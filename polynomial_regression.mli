(** Polynomial regression algorithm*)

(** [fit v1 v2 n] takes in the x-values v1 and y-values v2 as column
    vectors and the degree of polynomial n and returns the weights
    corresponding to each term as a column vector*)
val fit : Matrix.t -> Matrix.t -> int -> Matrix.t

(** [predict v1] takes in the x-values v1 and produces the corresponding
    y-values as a column vector *)
val predict : Matrix.t -> Matrix.t

(** [fit_and_predict x_train y_train x_test y_test n] fits the
    polynomial regression model using features [x_train] and target
    [y_train] then predicts the targets of [x_test], and returns the
    (accuracy, mean-squared error, y_pred) when comparing this
    prediction to the actual [y_test] *)
val fit_and_predict :
  Matrix.t ->
  Matrix.t ->
  Matrix.t ->
  Matrix.t ->
  int ->
  float * float * float list
