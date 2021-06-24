(** Logistic regression algorithm*)

(** [fit x y alpha n] takes in col vectors x and y, an alpha value, and
    number of repitions n, and returns the predicted weights as a column
    vector *)
val fit : Matrix.t -> Matrix.t -> float -> int -> Matrix.t

(** [predict x] takes in a column vector of x and returns of column
    vector of outputs y *)
val predict : Matrix.t -> Matrix.t

(** [fit_and_predict x_train y_train x_test y_test n] fits the logistic
    regression model using features [x_train] and target [y_train] then
    predicts the targets of [x_test], and returns the (accuracy,
    mean-squared error, y_pred) when comparing this prediction to the
    actual [y_test] *)
val fit_and_predict :
  Matrix.t ->
  Matrix.t ->
  Matrix.t ->
  Matrix.t ->
  float ->
  int ->
  float * float * float list
