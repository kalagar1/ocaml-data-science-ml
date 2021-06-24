(** K-nearest neighbors algorithm*)

(** [set_k k] sets the value k to be used in the knn algorithm, and
    returns k *)
val set_k : int -> int

(** [predict x_test x_train y_train] computes the predicted
    corresponding labels of x_test using the x_train and y_train as
    training data for the points and labels respectively*)
val predict : float list list -> float list list -> 'a list -> 'a list

(** [fit_and_predict x_train y_train x_test y_test n] fits the k nearest
    neighbors model using features [x_train] and target [y_train] then
    predicts the targets of [x_test], and returns the (accuracy,
    mean-squared error, y_pred) when comparing this prediction to the
    actual [y_test] *)
val fit_and_predict :
  float list list ->
  float list ->
  float list list ->
  float list ->
  float * float * float list
