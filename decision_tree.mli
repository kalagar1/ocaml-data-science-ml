(** Decision tree algorithm *)

type node = {
  feature_i : int option;
  threshold : float option;
  value : float option;
  true_branch : node option;
  false_branch : node option;
}
(** [node] is the representation of the tree that is built in the 
    decision tree algorithm. *)

val predict : Matrix.t -> Float.t list -> float list
(** [predict x y] fits the decision tree model using features [x] and
    target [y] then returns the predictions for the features [x] *)

val fit_and_predict :
  Matrix.t ->
  Matrix.t ->
  Float.t list ->
  float list ->
  float * float * float list
(** [fit_and_predict x_train x_test y_train y_test] fits the decision tree
    model using features [x_train] and target [y_train] then predicts
    the targets of [x_test], and returns the (accuracy, mean-squared
    error, y_pred) when comparing this prediction to the actual [y_test] *)
