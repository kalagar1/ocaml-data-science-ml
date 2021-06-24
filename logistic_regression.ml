open Matrix
open Utils

(* Will use gradient descent with this. Check link:
   https://rickwierenga.com/blog/ml-fundamentals/logistic-regression.html *)
let e = 2.7182818284590452353602874713527

let w = ref (fill 1 1 (Random.float 1.0))

let g z = 1. /. (1. +. (e ** (-1.0 *. z)))

let h x = elem_f (mult x !w) g

let compute_gradient x y =
  let preds = h x in
  scale
    (mult (transpose x) (op preds y ( -. )))
    (1. /. float_of_int (x |> dim |> fst))

let rec fit x y alpha n =
  for j = 0 to pred n do
    let gradient = compute_gradient x y in
    w := op !w (scale gradient alpha) ( -. )
  done;
  !w

let predict x = h x

let fit_and_predict x_train y_train x_test y_test alpha n =
  let weights = fit x_train y_train alpha n in
  let y_p = predict x_test |> transpose |> matrix |> List.hd in
  let y_t = y_test |> transpose |> matrix |> List.hd in
  let acc = accuracy y_t y_p in
  let mse = mean_squared_error y_t y_p in
  (acc, mse, y_p)
