open Matrix
open Utils

let w = ref (zero 1 1)

(* Using Normal Equation Source:
   https://medium.com/analytics-vidhya/linear-regression-and-polynomial-regression-using-normal-equation-method-c3929d71734d *)
let fit x y n =
  let rec create r acc =
    if r = n + 1 then acc
    else create (r + 1) (concat acc (elem_pow x (float_of_int r)))
  in
  let x = create 1 (fill (x |> dim |> fst) 1 1.0) in
  let inv = pinv x in
  let w_ = mult inv y in
  w := w_;
  w_

let predict x =
  let n = !w |> dim |> fst in
  let rec make r acc =
    if r = n then acc
    else make (r + 1) (concat acc (elem_pow x (float_of_int r)))
  in
  let x' = make 0 (List.init (x |> dim |> fst) (fun x -> []) |> construct) in
  mult x' !w

let fit_and_predict x_train y_train x_test y_test n =
  let weights = fit x_train y_train n in
  let y_p = predict x_test |> transpose |> matrix |> List.hd in
  let y_t = y_test |> transpose |> matrix |> List.hd in
  let acc = accuracy y_t y_p in
  let mse = mean_squared_error y_t y_p in
  (acc, mse, y_p)
