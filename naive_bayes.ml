open Matrix
open Float

let x_where_c x y c =
  let x_indices =
    List.mapi (fun i x -> if x = c then i else -1) y
    |> List.filter (fun x -> x <> -1)
  in
  List.map
    (fun single_list ->
      List.filteri (fun i x -> List.mem i x_indices) single_list)
    x

let fit x y =
  let classes = List.sort_uniq compare y in
  let parameters =
    List.map
      (fun c ->
        let cols =
          x_where_c x y c |> construct |> transpose |> matrix
        in
        List.map
          (fun col -> (Statistics.mean col, Statistics.var col))
          cols)
      classes
  in
  (classes, parameters)

let calc_likelihood mean var x =
  let coeff = 1. /. sqrt ((2. *. pi *. var) +. min_float) in
  let exponent =
    exp (-1. *. pow (x -. mean) 2. /. ((2. *. var) +. min_float))
  in
  coeff *. exponent

let calc_prior y c =
  let y_indices = List.map (fun x -> if x = c then 1. else 0.) y in
  Statistics.mean y_indices

let rec mult_all_in_lst lst =
  match lst with [] -> 1. | h :: t -> h *. mult_all_in_lst t

let rec max_of_lst lst =
  match lst with
  | [ x ] -> x
  | h :: t -> max h (max_of_lst t)
  | [] -> failwith "Empty"

let find_index_in_lst elem lst =
  let rec find_index elem lst current_index =
    match lst with
    | [] -> failwith "Element not found"
    | h :: t ->
        if h = elem then current_index
        else find_index elem lst (current_index + 1)
  in
  find_index elem lst 0

let classify classes parameters row y =
  let posteriors =
    List.mapi
      (fun i c ->
        let feature_and_params =
          List.combine row (List.nth parameters i)
        in
        let lst_of_likelihoods =
          List.map
            (fun x ->
              let feature_value, params = x in
              calc_likelihood (fst params) (snd params) feature_value)
            feature_and_params
        in
        let posterior = calc_prior y c in
        let likelihoods_multiplied =
          mult_all_in_lst lst_of_likelihoods
        in
        posterior *. likelihoods_multiplied)
      classes
  in
  List.nth classes
    (find_index_in_lst (max_of_lst posteriors) posteriors)

let predict x y =
  let classes, parameters = fit (matrix x) y in
  List.map (fun row -> classify classes parameters row y) (matrix x)

let fit_and_predict x_train x_test y_train y_test =
  let classes, parameters = fit (matrix x_train) y_train in
  let y_pred =
    List.map
      (fun row -> classify classes parameters row y_train)
      (matrix x_test)
  in
  let acc = Utils.accuracy y_test y_pred in
  let mse = Utils.accuracy y_test y_pred in
  (acc, mse, y_pred)
