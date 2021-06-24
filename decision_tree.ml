open Matrix
open Float

type node = {
  feature_i : int option;
  threshold : float option;
  value : float option;
  true_branch : node option;
  false_branch : node option;
}

type best_criteria = {
  feature_i : int;
  threshold : float;
}

type best_sets = {
  left_x : float list list;
  left_y : float list;
  right_x : float list list;
  right_y : float list;
}

let min_samples_split = 2

let min_impurity = min_float

let max_depth = 20

let entropy_for_label y label =
  let log2 x = log x /. log 2. in
  let count = List.length (List.filter (fun x -> x = label) y) in
  let p = float_of_int (count / List.length y) in
  -1. *. p *. log2 p

let calculate_entropy y =
  let unique_labels = List.sort_uniq compare y in
  List.map (fun label -> entropy_for_label y label) unique_labels
  |> Statistics.sum

let divide_on_feature x feature_i threshold =
  let split_func row = List.nth row feature_i >= threshold in
  let x1 = List.filter (fun row -> split_func row) x in
  let x2 = List.filter (fun row -> not (split_func row)) x in
  (x1, x2)

let rec num_occurence_in_lst elem lst =
  match lst with
  | [] -> 0
  | h :: t ->
      if h = elem then 1 + num_occurence_in_lst elem t
      else num_occurence_in_lst elem t

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

let leaf_value_calculate y =
  let classes = List.sort_uniq compare y in
  let count_lst =
    List.map (fun x -> float_of_int (num_occurence_in_lst x y)) classes
  in
  let max_elem = max_of_lst count_lst in
  let most_common_index = find_index_in_lst max_elem count_lst in
  List.nth classes most_common_index

let fv_before x n_features =
  List.map (fun row -> List.filteri (fun i x -> i <= n_features) row) x

let fv_after x n_features =
  List.map (fun row -> List.filteri (fun i x -> i >= n_features) row) x

let impurity_calculate y y1 y2 =
  let p =
    float_of_int (List.length y1) /. float_of_int (List.length y2)
  in
  let entropy = calculate_entropy y in
  entropy
  -. (p *. calculate_entropy y1)
  -. ((1. -. p) *. calculate_entropy y2)

let to_single_col y =
  List.map
    (fun x ->
      match x with
      | [ x ] -> x
      | _ -> failwith "Cannot convert to single column")
    y

let find_best_criteria_sets criteria_sets_lst =
  let impurities_lst =
    List.map
      (fun (impurity, criteria, set) -> impurity)
      criteria_sets_lst
  in
  let largest_impurity = max_of_lst impurities_lst in
  let largest_index =
    find_index_in_lst largest_impurity impurities_lst
  in
  List.nth criteria_sets_lst largest_index

let rec build_tree x y current_depth =
  let y_as_float_lst_lst = List.map (fun x -> [ x ]) y in
  let xy = y_as_float_lst_lst |> construct |> concat (construct x) in
  let cols = xy |> transpose |> matrix in
  let n_samples, n_features = dim xy in
  if n_samples < min_samples_split && current_depth > max_depth then
    let leaf_value = leaf_value_calculate y in
    {
      feature_i = None;
      threshold = None;
      value = Some leaf_value;
      true_branch = None;
      false_branch = None;
    }
  else
    let best_values_for_features =
      List.mapi
        (fun i col ->
          let unique_values = List.sort_uniq compare col in
          let criteria_sets_lst =
            List.map
              (fun t ->
                let xy1, xy2 = divide_on_feature (matrix xy) i t in
                if List.length xy1 > 0 && List.length xy2 > 0 then
                  let y1 = fv_after xy1 n_features |> to_single_col in
                  let y2 = fv_after xy2 n_features |> to_single_col in
                  let impurity = impurity_calculate y y1 y2 in
                  ( impurity,
                    { feature_i = i; threshold = t },
                    {
                      left_x = fv_before xy1 n_features;
                      left_y = fv_after xy1 n_features |> to_single_col;
                      right_x = fv_before xy2 n_features;
                      right_y = fv_after xy2 n_features |> to_single_col;
                    } )
                else
                  ( min_float,
                    { feature_i = 0; threshold = 0. },
                    {
                      left_x = [];
                      left_y = [];
                      right_x = [];
                      right_y = [];
                    } ))
              unique_values
          in
          let best_values_for_threshold =
            find_best_criteria_sets criteria_sets_lst
          in
          best_values_for_threshold)
        cols
    in
    let largest_impurity, best_c, best_s =
      find_best_criteria_sets best_values_for_features
    in
    if largest_impurity > min_impurity then
      let t_branch =
        build_tree best_s.left_x best_s.left_y (current_depth + 1)
      in
      let f_branch =
        build_tree best_s.right_x best_s.right_y (current_depth + 1)
      in
      {
        feature_i = Some best_c.feature_i;
        threshold = Some best_c.threshold;
        value = None;
        true_branch = Some t_branch;
        false_branch = Some f_branch;
      }
    else
      let leaf_value = leaf_value_calculate y in
      {
        feature_i = None;
        threshold = None;
        value = Some leaf_value;
        true_branch = None;
        false_branch = None;
      }

let fit x y = build_tree (matrix x) y 0

let rec predict_value row tree =
  match tree.value with
  | Some x -> x
  | None ->
      let f_i =
        match tree.feature_i with
        | Some x -> x
        | None -> failwith "This value can never be none"
      in
      let th =
        match tree.threshold with
        | Some x -> x
        | None -> failwith "This value can never be none"
      in
      let t_branch =
        match tree.true_branch with
        | Some x -> x
        | None -> failwith "This value can never be none"
      in
      let f_branch =
        match tree.false_branch with
        | Some x -> x
        | None -> failwith "This value can never be none"
      in
      let feature_value = List.nth row f_i in
      let branch = if feature_value >= th then t_branch else f_branch in
      predict_value row branch

let predict x y =
  let tree = fit x y in
  List.map (fun row -> predict_value row tree) (matrix x)

let fit_and_predict x_train x_test y_train y_test =
  let tree = fit x_train y_train in
  let y_pred =
    List.map (fun row -> predict_value row tree) (matrix x_test)
  in
  let acc = Utils.accuracy y_test y_pred in
  let mse = Utils.mean_squared_error y_test y_pred in
  (acc, mse, y_pred)
