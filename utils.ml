let diff l1 l2 =
  try List.mapi (fun i x -> List.nth l1 i -. List.nth l2 i) l1
  with _ -> failwith "List lengths do not match"

let mean_squared_error y_true y_pred =
  Statistics.mean (List.map (fun x -> x ** 2.) (diff y_true y_pred))

let distance x1 x2 =
  List.map (fun x -> x ** 2.) (diff x1 x2)
  |> Statistics.sum |> Float.sqrt

let accuracy y_true y_pred =
  try
    let num_of_same_values =
      List.mapi
        (fun i x ->
          if List.nth y_true i = List.nth y_pred i then 1 else 0)
        y_true
      |> List.filter (fun x -> x = 1)
      |> List.length
    in
    float_of_int num_of_same_values /. float_of_int (List.length y_true)
  with _ -> failwith "List lengths do not match"
