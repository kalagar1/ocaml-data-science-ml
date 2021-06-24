type dataframe = {
  header : string list;
  data : string list list;
}

let loadfile file_name =
  let x = file_name |> Csv.load |> Csv.trim |> Csv.square in
  let head, dat =
    match x with h :: d -> (h, Csv.transpose d) | [] -> assert false
  in
  { header = head; data = dat }

let save_df df file_name =
  Csv.save file_name (df.header :: Csv.transpose df.data)

let print_df df =
  Csv.print_readable (df.header :: Csv.transpose df.data);
  print_endline ""

let get_encodings string_lst =
  let uniques =
    string_lst
    |> List.map (fun x -> String.lowercase_ascii x)
    |> List.sort_uniq compare
  in
  let len = List.length uniques in
  let one_to_n = List.init len (fun x -> float_of_int (x + 1)) in
  List.combine uniques one_to_n

let encode string_lst =
  let pairs = get_encodings string_lst in
  let lowercase_lst =
    List.map (fun x -> String.lowercase_ascii x) string_lst
  in
  List.map (fun x -> List.assoc x pairs) lowercase_lst

let string_to_float lst_of_str_lst =
  let clean_up lst =
    try List.map (fun x -> float_of_string x) lst with _ -> encode lst
  in
  List.map (fun x -> clean_up x) lst_of_str_lst

let cols_to_float df =
  let clean_up lst =
    try List.map (fun x -> float_of_string x) lst with _ -> encode lst
  in
  List.map (fun x -> clean_up x) df.data

let pre_process df =
  let clean_up lst = List.map (fun x -> string_of_float x) lst in
  let u_data = List.map (fun x -> clean_up x) (cols_to_float df) in
  { df with data = u_data }

let rec find_index x lst =
  match lst with
  | [] -> failwith "Not found"
  | h :: t -> if h = x then 0 else find_index x t + 1

let indices_from_col_lst df col_lst =
  List.map (fun x -> find_index x df.header) col_lst

let rename_cols_helper df index updated_name =
  let u_header =
    List.mapi
      (fun i x ->
        if i = index then updated_name else x)
      df.header
  in
  { df with header = u_header }

let rename_col df col_name updated_name =
  let index = find_index col_name df.header in
  rename_cols_helper df index updated_name

let rename_col_i df index updated_name =
  rename_cols_helper df index updated_name

let select_cols_helper df indices =
  let u_header =
    List.filteri (fun i x -> List.mem i indices) df.header
  in
  let u_data = List.filteri (fun i x -> List.mem i indices) df.data in
  { header = u_header; data = u_data }

let select_cols df col_lst =
  let indices = indices_from_col_lst df col_lst in
  select_cols_helper df indices

let select_cols_i df indices = select_cols_helper df indices

let filter_helper df index f =
  let column = List.nth df.data index in
  let indices =
    List.mapi (fun i x -> if f x then i else -1) column
    |> List.filter (fun x -> x <> -1)
  in
  let u_data =
    List.map
      (fun x -> List.filteri (fun i y -> List.mem i indices) x)
      df.data
  in
  { df with data = u_data }

let filter df col f =
  let index = find_index col df.header in
  filter_helper df index f

let filter_i df index f = filter_helper df index f

let update_helper df index f new_value =
  let column = List.nth df.data index in
  let u_col = List.map (fun x -> if f x then new_value else x) column in
  let u_data =
    List.mapi (fun i x -> if i = index then u_col else x) df.data
  in
  { df with data = u_data }

let update df col f new_value =
  let index = find_index col df.header in
  update_helper df index f new_value

let update_i df index f new_value = update_helper df index f new_value

let get_x_y df x_lst y =
  let u_df = pre_process df in
  let x = (select_cols u_df x_lst).data in
  let index = find_index y u_df.header in
  let y =
    List.map (fun x -> float_of_string x) (List.nth u_df.data index)
  in
  (x, y)

let train_test_split df x y test_percent =
  let x, y = get_x_y df x y in
  if test_percent >= 1. then failwith "Make sure the test_percent < 1"
  else
    let split_i =
      int_of_float
        (float_of_int (List.length y)
        -. (float_of_int (List.length y) /. (1. /. test_percent)))
    in

    let x_as_rows = Csv.transpose x in
    let x_train =
      x_as_rows
      |> List.filteri (fun i x -> i <= split_i)
      |> string_to_float
    in
    let x_test =
      x_as_rows
      |> List.filteri (fun i x -> i > split_i)
      |> string_to_float
    in

    let y_train = List.filteri (fun i x -> i <= split_i) y in
    let y_test = List.filteri (fun i x -> i > split_i) y in
    (x_train, x_test, y_train, y_test)

let split_with_cross_val df x y test_percent cross_percent =
  let x, y = get_x_y df x y in
  if cross_percent +. test_percent >= 1. then
    failwith
      "Make sure test_percent and cross_percent amount to less than 1"
  else
    let split_cr =
      int_of_float
        (float_of_int (List.length y)
        -. float_of_int (List.length y)
           /. (1. /. (cross_percent +. test_percent)))
    in
    let split_test =
      int_of_float
        (float_of_int (List.length y)
        -. (float_of_int (List.length y) /. (1. /. test_percent)))
    in

    let x_as_rows = Csv.transpose x in
    let x_validation =
      x_as_rows
      |> List.filteri (fun i x -> i > split_cr && i <= split_test)
      |> string_to_float
    in
    let x_train =
      x_as_rows
      |> List.filteri (fun i x -> i <= split_cr)
      |> string_to_float
    in
    let x_test =
      x_as_rows
      |> List.filteri (fun i x -> i > split_test)
      |> string_to_float
    in

    let y_validation =
      List.filteri (fun i x -> i > split_cr && i <= split_test) y
    in
    let y_train = List.filteri (fun i x -> i <= split_cr) y in
    let y_test = List.filteri (fun i x -> i > split_test) y in
    (x_train, x_validation, x_test, y_train, y_validation, y_test)
