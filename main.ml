let file_error_msg =
  "\nSorry, could not find the file. Please try again: \n\n"

let command_error_msg =
  "\nCould not execute command command. Please try again. \n\n"

let farewell_msg = "\nThank you!\n\n"

let print_red = ANSITerminal.print_string [ ANSITerminal.red ]

let print_blue = ANSITerminal.print_string [ ANSITerminal.blue ]

let print_magenta = ANSITerminal.print_string [ ANSITerminal.magenta ]

let print_green = ANSITerminal.print_string [ ANSITerminal.green ]

let print_init_instr () =
  print_string
    "Choose a number: (Eg. If you want to update/impute values, enter 3) \n";
  print_blue "1. Selece subset of columns\n";
  print_blue "2. Rename a column\n";
  print_blue "3. Update/impute values\n";
  print_blue "4. Save dataset as csv file\n";
  print_blue "5. Split dataset into training and test sets\n"

let print_model_choice () =
  print_string
    "\n\
     Choose a learning method: \n";
  print_blue "\n1. Supervised Learning";
  print_blue "\n2. Unsupervised Learning"

let rec choose_model x_train x_test y_train y_test =
  print_model_choice ();
  print_string "\n> ";
  let user_input = read_line () in

  match user_input with
  | "quit" ->
      print_magenta farewell_msg;
      exit 0
  | "1" | "2" ->
      execute_model x_train x_test y_train y_test user_input
  | _ ->
      print_red command_error_msg;
      choose_model x_train x_test y_train y_test

and execute_model x_train x_test y_train y_test user_input =
  let x_train_mat = Matrix.construct x_train in
  let x_test_mat = Matrix.construct x_test in
  let y_train_mat =
    [ y_train ] |> Matrix.construct |> Matrix.transpose
  in
  let y_test_mat = [ y_test ] |> Matrix.construct |> Matrix.transpose in

  let acc, mse, y_pred =
    match user_input with
    | "1" ->
        Logistic_regression.fit_and_predict x_train_mat y_train_mat
          x_test_mat y_test_mat 0.1 1000
    | "2" -> Knn.fit_and_predict x_train y_train x_test y_test
    | _ -> failwith "impossible branch"
  in
  print_endline ("Accuracy: " ^ string_of_float acc);
  print_endline ("Mean-Squared Error: " ^ string_of_float mse);
  print_endline "y_pred, y_actual:";
  List.iter2
    (fun p a ->
      print_string (string_of_float p);
      print_string (" " ^ string_of_float a ^ "\n"))
    y_pred y_test

let rec split_file file =
  print_string
    "\nEnter the feature column\n";
  let input_string = read_line () in
  let cols = String.split_on_char ' ' input_string in
  print_string "\nEnter the target column: \n";
  let target = read_line () in
  print_string "\nEnter the test percent: \n";
  let test_percent = read_line () in
  try
    let x_train, x_test, y_train, y_test =
      Dataframe.train_test_split file cols target
        (float_of_string test_percent)
    in
    choose_model x_train x_test y_train y_test
  with _ ->
    print_red command_error_msg;
    Dataframe.print_df file;
    split_file file

let rec handle_select file input_func =
  print_string
    "\n\
      Enter the columns you want to select, one after another, \ 
      separated by a single space: \n";
  let input_string = read_line () in
  match input_string with
  | "quit" ->
      print_magenta farewell_msg;
      exit 0
  | v -> (
      try
        let cols = String.split_on_char ' ' v in
        let file_updated =
          Dataframe.select_cols file cols
        in
        print_green
          "\nColumns were succesfully selected! \ 
          The resulting dataset is: \n\n";
        Dataframe.print_df file_updated;
        input_func file_updated
      with _ ->
        print_red command_error_msg;
        handle_select file input_func)

let rec handle_rename file input_func =
  print_string "Enter the column you want to rename: ";
  let v1 = read_line () in 
  print_string "What do you want to rename the column to? ";
  let v2 = read_line () in 
  match v1, v2 with
  | "quit", _ ->
      print_magenta farewell_msg;
      exit 0
  | _, "quit" ->
      print_magenta farewell_msg;
      exit 0
  | update_what, update_with -> (
      try
        let file_updated = 
          Dataframe.rename_col file update_what update_with in
        print_green
          "\nColumn was succesfully renamed! The resulting dataset \ 
          is: \n\n";
        Dataframe.print_df file_updated;
        input_func file_updated
      with _ ->
        print_red command_error_msg;
        handle_select file input_func)

let rec handle_impute file input_func =
  print_string
    "Enter the name of the column you want to update values for: ";
  let col = read_line () in
  match col with
  | "quit" ->
      print_magenta farewell_msg;
      exit 0
  | col_name -> (
      try
        print_string
          "Enter the value in this column that you like to replace: ";
        let update_what = read_line () in
        print_string
          "Enter the value that you would like to replace the previous \
            value: ";
        let update_with = read_line () in
        let file_updated =
          Dataframe.update file col_name
            (fun x -> x = update_what)
            update_with
        in
        print_green
          "\nValue succesfully imputed! The resulting dataset is: \n\n";
        Dataframe.print_df file_updated;
        input_func file_updated
      with _ ->
        print_red command_error_msg;
        handle_impute file input_func)

let rec handle_save file input_func =
  print_string
    "\n\
      Enter the name of the file where you want to save the dataset \n";
  let input_string = read_line () in
  match input_string with
  | "quit" ->
      print_magenta farewell_msg;
      exit 0
  | v -> (
      try
        Dataframe.save_df file v;
        print_green "Dataset succesfully saved!"
      with _ ->
        print_red command_error_msg;
        handle_save file input_func)

let rec ask_input file =
  print_init_instr ();
  print_string "> ";
  let user_input = read_line () in
  match user_input with
  | "1" -> handle_select file ask_input
  | "2" -> handle_rename file ask_input
  | "3" -> handle_impute file ask_input
  | "4" -> handle_save file ask_input
  | "5" -> split_file file
  | "quit" ->
      print_magenta farewell_msg;
      exit 0
  | _ ->
      print_red command_error_msg;
      ask_input file

let rec start_ui () =
  print_string
    "Please enter the name of the csv file you want to load, or ";
  print_red "quit\n";
  print_string "> ";
  match read_line () with
  | "quit" ->
      print_magenta "Thank you!\n";
      exit 0
  | exception End_of_file ->
      print_red file_error_msg;
      start_ui ()
  | file_name -> (
      try
        print_endline "";
        let file = Dataframe.loadfile file_name in
        Dataframe.print_df file;
        ask_input file
      with _ ->
        print_red file_error_msg;
        start_ui ())

let main () =
  ANSITerminal.erase Screen;
  print_blue "\nWelcome to the OCaml Data Science Library.\n\n";
  start_ui ()

let () = main ()
