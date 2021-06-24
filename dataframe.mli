(** Representation of dataframes

    This module represents dataframes and provides useful functions to
    update/filter*)

type dataframe = { header : string list; data : string list list }
(** type [dataframe] represents a dataframe with that has column names
    [header] and data values represented by the string list list [data]*)

val loadfile : string -> dataframe
(** [loadfile str] creates a dataframe from the csv [str]*)

val save_df : dataframe -> string -> unit
(** [save_df df str] saves the dataframe [df] as a csv file names [str]*)

val print_df : dataframe -> unit
(** [print_df df] prints the dataframe [df] to the terminal*)

val encode : string list -> float list
(** [encode lst] translates the string list [lst] to a float list*)

val cols_to_float : dataframe -> float list list
(** [cols_to_float df] translates the columns of the dataframe [df] from
    string to float*)

val pre_process : dataframe -> dataframe
(** [pre_process df] pre processes [df] by converting all the values in
    the df from their default type to float*)

val select_cols : dataframe -> string list -> dataframe
(** [select_cols df cols_lst] returns a dataframe that contains only the
    columns in the [cols_lst] in [df]*)

val select_cols_i : dataframe -> int list -> dataframe
(** [select_cols df indices] returns a dataframe of index [indeces] from
    [df]*)

val rename_col : dataframe -> string -> string -> dataframe
(** [rename_col df col_name updated_name] returns a dataframe in which 
    the [col_name] column of [df] is renamed to [updated_name] *)

val rename_col_i : dataframe -> int -> string -> dataframe
(** [rename_col_i df index updated_name] returns a dataframe in which 
    the [index]th column of [df] is renamed to [updated_name] *)

val update : dataframe -> string -> (string -> bool) -> string -> dataframe
(** [update df col f str] updates the values of column [col] in
    dataframe [df] that satisfy the function [f] to be the new value
    [str] *)

val update_i : dataframe -> int -> (string -> bool) -> string -> dataframe
(** [update df index f str] updates the values of the column at [index]
    in dataframe [df] that satisfy the function [f] to be the new value
    [str] *)

val filter : dataframe -> string -> (string -> bool) -> dataframe
(** [filter df col f] filters the rows of dataframe [df] by filtering
    the values of colulmn [col] using the function [f] *)

val filter_i : dataframe -> int -> (string -> bool) -> dataframe
(** [filter df index f] filters the rows of dataframe [df] by filtering
    the values of the colulmn at [index] using the function [f] *)

val train_test_split :
  dataframe ->
  string list ->
  string ->
  float ->
  float list list * float list list * float list * float list
(** [train_test_split df features target test_percent] splits the
    dataframe [df] into training and testing sets (x_train, x_test,
    y_train, y_test) with [features] and a [target] and test percent of
    [test_percent] *)

val split_with_cross_val :
  dataframe ->
  string list ->
  string ->
  float ->
  float ->
  float list list
  * float list list
  * float list list
  * float list
  * float list
  * float list
(** [split_with_cross_val df features target test_percent cross_percent]
    splits the dataframe [df] into training, validation, and testing
    sets (x_train, x_validation, x_test, y_train, y_validation, y_test)
    with [features], [target], test percent of [test_percent], and cross
    percent of [cross_percent] *)
