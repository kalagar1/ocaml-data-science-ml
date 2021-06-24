(* Test plan:

   We used a combination of black box and manual testing to ensure the
   correctness of our project. The dataframe, matrix and statistics
   modules were extensively tested using OUnit black box testing. Since
   the functions in these modules are either computing specific values
   or changing a data structure in a specific way, it makes sense to
   test if those operations are carried out according to the
   specifications of the respective function. We made the judgement that
   glass-box testing would be an unnecessary amount of work since
   testing for edge cases and normal inputs using black box testing is
   already fairly extensive, and was very effective at helping us catch
   bugs. Glass box testing was also not a good choice as our algorithms
   were dependent on the specifications of the functions in these
   modules, not how those functions were implemented. As a result, we
   found ourselves constantly changing the implementations of functions
   without changing their specifications, which would be difficult to
   keep track of in glass box testing but requires no change in our
   strategy for black box testing.

   Owing to the unusual nature of our project, we decided that it does
   not really make sense to apply the same testing mechanism for the
   machine learning modules since the performance of a machine learning
   algorithm is dependent upon the data it receives. The fact that we
   were expecting a 85% accuracy but only got 80%, for example, is not
   an indication that the algorithm is wrong in any way. Therefore, the
   algorithms were manually tested using jupyter demos to evaluate their
   correctness and performance. The fact that they were able to
   correctly perform the classification/regression tasks, coupled with
   visual proof of how they performed on the datasets is evidence that
   they are correct.

   Finally, the command line interface was also tested manually by
   simply having different people try different, weird inputs and
   checking if the outputs were expected. User interfaces cannot really
   be tested in any other way, and we believe we have performed enough
   manual testing to claim that our interface is working flawlessly. *)

open Matrix
open Statistics
open Dataframe
open OUnit2

let comp_matrix mat1 mat2 =
  assert_equal (dim mat1) (dim mat2);
  let m1 = matrix mat1 and m2 = matrix mat2 in
  List.iter2
    (fun l1 l2 ->
      List.iter2
        (fun x1 x2 -> assert (abs_float (x1 -. x2) < 0.0001))
        l1 l2)
    m1 m2

let eye_test name n res =
  name >:: fun ctxt -> assert_equal res (eye n |> matrix)

let zero_test name m n res =
  name >:: fun ctxt -> assert_equal res (zero m n |> matrix)

let transpose_test name m res =
  name >:: fun ctxt ->
  let mat = m |> construct |> transpose in
  assert_equal res (mat |> matrix);
  assert_equal
    (List.length res, res |> List.hd |> List.length)
    (dim mat)

let lu_decomp_test name m res =
  name >:: fun ctxt ->
  assert_equal res
    (let x = m |> construct |> lu_decomp in
     (matrix (fst x), matrix (snd x)))

let invert_test name m res =
  name >:: fun ctxt ->
  comp_matrix (res |> construct) (m |> construct |> invert)

let det_test name m res =
  name >:: fun ctxt -> assert_equal res (m |> construct |> det)

let normalize_test name m res =
  name >:: fun ctxt ->
  assert_equal res (m |> construct |> normalize |> matrix)

let concat_test name m1 m2 res =
  name >:: fun ctxt ->
  assert_equal res (concat (construct m1) (construct m2) |> matrix)

let scale_test name m c res =
  name >:: fun ctxt -> assert_equal res (scale (construct m) c |> matrix)

let op_test name m1 m2 f res =
  name >:: fun ctxt ->
  comp_matrix (res |> construct) (op (construct m1) (construct m2) f)

let dot name v1 v2 res =
  name >:: fun ctxt ->
  assert_equal res (dot (construct v1) (construct v2))

let elem_f_test name m f res =
  name >:: fun ctxt ->
  assert_equal res (elem_f (construct m) f |> matrix)

let rref_test name m res =
  name >:: fun ctxt ->
  comp_matrix (rref (m |> construct)) (res |> construct)

let matrix_tests =
  [
    eye_test "5x5 identity matrix" 5
      [
        [ 1.0; 0.0; 0.0; 0.0; 0.0 ];
        [ 0.0; 1.0; 0.0; 0.0; 0.0 ];
        [ 0.0; 0.0; 1.0; 0.0; 0.0 ];
        [ 0.0; 0.0; 0.0; 1.0; 0.0 ];
        [ 0.0; 0.0; 0.0; 0.0; 1.0 ];
      ];
    zero_test "5x6 zero matrix" 5 6
      [
        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ];
        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ];
        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ];
        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ];
        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ];
      ];
    transpose_test "Transpose test on vector" [ [ 1.; 2.; 3.; 4. ] ]
      [ [ 1. ]; [ 2. ]; [ 3. ]; [ 4. ] ];
    transpose_test "Traponse test on single entry" [ [ 1. ] ] [ [ 1. ] ];
    transpose_test "Transpose test on rectangular matrix"
      [ [ 1.; 2.; 3.; 4. ]; [ 2.; 3.; 4.; 5. ]; [ 3.; 4.; 5.; 6. ] ]
      [ [ 1.; 2.; 3. ]; [ 2.; 3.; 4. ]; [ 3.; 4.; 5. ]; [ 4.; 5.; 6. ] ];
    lu_decomp_test "LU decomposition test 1"
      [ [ 2.0; -1.0; -2.0 ]; [ -4.0; 6.0; 3.0 ]; [ -4.0; -2.0; 8.0 ] ]
      ( [ [ 1.0; 0.0; 0.0 ]; [ -2.0; 1.0; 0.0 ]; [ -2.0; -1.0; 1.0 ] ],
        [ [ 2.0; -1.0; -2.0 ]; [ 0.0; 4.0; -1.0 ]; [ 0.0; 0.0; 3.0 ] ]
      );
    invert_test "Matrix inverse test 1"
      [ [ 3.0; 0.0; 2.0 ]; [ 2.0; 0.0; -2.0 ]; [ 0.0; 1.0; 1.0 ] ]
      [ [ 0.2; 0.2; 0. ]; [ -0.2; 0.3; 1. ]; [ 0.2; -0.3; 0. ] ];
    invert_test "Matrix inverse test on identity matrix"
      [
        [ 1.0; 0.0; 0.0; 0.0; 0.0 ];
        [ 0.0; 1.0; 0.0; 0.0; 0.0 ];
        [ 0.0; 0.0; 1.0; 0.0; 0.0 ];
        [ 0.0; 0.0; 0.0; 1.0; 0.0 ];
        [ 0.0; 0.0; 0.0; 0.0; 1.0 ];
      ]
      [
        [ 1.0; 0.0; 0.0; 0.0; 0.0 ];
        [ 0.0; 1.0; 0.0; 0.0; 0.0 ];
        [ 0.0; 0.0; 1.0; 0.0; 0.0 ];
        [ 0.0; 0.0; 0.0; 1.0; 0.0 ];
        [ 0.0; 0.0; 0.0; 0.0; 1.0 ];
      ];
    rref_test "Rref test 1"
      [ [ 1.; 5.; 9. ]; [ 4.; -6.; 8. ]; [ 7.; 9.; 3. ] ]
      [ [ 1.; 0.; 0. ]; [ 0.; 1.; 0. ]; [ 0.; 0.; 1. ] ];
    rref_test "Rref test 2"
      [
        [ 1.; 2.; 3. ];
        [ 4.; 5.; 6. ];
        [ 7.; 8.; 9. ];
        [ 10.; 11.; 12. ];
        [ 13.; 14.; 15. ];
      ]
      [
        [ 1.; 0.; -1. ];
        [ 0.; 1.; 2. ];
        [ 0.; 0.; 0. ];
        [ 0.; 0.; 0. ];
        [ 0.; 0.; 0. ];
      ];
    rref_test "Rref test on identity matrix"
      [
        [ 1.0; 0.0; 0.0; 0.0; 0.0 ];
        [ 0.0; 1.0; 0.0; 0.0; 0.0 ];
        [ 0.0; 0.0; 1.0; 0.0; 0.0 ];
        [ 0.0; 0.0; 0.0; 1.0; 0.0 ];
        [ 0.0; 0.0; 0.0; 0.0; 1.0 ];
      ]
      [
        [ 1.0; 0.0; 0.0; 0.0; 0.0 ];
        [ 0.0; 1.0; 0.0; 0.0; 0.0 ];
        [ 0.0; 0.0; 1.0; 0.0; 0.0 ];
        [ 0.0; 0.0; 0.0; 1.0; 0.0 ];
        [ 0.0; 0.0; 0.0; 0.0; 1.0 ];
      ];
    det_test "2x2 matrix" [ [ 4.0; 6.0 ]; [ 3.0; 8.0 ] ] 14.0;
    det_test "3x3 matrix"
      [ [ 6.0; 1.0; 1.0 ]; [ 4.0; -2.0; 5.0 ]; [ 2.0; 8.0; 7.0 ] ]
      (-306.0);
    det_test "4x4 matrix"
      [
        [ 1.0; 3.0; 5.0; 9.0 ];
        [ 1.0; 3.0; 1.0; 7.0 ];
        [ 4.0; 3.0; 9.0; 7.0 ];
        [ 5.0; 2.0; 0.0; 9.0 ];
      ]
      (-376.0);
    (let m = [ [ 1.; 2.; 3. ] ] |> construct |> magnitude in
     normalize_test "Normalize test: Row vector" [ [ 1.; 2.; 3. ] ]
       [ [ 1. /. m; 2. /. m; 3. /. m ] ]);
    (let m = [ [ 1. ]; [ 2. ]; [ 3. ] ] |> construct |> magnitude in
     normalize_test "Normalize test: Column vector"
       [ [ 1. ]; [ 2. ]; [ 3. ] ]
       [ [ 1. /. m ]; [ 2. /. m ]; [ 3. /. m ] ]);
    concat_test "Concat test 1"
      [ [ 1. ]; [ 2. ]; [ 3. ] ]
      [ [ 1. ]; [ 2. ]; [ 3. ] ]
      [ [ 1.; 1. ]; [ 2.; 2. ]; [ 3.; 3. ] ];
    concat_test "Concat test 2"
      [ [ 1. ]; [ 2. ]; [ 3. ] ]
      [ [ 1.; 1. ]; [ 2.; 2. ]; [ 3.; 3. ] ]
      [ [ 1.; 1.; 1. ]; [ 2.; 2.; 2. ]; [ 3.; 3.; 3. ] ];
    concat_test "Concat test on single entries" [ [ 1. ] ] [ [ 1. ] ]
      [ [ 1.; 1. ] ];
    scale_test "Scale test"
      [ [ 1.; 1.; 1. ]; [ 1.; 1.; 1. ]; [ 1.; 1.; 1. ] ]
      5.
      [ [ 5.; 5.; 5. ]; [ 5.; 5.; 5. ]; [ 5.; 5.; 5. ] ];
    op_test "Operation test"
      [ [ 1.; 1.; 1. ]; [ 1.; 1.; 1. ]; [ 1.; 1.; 1. ] ]
      [ [ 1.; 1.; 1. ]; [ 1.; 1.; 1. ]; [ 1.; 1.; 1. ] ]
      ( +. )
      [ [ 2.; 2.; 2. ]; [ 2.; 2.; 2. ]; [ 2.; 2.; 2. ] ];
    op_test "Operation test 2"
      [ [ 1.; 2.; 3. ]; [ 1.; 2.; 3. ] ]
      [ [ 1.; 2.; 3. ]; [ 1.; 2.; 3. ] ]
      ( *. )
      [ [ 1.; 4.; 9. ]; [ 1.; 4.; 9. ] ];
    dot "Dot test 1" [ [ 1.; 2.; 3.; 4. ] ] [ [ 1.; 2.; 3.; 4. ] ] 30.0;
    dot "Dot test 2"
      [ [ 1. ]; [ 2. ]; [ 3. ]; [ 4. ] ]
      [ [ 1.; 2.; 3.; 4. ] ] 30.0;
    dot "Dot test 3" [ [ 1.; 2.; 3.; 4. ] ]
      [ [ 1. ]; [ 2. ]; [ 3. ]; [ 4. ] ]
      30.0;
    elem_f_test "Elem-wise function test"
      [ [ 1.; 1.; 1. ]; [ 1.; 1.; 1. ]; [ 1.; 1.; 1. ] ]
      (fun x -> x *. 2.)
      [ [ 2.; 2.; 2. ]; [ 2.; 2.; 2. ]; [ 2.; 2.; 2. ] ];
  ]

let stats_test name exp out =
  name >:: fun _ -> assert_equal exp out ~printer:string_of_float

let big_float_lst lo hi =
  let rec loop lo hi acc =
    if lo > hi then acc else loop (lo +. 1.) hi (lo :: acc)
  in
  List.rev (loop lo hi [])

let basic_lst = [ 1. ]

let med_lst = [ 1.4; 5.2; 20.10; 6.32; 50.13; 232.45 ]

let big_lst = big_float_lst 1. 100.

let statistics_tests =
  [
    stats_test "basic list sum test" 1. (sum basic_lst);
    stats_test "med list sum test" 315.6 (sum med_lst);
    stats_test "big list sum test" 5050. (sum big_lst);
    stats_test "basic list mean test" 1. (mean basic_lst);
    stats_test "med list mean test" 52.6 (mean med_lst);
    stats_test "big list mean test" 50.5 (mean big_lst);
    stats_test "basic list median test" 1. (median basic_lst);
    stats_test "med list median test" 13.21 (median med_lst);
    stats_test "big list median test" 50.5 (median big_lst);
    stats_test "basic list q1 test" 1. (q1 basic_lst);
    stats_test "med list q1 test" 5.48 (q1 med_lst);
    stats_test "big list q1 test" 25.75 (q1 big_lst);
    stats_test "basic list q3 test" 1. (q1 basic_lst);
    stats_test "med list q3 test" 42.6225 (q3 med_lst);
    stats_test "big list q3 test" 75.25 (q3 big_lst);
    stats_test "basic list var test" 0. (var basic_lst);
    stats_test "med list var test" 6736. (Float.round (var med_lst));
    stats_test "big list var test" 833. (Float.round (var big_lst));
    stats_test "basic list std test" 0. (std basic_lst);
    stats_test "med list std test" 82. (Float.round (std med_lst));
    stats_test "big list std test" 29. (Float.round (std big_lst));
    stats_test "basic list 30th perc test" 1. (percentile basic_lst 30.);
    stats_test "med list 30th perc test" 5.76 (percentile med_lst 30.);
    stats_test "big list 30th perc test" 30.7 (percentile big_lst 30.);
  ]

let init_df = { header = [ "x1"; "y1"; "y2"; "y3"; "y4" ];
  data = [
    ["10"; "8"; "13"; "9"; "11"; "14"; "6"; "4"; "12"];
    ["8.04"; "6.95"; "7.58"; "8.81"; "8.33"; "9.96"; "7.24"; "4.26"; "10.84"];
    ["9.14"; "8.14"; "8.74"; "8.77"; "9.26"; "8.1"; "6.13"; "3.1"; "9.13"];
    ["7.46"; "6.77"; "12.74"; "7.11"; "7.81"; "8.84"; "6.08"; "5.39"; "8.15"];
    ["6.58"; "5.76"; "7.71"; "8.84"; "8.47"; "7.04"; "5.25"; "12.5"; "5.56"];
  ];
}

let selected_many = { header = [ "x1"; "y1"; "y2"; "y3" ];
  data = [
      ["10"; "8"; "13"; "9"; "11"; "14"; "6"; "4"; "12" ];
      ["8.04"; "6.95"; "7.58"; "8.81"; "8.33"; "9.96"; "7.24"; "4.26"; "10.84"];
      ["9.14"; "8.14"; "8.74"; "8.77"; "9.26"; "8.1"; "6.13"; "3.1"; "9.13"];
      ["7.46"; "6.77"; "12.74"; "7.11"; "7.81"; "8.84"; "6.08"; "5.39"; "8.15";];
  ];
}

let selected_one =
  {
    header = [ "x1" ];
    data = [ [ "10"; "8"; "13"; "9"; "11"; "14"; "6"; "4"; "12" ] ];
  }

let selected_many_by_indices = { header = [ "x1"; "y2"; "y3" ];
  data = [
    ["10"; "8"; "13"; "9"; "11"; "14"; "6"; "4"; "12" ];
    ["9.14"; "8.14"; "8.74"; "8.77"; "9.26"; "8.1"; "6.13"; "3.1"; "9.13"];
    ["7.46"; "6.77"; "12.74"; "7.11"; "7.81"; "8.84"; "6.08"; "5.39"; "8.15"];
  ];
}

let selected_one_by_indices = { header = [ "y1" ];
  data = [
    ["8.04"; "6.95"; "7.58"; "8.81"; "8.33"; "9.96"; "7.24"; "4.26"; "10.84"];
  ];
  }

let update_by_col = { header = [ "x1"; "y1"; "y2"; "y3"; "y4" ];
  data = [
    ["12"; "8"; "13"; "9"; "11"; "14"; "6"; "4"; "12"];
    ["8.04"; "6.95"; "7.58"; "8.81"; "8.33"; "9.96"; "7.24"; "4.26"; "10.84"];
    ["9.14"; "8.14"; "8.74"; "8.77"; "9.26"; "8.1"; "6.13"; "3.1"; "9.13"];
    ["7.46"; "6.77"; "12.74"; "7.11"; "7.81"; "8.84"; "6.08"; "5.39"; "8.15"];
    ["6.58"; "5.76"; "7.71"; "8.84"; "8.47"; "7.04"; "5.25"; "12.5"; "5.56"];
  ];
}

let update_by_index = { header = [ "x1"; "y1"; "y2"; "y3"; "y4" ];
  data = [
    ["10"; "8"; "13"; "9"; "11"; "14"; "6"; "4"; "12"];
    ["8.04"; "6.95"; "7.58"; "8.81"; "8.33"; "9.96"; "7.24"; "4.26"; "10.84"];
    ["9.14"; "12"; "8.74"; "8.77"; "9.26"; "8.1"; "6.13"; "3.1"; "9.13"];
    ["7.46"; "6.77"; "12.74"; "7.11"; "7.81"; "8.84"; "6.08"; "5.39"; "8.15"];
    ["6.58"; "5.76"; "7.71"; "8.84"; "8.47"; "7.04"; "5.25"; "12.5"; "5.56"];
  ];
}

let filter_by_col = { header = [ "x1"; "y1"; "y2"; "y3"; "y4" ];
  data = [
    ["8"; "13"; "9"; "11"; "14"; "6"; "4"; "12"];
    ["6.95"; "7.58"; "8.81"; "8.33"; "9.96"; "7.24"; "4.26"; "10.84"];
    [ "8.14"; "8.74"; "8.77"; "9.26"; "8.1"; "6.13"; "3.1"; "9.13"];
    ["6.77"; "12.74"; "7.11"; "7.81"; "8.84"; "6.08"; "5.39"; "8.15"];
    ["5.76"; "7.71"; "8.84"; "8.47"; "7.04"; "5.25"; "12.5"; "5.56"];
  ];
}

let filter_by_col_2 =
  {
    header = [ "x1"; "y1"; "y2"; "y3"; "y4" ];
    data = [ [ "10" ]; [ "8.04" ]; [ "9.14" ]; [ "7.46" ]; [ "6.58" ] ];
  }

let filter_by_index = { header = [ "x1"; "y1"; "y2"; "y3"; "y4" ];
  data = [
    [ "10"; "13"; "9"; "11"; "14"; "6"; "4"; "12" ];
    ["8.04"; "7.58"; "8.81"; "8.33"; "9.96"; "7.24"; "4.26"; "10.84"];
    ["9.14"; "8.74"; "8.77"; "9.26"; "8.1"; "6.13"; "3.1"; "9.13"];
    ["7.46"; "12.74"; "7.11"; "7.81"; "8.84"; "6.08"; "5.39"; "8.15"];
    ["6.58"; "7.71"; "8.84"; "8.47"; "7.04"; "5.25"; "12.5"; "5.56"];
  ];
}

let train_test_split_1 =
  ( [
      [ 8.04; 9.14; 7.46 ];
      [ 6.95; 8.14; 6.77 ];
      [ 7.58; 8.74; 12.74 ];
      [ 8.81; 8.77; 7.11 ];
      [ 8.33; 9.26; 7.81 ];
    ],
    [
      [ 9.96; 8.1; 8.84 ];
      [ 7.24; 6.13; 6.08 ];
      [ 4.26; 3.1; 5.39 ];
      [ 10.84; 9.13; 8.15 ];
    ],
    [ 10.; 8.; 13.; 9.; 11. ],
    [ 14.; 6.; 4.; 12. ] )

let train_test_split_2 =
  ( [
      [ 8.04; 9.14; 7.46 ];
      [ 6.95; 8.14; 6.77 ];
      [ 7.58; 8.74; 12.74 ];
      [ 8.81; 8.77; 7.11 ];
      [ 8.33; 9.26; 7.81 ];
      [ 9.96; 8.1; 8.84 ];
      [ 7.24; 6.13; 6.08 ];
      [ 4.26; 3.1; 5.39 ];
    ],
    [ [ 10.84; 9.13; 8.15 ] ],
    [ 10.; 8.; 13.; 9.; 11.; 14.; 6.; 4. ],
    [ 12. ] )

let cross_val_split_1 =
  ( [
      [ 8.04; 9.14; 7.46 ];
      [ 6.95; 8.14; 6.77 ];
      [ 7.58; 8.74; 12.74 ];
      [ 8.81; 8.77; 7.11 ];
      [ 8.33; 9.26; 7.81 ];
      [ 9.96; 8.1; 8.84 ];
      [ 7.24; 6.13; 6.08 ];
      [ 4.26; 3.1; 5.39 ];
    ],
    [ [ 10.84; 9.13; 8.15 ] ],
    [],
    [ 10.; 8.; 13.; 9.; 11.; 14.; 6.; 4. ],
    [ 12. ],
    [] )

let cross_val_split_2 =
  ( [
      [ 8.04; 9.14; 7.46 ];
      [ 6.95; 8.14; 6.77 ];
      [ 7.58; 8.74; 12.74 ];
      [ 8.81; 8.77; 7.11 ];
      [ 8.33; 9.26; 7.81 ];
      [ 9.96; 8.1; 8.84 ];
    ],
    [ [ 7.24; 6.13; 6.08 ]; [ 4.26; 3.1; 5.39 ] ],
    [ [ 10.84; 9.13; 8.15 ] ],
    [ 10.; 8.; 13.; 9.; 11.; 14. ],
    [ 6.; 4. ],
    [ 12. ] )

let load_test name file res =
  name >:: fun ctxt -> assert_equal res (loadfile file)

let selected_many_test name df cols_lst res =
  name >:: fun ctxt -> assert_equal res (select_cols df cols_lst)

let selected_one_test name df col res =
  name >:: fun ctxt -> assert_equal res (select_cols df [ col ])

let selected_many_by_indices_test name df indices_lst res =
  name >:: fun ctxt -> assert_equal res (select_cols_i df indices_lst)

let selected_one_by_indices_test name df idx res =
  name >:: fun ctxt -> assert_equal res (select_cols_i df [ idx ])

let update_by_col_test name df col f update_to res =
  name >:: fun ctxt -> assert_equal res (update df col f update_to)

let update_by_col_i_test name df col_index f update_to res =
  name >:: fun ctxt ->
  assert_equal res (update_i df col_index f update_to)

let filter_by_col_test name df col f res =
  name >:: fun ctxt -> assert_equal res (filter df col f)

let filter_by_col_i_test name df col_index f res =
  name >:: fun ctxt -> assert_equal res (filter_i df col_index f)

let train_test_split_test name df x_lst y test_percent res =
  name >:: fun ctxt ->
  assert_equal res (train_test_split df x_lst y test_percent)

let cross_val_split_test name df x_lst y test_p cross_p res =
  name >:: fun ctxt ->
  assert_equal res (split_with_cross_val df x_lst y test_p cross_p)

let dataframe_tests =
  [
    load_test "loading files" "quartet.csv" init_df;
    selected_many_test "selecting many cols by name" init_df
      [ "x1"; "y1"; "y2"; "y3" ]
      selected_many;
    selected_one_test "selecting one column by name" init_df "x1"
      selected_one;
    selected_many_by_indices_test "selecting many columns using indices"
      init_df [ 0; 2; 3 ] selected_many_by_indices;
    selected_one_by_indices_test "selecting one column using its index"
      init_df 1 selected_one_by_indices;
    update_by_col_test "updating a column by name" init_df "x1"
      (fun x -> x = "10")
      "12" update_by_col;
    update_by_col_i_test "updating a column by its index" init_df 2
      (fun x -> x = "8.14")
      "12" update_by_index;
    filter_by_col_test "filtering a column using unequal condition"
      init_df "x1"
      (fun x -> x <> "10")
      filter_by_col;
    filter_by_col_test "filtering a column using equal condition"
      init_df "x1"
      (fun x -> x = "10")
      filter_by_col_2;
    filter_by_col_i_test
      "filtering a column using its index instead of name" init_df 2
      (fun x -> x <> "8.14")
      filter_by_index;
    train_test_split_test "train test split 1" init_df
      [ "y1"; "y2"; "y3" ] "x1" 0.5 train_test_split_1;
    train_test_split_test "train test split 2" init_df
      [ "y1"; "y2"; "y3" ] "x1" 0.2 train_test_split_2;
    cross_val_split_test "cross val split test 1" init_df
      [ "y1"; "y2"; "y3" ] "x1" 0.1 0.1 cross_val_split_1;
    cross_val_split_test "cross val split test 2" init_df
      [ "y1"; "y2"; "y3" ] "x1" 0.2 0.2 cross_val_split_2;
  ]

let suite =
  "test suite for project"
  >::: List.flatten [ matrix_tests; statistics_tests ]

let _ = run_test_tt_main suite
