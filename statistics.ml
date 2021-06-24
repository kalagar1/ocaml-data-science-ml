let sum x = List.fold_left (fun x y -> x +. y) 0. x

let mean x =
  let len = Float.of_int (List.length x) in
  sum x /. len

let round x = floor (x +. 0.5)

let percentile x perc =
  let p = perc /. 100. in
  let sorted_x = List.sort compare x in
  let len = List.length x in
  let index = p *. float_of_int (len - 1) in
  let trun_i = int_of_float index in
  let delta = index -. float_of_int trun_i in
  if len = 0 then 0.
  else if trun_i = len - 1 then List.nth sorted_x trun_i
  else
    ((1. -. delta) *. List.nth sorted_x trun_i)
    +. (delta *. List.nth sorted_x (trun_i + 1))

let median x = percentile x 50.

let q1 x = percentile x 25.

let q3 x = percentile x 75.

let var x =
  let mean_x = mean x in
  let deviations =
    List.map (fun e -> (e -. mean_x) *. (e -. mean_x)) x
  in
  mean deviations

let std x = Float.sqrt (var x)
