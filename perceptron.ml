open Matrix

let w = ref (fill 1 3 (Random.float 1.))

let train mat alpha actual n =
  let x = concat (fill (mat |> dim |> fst) 1 1.0) mat in
  let x' = transpose x in
  let actual = transpose actual in
  for i = 1 to n do
    let sum = mult !w x' in
    let predicted = elem_f sum (fun x -> if x > 0. then 1.0 else 0.0) in
    let diff = scale (mult (op actual predicted ( -. )) x) alpha in
    w := op !w diff ( +. )
  done;
  !w
