type t = { 
  dimensions : int * int; 
  matrix : float list list 
}

exception InvalidDimensions of string

let dim m = m.dimensions

let matrix m = m.matrix

let fill m n x =
  {
    dimensions = (m, n);
    matrix = List.init m (fun i -> List.init n (fun j -> x));
  }

let zero m n = fill m n 0.0

let eye n =
  {
    dimensions = (n, n);
    matrix =
      List.init n (fun i -> List.init n 
        (fun j -> if i = j then 1.0 else 0.0));
  }

let transpose m =
  let rec helper = function
    | [] -> []
    | [] :: _ -> []
    | (a :: b) :: c ->
        (a :: List.map List.hd c) :: helper (b :: List.map List.tl c)
  in
  {
    dimensions = (snd m.dimensions, fst m.dimensions);
    matrix = helper m.matrix;
  }

let mult m1 m2 =
  if snd m1.dimensions != fst m2.dimensions then
    raise (InvalidDimensions "Dimensions do not match!")
  else
    let product v1 v2 =
      List.fold_left ( +. ) 0.0 (List.map2 (fun x1 x2 -> x1 *. x2) v1 v2)
    in
    let columns = (transpose m2).matrix in
    {
      dimensions = (fst m1.dimensions, snd m2.dimensions);
      matrix = List.map (fun row -> List.map (product row) columns) m1.matrix;
    }

let to_array m = Array.of_list (List.map Array.of_list m)

let to_list m = Array.to_list (Array.map Array.to_list m)

let swap m i j =
  let temp = m.(i) in
  m.(i) <- m.(j);
  m.(j) <- temp

let rref mat =
  let m = to_array mat.matrix in
  let lead = ref 0 in
  let rowCount = Array.length m in
  let columnCount = Array.length m.(0) in
  try
    for r = 0 to pred rowCount do
      if columnCount <= !lead then raise Exit;
      let i = ref r in
      while m.(!i).(!lead) = 0.0 do
        incr i;
        if rowCount = !i then (
          i := r;
          incr lead;
          if columnCount = !lead then raise Exit)
      done;
      swap m !i r;
      let le = m.(r).(!lead) in
      if le != 0.0 then m.(r) <- Array.map (fun x -> x /. le) m.(r);
      for i = 0 to pred rowCount do
        if i != r then
          let le = m.(i).(!lead) in
          m.(i) <- Array.mapi (fun i x -> x -. (le *. m.(r).(i))) m.(i)
      done;
      incr lead
    done;
    { mat with matrix = to_list m }
  with Exit -> { mat with matrix = to_list m }

let construct lst =
  let rec check = function
    | [] -> lst
    | [ h ] -> lst
    | a :: b :: c ->
        if List.length a != List.length b then
          raise
            (InvalidDimensions
               "Please ensure that the rows are of the same length!")
        else check (b :: c)
  in
  {
    dimensions = (List.length lst, List.length (List.hd lst));
    matrix = check lst;
  }

let lu_decomp mat =
  if fst mat.dimensions != snd mat.dimensions then
    raise (InvalidDimensions "Matrix is not square!")
  else
    let n = fst mat.dimensions in
    let m = mat.matrix |> to_array in
    let l = (zero n n).matrix |> to_array in
    let u = (zero n n).matrix |> to_array in
    for i = 0 to pred n do
      for k = i to pred n do
        let sum = ref 0.0 in
        for j = 0 to pred i do
          sum := !sum +. (l.(i).(j) *. u.(j).(k))
        done;
        u.(i).(k) <- m.(i).(k) -. !sum
      done;
      for k = i to pred n do
        if i = k then l.(i).(i) <- 1.0
        else
          let sum = ref 0.0 in
          for j = 0 to pred i do
            sum := !sum +. (l.(k).(j) *. u.(j).(i))
          done;
          l.(k).(i) <- (m.(k).(i) -. !sum) /. u.(i).(i)
      done
    done;
    ( { dimensions = (n, n); matrix = l |> to_list },
      { dimensions = (n, n); matrix = u |> to_list } )

let concat mat1 mat2 =
  if fst mat1.dimensions != fst mat2.dimensions then
    raise (InvalidDimensions "Number of rows do not match!")
  else
    let m1 = mat1.matrix in
    let m2 = mat2.matrix in
    let rec helper m1 m2 acc =
      match m1 with
      | [] ->
          {
            dimensions =
              (fst mat1.dimensions, 
                snd mat1.dimensions + snd mat2.dimensions);
            matrix = acc;
          }
      | h :: t -> (
          match m2 with
          | [] -> assert false
          | x :: xs -> helper t xs (acc @ [ h @ x ]))
    in
    helper m1 m2 
    []

let invert mat =
  if fst mat.dimensions != snd mat.dimensions then
    raise (InvalidDimensions "Matrix is not square!")
  else
    let n = fst mat.dimensions in
    let id = eye n in
    let m = concat mat id |> rref in
    let rec split n = function
      | [] -> assert false
      | x :: xs -> if n = 1 then xs else split (n - 1) xs
    in
    {
      dimensions = (n, n);
      matrix =
        m |> transpose |> matrix |> split n |> construct |> transpose |> matrix;
    }

let scale mat c =
  let m = mat.matrix in
  { mat with matrix = List.map (fun x -> List.map (fun x -> c *. x) x) m }

let rec det mat =
  if fst mat.dimensions != snd mat.dimensions then
    raise (InvalidDimensions "Matrix is not square!")
  else
    let m = to_array mat.matrix in
    let n = fst mat.dimensions in
    if n = 2 then (m.(0).(0) *. m.(1).(1)) -. (m.(0).(1) *. m.(1).(0))
    else
      let sum = ref 0.0 in
      for j = 0 to pred n do
        let m' = Array.make_matrix (n - 1) (n - 1) 0.0 in
        for i = 1 to pred n do
          m'.(i - 1) <-
            Array.append
              (Array.sub m.(i) 0 j)
              (Array.sub m.(i) 
                (if j + 1 < n then j + 1 else n - 1) (n - j - 1))
        done;
        sum :=
          !sum
          +. (-1.0 ** float_of_int (j mod 2))
             *. m.(0).(j)
             *. det { dimensions = (n - 1, n - 1); matrix = m' |> to_list }
      done;
      !sum

let magnitude vec =
  if fst vec.dimensions != 1 && snd vec.dimensions != 1 then
    raise (InvalidDimensions "Please ensure that matrix is a vector!")
  else
    let m =
      if snd vec.dimensions = 1 then vec |> transpose |> matrix 
      else vec.matrix
    in
    match m with
    | [] -> assert false
    | h :: t -> List.fold_left (fun acc x -> (x ** 2.) +. acc) 0. h ** 0.5

let normalize mat =
  if fst mat.dimensions != 1 && snd mat.dimensions != 1 then
    let det' = det mat in
    scale mat (1. /. det')
  else
    let m =
      if snd mat.dimensions = 1 then mat |> transpose |> matrix 
      else mat.matrix
    in
    match m with
    | [] -> assert false
    | h :: t ->
        let magn = magnitude mat in
        let res =
          {
            dimensions = (1, List.length h);
            matrix = [ List.map (fun x -> x /. magn) h ];
          }
        in
        if mat.dimensions = res.dimensions then res 
        else transpose res

(* Source: https://www.cs.cornell.edu/~bindel/class/cs6210-f09/lec26.pdf *)
let eigen mat dom =
  if fst mat.dimensions != snd mat.dimensions then
    raise (InvalidDimensions "Matrix is not square!")
  else
    let m = if dom = true then mat else invert mat in
    let n = fst mat.dimensions in
    let b_k =
      {
        dimensions = (1, n);
        matrix = [ List.init n (fun x -> Random.float 1.0) ];
      }
      |> transpose
    in
    let rec power_iteration b_k n =
      let b_k1 = mult m b_k in
      let b_k1_norm = normalize b_k1 in
      if n = 0 then (magnitude b_k1, b_k1_norm)
      else power_iteration b_k1_norm (n - 1)
    in
    power_iteration b_k 1000

let elem_pow mat r =
  let m = mat.matrix in
  { mat with matrix = List.map 
    (fun x -> List.map (fun x -> x ** r) x) m }

let pinv m =
  let m' = transpose m in
  let dot_inverse = m |> mult m' |> invert in
  mult dot_inverse m'

let op mat1 mat2 f =
  if
    fst mat1.dimensions != fst mat2.dimensions
    || snd mat1.dimensions != snd mat2.dimensions
  then raise (InvalidDimensions "Matrix dimensions do not match!")
  else
    let m1 = mat1.matrix in
    let m2 = mat2.matrix in
    let m' =
      List.map2 (fun l1 l2 -> List.map2 (fun x y -> f x y) l1 l2) m1 m2
    in
    { mat1 with matrix = m' }

let dot vec1 vec2 =
  if
    (fst vec1.dimensions != 1 && snd vec1.dimensions != 1)
    || (fst vec2.dimensions != 1 && snd vec2.dimensions != 1)
  then 
    raise (InvalidDimensions "Please use [mult] for matrix multiplication!")
  else
    let v1 =
      (if fst vec1.dimensions != 1 then transpose vec1 else vec1).matrix
    in
    let v2 =
      (if fst vec2.dimensions != 1 then transpose vec2 else vec2).matrix
    in
    List.fold_left2
      (fun acc l1 l2 ->
        List.fold_left2 (fun acc x1 x2 -> acc +. (x1 *. x2)) 0.0 l1 l2)
      0.0 v1 v2

let elem_f mat f =
  let m = mat.matrix in
  { mat with matrix = List.map (fun lst -> List.map (fun x -> f x) lst) m }
