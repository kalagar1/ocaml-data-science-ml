type point = { 
  tag : int; 
  pos : float * float 
}

let construct lst =
  let rec helper acc = function
    | [] -> acc
    | x :: xs -> helper ({ tag = 0; pos = x } :: acc) xs
  in
  Array.of_list (helper [] lst)

let to_list points =
  let points_lst = Array.to_list points in
  let rec helper acc = function
    | [] -> acc
    | x :: xs -> helper (x.pos :: acc) xs
  in
  helper [] points_lst

let classify points n max_iter =
  let centroids =
    Array.init n (fun i ->
        { tag = i; pos = (Random.float 20.0, Random.float 20.0) })
  in
  let dist p1 p2 =
    let x1 = fst p1.pos in
    let y1 = snd p1.pos in
    let x2 = fst p2.pos in
    let y2 = snd p2.pos in
    Float.sqrt (((x2 -. x1) ** 2.) +. ((y2 -. y1) ** 2.))
  in
  let find_nearest p =
    let min_tag = ref 0 in
    let min_dist = ref (dist p centroids.(0)) in
    for i = 1 to pred n do
      let temp = dist p centroids.(i) in
      if temp < !min_dist then min_tag := i;
      min_dist := temp
    done;
    !min_tag
  in
  for r = 0 to pred max_iter do
    for i = 0 to pred (Array.length points) do
      points.(i) <- { (points.(i)) with tag = find_nearest points.(i) }
    done;
    for i = 0 to pred n do
      let m =
        Array.fold_left
          (fun acc p -> if p.tag = i then acc + 1 else acc)
          0 points
      in
      let new_pos =
        Array.fold_left
          (fun acc p ->
            if p.tag = i then (fst acc +. fst p.pos, snd acc +. snd p.pos)
            else acc)
          (0.0, 0.0) points
      in
      if m != 0 then
        centroids.(i) <-
          {
            (centroids.(i)) with
            pos = (fst new_pos 
              /. float_of_int m, snd new_pos /. float_of_int m);
          }
    done
  done;
  (centroids, points)
