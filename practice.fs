// 40.1
let rec sum (p, xs) = 
  match xs with 
  | [] -> 0
  | [n] when p n -> n
  | head :: tail when (p head)-> head + sum(p, tail)
  | head :: tail -> 0+sum(p, tail)

// 40.2.1
let rec count (xs, n) =
    match xs with
    | [] -> 0
    | [x] when x = n -> 1
    | [x] -> 0
    | head :: tail when head > n -> 0
    | head :: tail when head = n -> 1 + count(tail, n)
    | head :: tail -> count(tail, n)

// 40.2.2
let rec insert (xs, n) = 
  match xs with
  | [] -> [n]
  | head::tail when head <= n -> [head] @ insert(tail, n)
  | head::tail when head > n -> [n] @ head :: tail
  | _ -> []

// 40.2.3
let rec intersect (xs1, xs2) = 
  match xs1, xs2 with
  | head1::tail1, head2::tail2 when head1 = head2 -> head1 :: intersect(tail1, tail2)
  | head1:: _, head2::tail2 when head1 > head2 -> intersect(xs1, tail2)
  | head1:: tail1, head2::_ when head1  < head2 -> intersect(tail1, xs2)
  | [x], [n] when x=n -> [x]
  | _ -> []

  // 40.2.4.
let rec plus (xs1, xs2) =
    match (xs1, xs2) with
    | (head1 :: tail1, head2 :: _) when head1 < head2 -> [ head1 ] @ plus (tail1, xs2)
    | (head1 :: _, head2 :: tail2) when head1 > head2 -> [ head2 ] @ plus (xs1, tail2)
    | (head1 :: tail1, head2 :: tail2) when head1 = head2 -> [ head1 ] @ [ head2 ] @ plus (tail1, tail2)
    | (xs1, [ ]) -> xs1
    | ([], xs2) -> xs2
    | ([ x ], [ y ]) when x = y-> [ x ] @ [ y ]
    | ([ x ], [ y ]) when x > y-> [ y ] @ [ x ]
    | ([ x ], [ y ]) when x < y -> [ x ] @ [ y ]
    | _ -> []

// 40.2.5.
let rec minus (xs1, xs2) =
    match (xs1, xs2) with
    | (head1 :: tail1, head2 :: _) when head1 < head2 -> [ head1 ] @ minus (tail1, xs2)
    | (head1 :: _, head2 :: tail2) when head1 > head2 ->  minus (xs1, tail2)
    | (head1 :: tail1, head2 :: tail2) when head1 = head2 -> minus (tail1, tail2)
    | (xs1, [ ]) -> xs1
    | ([], xs2) -> []
    | ([ x ], [ y ]) when x = y-> [ ]
    | ([ x ], [ y ]) when x <> y-> [ x ]
    | _ -> []



let rec iterator (xs, min) =
        match xs with
        | head::tail when head > min  -> iterator(tail,min)
        | head::tail when head <= min  -> iterator(tail,head)
        | [  ] -> Some min
        | _ -> Some min

// 40.3.1.
let rec smallest = fun xs ->
    let head :: tail = xs
    iterator(tail,head)


//40.3.2. 
let rec delete (n, xs) =
    match xs with
    | [] -> []
    | head :: tail when head <> n -> [head] @ delete(n,tail)
    | head :: tail when head = n -> tail
    | _ -> []



// 40.3.3.
let rec sort = fun xs ->
    let rec iterate (list: 'b list) result =
        if(list.Length=0) then result
        else
            let min = smallest list
            let tail = delete(min.Value,list)
            iterate tail (result @ [min.Value])

    iterate xs []



// 40.4.
let rec revrev = fun (xs: list<list<int>>) ->
   match xs with
   | [] -> []
   | [ x ] -> [ List.rev x ]
   | head :: tail -> revrev tail @ [ List.rev head ]