// 39.1
let rec rmodd = 
  function
  | [] -> []
  | [x] -> []
  | head :: need_element :: tail-> [need_element] @ rmodd tail   

// 39.2
let rec del_even = 
  function
  | [] -> []
  | head  :: tail when (head % 2 = 0) -> del_even tail
  | head :: tail when (head % 2 = 1) -> [head] @ del_even tail

// 39.3
let rec multiplicity x xs = 
  match x, xs with
  | x, [] -> 0
  | x, head :: tail when (head <> x) -> multiplicity x tail
  | x, head :: tail when (head = x) -> 1 + multiplicity x tail


// 39.4
let rec split = 
  let rec first_half = function
    | [] -> []
    | [x] -> [x]
    | head :: (second :: tail) -> head :: first_half tail
  
  let rec second_half = function
    | [] -> []
    | [x] -> []
    | head :: need_element :: tail-> [need_element] @ second_half tail   

  fun xs -> (first_half xs, second_half xs)

  // 39.5
let rec zip (xs1,xs2) = 
  match xs1,xs2 with
  | [], [] -> []
  | first_head :: first_tail, second_head::second_tail when (xs1.Length = xs2.Length) -> [(first_head, second_head)] @ zip (first_tail, second_tail)
  | first_head :: first_tail, second_head::second_tail when (xs1.Length <> xs2.Length) -> failwith "not same legth"
