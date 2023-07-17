// 41.4.1
let list_filter f xs = 
  List.foldBack (fun x acc -> if f x then x::acc else acc) xs []
  
// 41.4.2
let sum (p, xs) = 
  List.foldBack (fun x acc -> if p x then x+acc else acc) xs 0

// 41.4.3
let revrev = function
  | [] -> []
  | xs -> List.fold (fun acc x -> List.rev x::acc) [] xs
