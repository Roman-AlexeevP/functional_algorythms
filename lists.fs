// 34.1
let rec upto n = 
  match n with
  | 1 -> [1]
  | n -> upto (n-1) @ [n]

// 34.2
let rec dnto = 
  function
  | 1 -> [1]
  | n -> n :: dnto (n-1)

// 34.3
let rec evenn = 
  function 
  | 1 -> [0]
  | n when n <= 0 -> []
  | n -> evenn(n-1) @ [(n-1)*2]