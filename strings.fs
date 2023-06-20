// 17.1
let rec pow = function
    | (s: string, 1) -> s
    | (s: string, n: int) -> s + (pow (s, n-1))

// 17.2
let rec isIthChar (s:string, n:int, c: char) = s.[n] = c 

let recAcc = function
        | (s:string, n, c) when n >= s.Length || n < 0  -> 0
        | (s, n, c) when isIthChar (s,n,c) -> 1
        | _ -> 0
  

// 17.3
let rec occFromIth  = function
    | (s:string, n, c) when n >= s.Length || n < 0  -> 0
    | (s, n, c)  -> recAcc(s,n,c) + occFromIth(s, n+1, c)

