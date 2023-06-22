// 17.1
let rec pow = function
    | (s: string, 1) -> s
    | (s: string, n: int) -> s + (pow (s, n-1))

// 17.2
let rec isIthChar = function
    | (s, n, c) when (n < 0) || (n >= String.length s) -> false
    | (s, n, c) -> s.[n] = c 


// 17.3
let occFromIth (s:string, n:int, c:char) =
    let rec inner = function
        | (it, count) when (String.length s) = it -> count
        | (it, count) when (it < 0) || (it > String.length s) -> count
        | (it, count) when (isIthChar (s, it, c)) -> inner (it+1, count+1)
        | (it, count) -> inner (it+1, count)
    inner(n, 0)
    

