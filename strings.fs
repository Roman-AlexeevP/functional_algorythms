// 17.1
let rec pow = function
    | (s, 0) -> ""
    | (s, n) when n < 0  -> ""
    | (s, n) -> s + pow (s, n - 1)


// 17.2
let rec isIthChar = function
    | (s, n, c) when n < 0 -> false
    | (s, n, c) when ((String.length s) <= n) -> false
    | (s, n, c) when (string s).[n] = c  -> true
    | (s, n, c) -> false


// 17.3
let rec occFromIth (str,number,char)=
    (let rec inner =
        function
        | (s, n, c, count) when ((String.length s) <= n) -> count
        | (s, n, c, count) when n < 0 -> 0
        | (s, n, c, count) when  ((string s).[n] = c) -> inner(s,n+1,c,count+1)
        | (s, n, c, count) when  ((string s).[n] <> c) -> inner(s,n+1,c,count)
        | _ -> 0

     inner (str, number, char, 0))
    


