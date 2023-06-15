let rec fibo = function
    | 0 -> 0
    | 1 -> 1 
    | n -> fibo(n-1) + fibo (n - 2)

let rec sum = function
    | 0 -> 0
    | 1 -> 1
    | n -> n + sum(n-1)

let rec sum2 = function
    | (m: int,0) -> m
    | (m,n) -> m + n + sum2 (m, n-1)