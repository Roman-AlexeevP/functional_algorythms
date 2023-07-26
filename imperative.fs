// 47.4.1
let f n = 
    let i = ref n
    if n = 0 then ignore 1
    else if n = 1 then ignore 1
    let mutable sum = 1
    let fib x = sum <- sum * x
    List.iter fib [1..n]
    sum

// 47.4.2
let fibo n =
    if n = 0 then ignore 0
    else if n = 1 then ignore 1

    let mutable n2 = ref 0
    let mutable n1 = ref 1
    let mutable index = ref 2
    let mutable result = ref 0
    while !index <= n do
        result := !n2 + !n1
        n2 := !n1
        n1 := !result
        index := !index + 1
    !result

