// 16.1
let notDivisible (n, m) = m % n = 0

// 16.2
let rec prime n =  
    let rec recPrime i = 
        i >  n / 2  || (n % i <> 0  && recPrime(i + 1))
    recPrime 2