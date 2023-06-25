// 20.3.1
let vat n x = x + (x * (float n) * 0.01)

// 20.3.2
let unvat n x = x * (100.0 / (100.0 + (float n)))

// 20.3.3
let rec min f = 
    (let rec it n = 
        if f (n) = 0 then
            n
        else
            it (n+1)
    it 1
            )