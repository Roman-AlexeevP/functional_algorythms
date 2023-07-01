let (.+.) x y =
    let (goldLeft, silverLeft, copperLeft) = x
    let (goldRight, silverRight, copperRight) = y
    let resultCopper = (copperLeft+copperRight)%12
    let resultSilver = (silverLeft+silverRight  + (copperLeft+copperRight)/12)%20
    let resultGold = (goldLeft+ goldRight+ (silverLeft+silverRight  + (copperLeft+copperRight)/12)/20)
    (resultGold,resultSilver,resultCopper)


let (.-.) x y =
     let (goldLeft, silverLeft, copperLeft) = x
     let z = (-goldLeft,-silverLeft,-copperLeft)
     z .+. y

let (.+) x y =
    let (a, b) = x
    let (c, d) = y
    (a + c, b + d)

let (.*) x y =
    let (a, b) = x
    let (c, d) = y
    (a * c - b * d, b * c + a * d)

let (.-) x y =
    let (c, d) = y
    let z = (-c,-d)
    x .+ z

let (./) x y =
    let (c, d) = y
    let z = (c/(c*c+d*d),-d/(c*c+d*d))
    x .* z
