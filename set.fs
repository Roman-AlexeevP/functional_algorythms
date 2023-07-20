let rec produceSet xs = 
    match xs with
      | [] -> [[]]
      | head::tail -> List.fold (fun head1 tail1 -> (head::tail1)::tail1::head1) [] (produceSet tail)

// 42.3
let rec allSubsets n k =
    
    Set.filter (fun xs -> Set.count xs = k) (Set.ofList (List.map Set.ofList (produceSet [1 .. n])))

