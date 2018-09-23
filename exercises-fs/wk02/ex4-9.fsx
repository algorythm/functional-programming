// Book, exercise 4.9, page 89
let rec zip = function
    | ([], []) -> []
    | (x::xs, y::ys) when xs.Length = ys.Length -> (x, y) :: zip(xs, ys)
    | _ -> failwith("Lists not equal lengths")

zip([1; 3; 5], [2; 4; 6]) // Expected: [(1; 2); (3; 4); (5; 6)]
zip([1; 2; 3], [4; 5]) // Expected: Exception
