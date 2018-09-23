// Book, exercise 4.8, page 89

let rec split = function
    | [] | [_] -> ([], [])
    | x0::x1::rest ->
        let (x2, x3) = split rest
        (x0::x2, x1::x3);;

split([1; 2; 3; 4]) // Expected ([1; 3], [2; 4])
split([5; 2; 3; 9; 6; 7]) // Expected ([5; 3; 6], [2; 9])
split([1..8])
