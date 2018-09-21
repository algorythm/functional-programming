// Exercise 1
let rec ex1Sum n = 
    match n with
    | 0 -> 0
    | n when n >= 0 -> n + ex1Sum(n - 1)
    | _ -> failwith "Value must be positive!"
let val1_1 = ex1Sum 4 // Expected 10
let val1_2 = ex1Sum 6 // Expected 21
let val1_3 = ex1Sum 10 // Expected 55

// Exercise 2
let rec ex2Sum(m, n) = 
    match (m, n) with
    | (_, 0) -> m
    | (m, n) when m >= 0 && n >= 0 -> m + n + ex2Sum(m, n - 1)
    | (_, _) -> failwith "m and n shall be greater than or equal to 0"
ex2Sum(3,2) // Expected 12
ex2Sum(4, 5) // Expected 39
ex2Sum(10, 10) // Expected 165

let rec ex3Pascal(n, k) =
    match n, k with
    | _, 0 -> 1
    | n, k when n = k -> 1
    | n, k when n <> 0 && k <> 0 && n > k -> ex3Pascal(n-1, k-1) + ex3Pascal(n-1, k)
ex3Pascal(0,0) // Expected 1
ex3Pascal(1,0) // Expected 1
ex3Pascal(2, 1) // Expected 2
ex3Pascal(3, 1) // Expected 3
ex3Pascal(4, 0) // Expected 1
ex3Pascal(4, 1) // Expected 4
ex3Pascal(4, 2) // Expected 6
ex3Pascal(4, 3) // Expected 4
ex3Pascal(4, 4) // Expected 1

// Exercise 4
let rec ex4Multiplicity(x, ys) =
    match ys with
    | [] -> 0
    | y::tail when x = y -> 1 + ex4Multiplicity(x, tail)
    | _::tail -> ex4Multiplicity(x, tail)
let ex4List = [2; 4; 2; 10; 1; 2];;
ex4Multiplicity(0, ex4List) // Expected 0
ex4Multiplicity(2, ex4List) // Expected 3
ex4Multiplicity(4, ex4List) // Expected 1

// Exercise 5
let rec mulC(x, ys) = 
    match ys with
    | [] -> ys
    | y::tail -> (x * y) :: mulC(x, tail)
mulC(2, [4; 10; 1]) // Expected [8; 20; 2]
mulC(0, [1;2;3;4]) // Expected [0; 0; 0; 0]

// Exercise 6
let rec addE(xs, ys) =
    match xs, ys with
    | _, [] -> xs
    | [], _ -> ys
    | x::xtail, y::ytail -> (x + y) :: addE(xtail, ytail)

addE([1;2;3], [4;5;6]) // Expected [5; 7; 9]
addE([1;2], [3;4;5;6]) // Expected [4; 6; 5; 6]
addE([1;2;3;4], [5;6]) // Expected [6; 8; 3; 4]

let rec mul(xs, ys) =
    match xs, ys with
    | [], _ -> []
    | x::tail, _ -> addE(mulC(x, ys), mul(tail, 0::ys))
 
mul([ 2; 3; 0; 1 ], [ 1; 2; 3 ]) // Expected [2, 7, 12, 10, 2, 3]
mul([1; 2; 3],[3; 2; 1]) // Expected [3, 8, 14, 8, 3]
