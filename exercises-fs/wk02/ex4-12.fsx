// Book, exercise 4.12, page 90
let p x = x > 0
p 3
p -2
let rec sum(p, xs) =
    match xs with
    | [] -> 0
    | x::xs when p x -> x + sum(p, xs)
    | _::xs -> sum(p ,xs)

let testList = [-1; -3; -7; 1; 10; 2; 5]

sum((fun x -> x > 0), testList) // Expected 18
sum((fun x -> x < 0), testList) // Expected -11
