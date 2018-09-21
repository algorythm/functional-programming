// Book, exercise 2.1, page 39
let f = function
    | n when (n % 2 = 0 || n % 3 = 0) && n % 5 <> 0 -> true
    | _ -> false

f(2)  // expected true
f(3)  // expected true
f(5)  // expected false
f(6)  // expected true
f(8)  // expected true
f(10) // expected false
