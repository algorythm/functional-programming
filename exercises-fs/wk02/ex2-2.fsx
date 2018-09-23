// Book, exercise 2.2, page 39
let rec pow(s, n) = 
    match n with
    | 0 -> ""
    | 1 -> s
    | _ -> s + " * " + pow(s, n - 1)

pow("HEJ", 0)
pow("HEJ", 1)
pow("HEJ", 2)
pow("HEJ", 3)
pow("HEJ", 4)
pow("HEJ", 5)
