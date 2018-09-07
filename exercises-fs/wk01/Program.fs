// Learn more about F# at http://fsharp.org

open System

// Exercise 1
let rec ex1_sum n = 
    match n with
    | 0 -> 0
    | n when n >= 0 -> n + ex1_sum(n - 1)
    | _ -> failwith "Value must be positive!"

// Exercise 2
let rec ex2_sum(m, n) = 
    match (m, n) with
    | (_, 0) -> m
    | (m, n) when m >= 0 && n >= 0 -> m + n + ex2_sum(m, n - 1)
    | (_, _) -> failwith "m and n shall be greater than or equal to 0"

// let rec ex3_pascal(n, k) =
//     match(n, k) with
//     | (_, 0) -> 1
//     | (_, n) -> 1
//     | (n, k) when n <> 0 && k <> 0 && n > k -> ()

[<EntryPoint>]
let main argv =
    printfn "Exercise 1"
    let val1_1 = ex1_sum 4
    let val1_2 = ex1_sum 6
    let val1_3 = ex1_sum 10
    printfn "ex1_sum(4) = %i" val1_1
    printfn "ex1_sum(6) = %i" val1_2
    printfn "ex1_sum(10) = %i" val1_3
    printfn ""

    printfn "Exercise 2"
    let val2_1 = ex2_sum(3,2)
    let val2_2 = ex2_sum(4, 5)
    let val2_3 = ex2_sum(10, 10)
    printfn "sum(3, 2) = %i" val2_1
    printfn "sum(4, 5) = %i" val2_2
    printfn "sum(10, 10) = %i" val2_3

    0 // return an integer exit code
