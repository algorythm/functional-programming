// Learn more about F# at http://fsharp.org

open System

// Exercise 1
let rec sum n = 
    match n with
    | 0 -> 0
    | n when n >= 0 -> n + sum(n - 1)
    | _ -> failwith "Value must be positive!"

[<EntryPoint>]
let main argv =
    printfn "Exercise 1"
    let val1 = sum 4
    let val2 = sum 6
    let val3 = sum 10
    printfn "sum(4) = %i" val1
    printfn "sum(6) = %i" val2
    printfn "sum(10) = %i" val3

    0 // return an integer exit code

