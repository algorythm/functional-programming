# Task 1.1

```fsharp
let rec foldBack f xlst e =
    match xlst with
    | x::xs -> f x (foldBack f xs e)
    | [] -> e;;
val foldBack:(’a -> ’b -> ’b) -> ’a list -> ’b -> ’b
```

`foldBack` takes 3 arguments: `f`, `xlst` `e` (`'a`, `'b` and `'c`) which returns `'d`:

```fsharp
val foldBack: 'a -> 'b -> 'c -> 'd
```

We see that the return is `e` which has type `'c` meaing we now have:

```fsharp
val foldBack: 'a -> 'b -> 'c -> 'c
```

That means that the `f` has the type of:

```fsharp
val f: 'b -> 'c -> 'c
```

Meaning `f` takes 2 arguments: `xlst: 'b list` and `e: 'c`.

Finally `foldBack` returns `'c`. Which leads to

```fsharp
val foldBack:(’a -> ’b -> ’b) -> ’a list -> ’b -> ’b
```
