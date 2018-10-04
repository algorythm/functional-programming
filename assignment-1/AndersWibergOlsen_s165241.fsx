// ############
// # Task 2.1 #
// ############

type Species     = string;;
open System.Threading
type Location    = string;;
type Time        = int;;
type Observation = Species * Location * Time;;

let os = [("Owl", "L1", 3); ("Sparrow", "L2", 4); ("Eagle", "L3", 5); ("Falcon", "L2", 7); ("Sparrow", "L1", 9); ("Eagle", "L1", 14)];;

let rec locationOf (s: Species) (os: Observation list) =
    match os with
    | (species, location, _) :: rest ->
        if s = species then location :: locationOf s rest
        else locationOf s rest
    | _ -> []
    ;;

locationOf "Owl"     os;; // Expected ["L1"]
locationOf "Sparrow" os;; // Expected ["L2"; "L1"]
locationOf "Eagle"   os;; // Expected ["L3"; "L1"]


// ############
// # Task 2.2 #
// ############

let occ = [("Owl", 1); ("Sparrow", 4); ("Eagle", 2)];;

type Count<'a when 'a:equality> = ('a*int) list;;
let rec insert a (occ: Count<Species>) : Count<Species> =
    match occ with
    | (species, count) :: rest when a = species -> (species, count + 1) :: rest
    | (species, count) :: rest                  -> (species, count)     :: insert a rest
    | [] -> [(a, 1)]
    ;;

insert "Owl" occ;;      // Expected [("Owl", 2); ("Sparrow", 4); ("Eagle", 2)]
insert "Lion" occ;;     // Expected [("Owl", 1); ("Sparrow", 4); ("Eagle", 2); ("Lion", 1)]
insert "Sparrow" occ;;  // Expected [("Owl", 1); ("Sparrow", 5); ("Eagle", 2)]


// ############
// # Task 2.3 #
// ############

let rec toCount (os: Observation list) : Count<Species> = 
    match os with
    | (s, _, _) :: tail -> insert s (toCount tail)
    | [] -> []
    ;;

toCount os;; // Expected: [("Eagle", 2); ("Sparrow", 2); ("Falcon", 1); ("Owl", 1)]


// ############
// # Task 2.4 #
// ############

type Interval = Time * Time;;

let rec select f (intv: Interval) (os : Observation list) =
    let (first, last) = intv
    match os with
    | (s, l, time) :: tail when time <= last && time >= first -> f (s, l, time) :: select f intv tail
    | _ :: tail -> select f intv tail
    | [] -> []
    ;;

select (fun (s, l, t) -> (s, l, t * 2)) (2, 6) os;; // Expected: [("Owl", "L1", 6); ("Sparrow", "L2", 8); ("Eagle", "L3", 10)]

// ############
// # Task 2.5 #
// ############

select (fun (s, l, _) -> (s, l)) (4, 9) os;;

// ############
// # Task 3.1 #
// ############

let locationOf2 (s: Species) (os: Observation list) =
    List.choose(fun (species, location, _) ->
        match species with
        | species when species = s -> Some(location)
        | _ -> None
    ) os
    ;;
    
locationOf2 "Owl"     os;; // Expected ["L1"]
locationOf2 "Sparrow" os;; // Expected ["L2"; "L1"]
locationOf2 "Eagle"   os;; // Expected ["L3"; "L1"]

// ############
// # Task 3.3 #
// ############

let toCount2 (os: Observation list) =
    List.fold(fun clist (s, _, _) -> insert s clist) [] os
    ;;

toCount2 os;; // Expected: [("Eagle", 2); ("Sparrow", 2); ("Falcon", 1); ("Owl", 1)]
