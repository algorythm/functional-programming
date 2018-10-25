open System.Security
// ############
// # Task 2.1 #
// ############

type Term = | V of string
            | C of int
            | F of string * Term list

let rec isGround = function
    | V _ -> false
    | C _ -> true
    | F (_, tlist) -> isGroundTermList tlist
and isGroundTermList = function
    | [] -> true
    | [t] -> isGround t
    | _::tlist -> isGroundTermList tlist

// Expected result: false
isGround(V("Should return false"));; 
// Expected result: true
isGround(C(0));; 
// Expected result: false
isGround(F("Should return false", [C(0); V"Should return false"]))
// Expected result: true
isGround(F("Should return true", [])) 
// Expected result: true
isGround(F("Test", [C(0)])) 
// Expected result: false
isGround(F("", [F("", [V"Should return false"])])) 

// ############
// # Task 2.2 #
// ############

let rec toString = function
    | V v -> v
    | C c -> string c
    | F (f, fs) -> f + "(" + toStringTermList fs + ")"
and toStringTermList = function
    | [] -> ""
    | [elm] -> toString elm
    | elm::rest -> toString elm + "," + toStringTermList rest

// Expected result: f3(f2(1,2),f1(x),f0())
toString(F("f3",[F("f2",[C 1; C 2]); F("f1",[V "x"]); F("f0",[])]));;

// ############
// # Task 2.3 #
// ############

// I don't really like the formulation of this task. As I understand it:
//     when calling:    subst x t' t
//     then every occurence of t'   in   t  should be replaced by x such that
//     subst "x" "y" V("x")    return   V("y")
//     subst 1 2 F("test", [C 1; C 2]) return F("test", [C 2; C 2])
let rec subst x tm t = function
