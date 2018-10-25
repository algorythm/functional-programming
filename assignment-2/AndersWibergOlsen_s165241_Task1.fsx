let test expected actual = System.Console.WriteLine("expected: {0}\n  actual: {1}", expected, actual)

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
    | _ :: tlist -> isGroundTermList tlist

// Expected result: false
isGround(V("Should return false"));; 
// Expected result: true
isGround(C(0))
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
    | elm :: rest -> toString elm + "," + toStringTermList rest

// Expected result: f3(f2(1,2),f1(x),f0())
toString(F("f3",[F("f2",[C 1; C 2]); F("f1",[V "x"]); F("f0",[])]));;

// ############
// # Task 2.3 #
// ############

// I don't really like the wording of this task. As I understand it:
//     when calling:    subst x t' t
//     then every occurence of t'   in   t  should be replaced by x such that
//     subst "x" C(0) V("x")    return   C 0
//     subst "test" C(2) F("test", [C 1; V("test")]) return F("test", [C 1; C 2])
//     subst "test" C(2) F("beep", [C 1; V("test")]) return F("beep", [C 1; C 2])

let rec subst x tm = function
    | V v when v = x -> tm 
    | F (f, flist) -> F(f, substFList x tm flist)
    | other -> other
and substFList x tm = function
    | [] -> []
    | [elm] -> [subst x tm elm]
    | elm :: rest -> subst x tm elm :: substFList x tm rest

let t0 = C 0
let t1 = V "x"
// Expected result: C 0
subst "x" t0 t1
// Expected result: V ("x")
subst "y" t0 t1
let t2 = F("test", [C 1; V "test"])
let t3 = F("test", [C 1; V "beep"])
// Expected result: F("test", [C 1; C 0])
subst "test" t0 t2
// Expected result: F("test", [C 1; V "Beep"])
subst "test" t0 t3