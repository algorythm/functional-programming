type Title    = string;;
type Document = Title * Element list
and Element   = Par of string | Sec of Document;;

let s1   = ("Background", [Par "Bla"]);;
let s21  = ("Expressions", [Sec("Arithmetical Expressions", [Par "Bla"]);
                         Sec("Boolean Expressions", [Par "Bla"])]);;
let s222 = ("Switch statements", [Par "Bla"]);;
let s223 = ("Repeat statements", [Par "Bla"]);;
let s22  = ("Statements",[Sec("Basics", [Par "Bla"]) ; Sec s222; Sec s223]);;
let s23  = ("Programs", [Par "Bla"]);;
let s2   = ("The Programming Language", [Sec s21; Sec s22; Sec s23]);;
let s3   = ("Tasks", [Sec("Frontend", [Par "Bla"]);
                   Sec("Backend", [Par "Bla"])]);;
let doc  = ("Compiler project", [Par "Bla"; Sec s1; Sec s2; Sec s3]);;

// ##########
// # Task 1 #
// ##########

let rec noOfSecs = function
    | _, [] -> 0
    | _, secList -> noOfSecsList secList
and noOfSecsList = function
    | [] -> 0
    | [Sec _] -> 1
    | Sec(_, secList) :: secRest -> noOfSecsList secList + noOfSecsList secRest + 1
    // | Sec(_, secList) :: secRest -> noOfSecsList secRest + noOfSecsList secList + 1
    | _ :: rest -> noOfSecsList rest

let testElement1 = ("Test", [ Sec("Test Section", [Par "This is a part"]) ])
let testElement2 = ("2 Sections", [ Sec("Section 1", []); Sec("Section 2", []) ])
let testElement3 = ("3 Sections (2 sec, 1 sub)", [ Sec("Section 1", []); Sec("Section 2", [ Sec("Subsection", []) ]) ])
let testElement4 = ("1 Section, not first", [Par "Bla"; Sec("Section", [])])
let testElement5 = ("2 Sectinos (1 sec, 1 sub)", [Sec("Section", [Sec("Subsection", [])])])


noOfSecs testElement1 // 1 -- tests 1 section
noOfSecs testElement2 // 2 -- tests 2 sections
noOfSecs testElement3 // 3 -- tests 2 sections, 1 subsection
noOfSecs testElement4 // 1 -- tests 1 section, but not as the first
noOfSecs testElement5 // 2 -- tests 1 section, 1 subsection
noOfSecs doc // 13

let test1 = noOfSecs testElement1 = 1
let test2 = noOfSecs testElement2 = 2
let test3 = noOfSecs testElement3 = 3
let test4 = noOfSecs testElement4 = 1
let test5 = noOfSecs testElement5 = 2
let test6 = noOfSecs doc = 13
