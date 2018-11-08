open System.Xml
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
    | title, head :: tail -> determineElement head + noOfSecs(title, tail)
and determineElement = function
    | Par _ -> 0
    | Sec(title, tail) -> noOfSecs(title, tail) + 1

let testElement1 = ("Test", [ Sec("Test Section", [Par "This is a part"]) ])
let testElement2 = ("2 Sections", [ Sec("Section 1", []); Sec("Section 2", []) ])
let testElement3 = ("3 Sections (2 sec, 1 sub)", [ Sec("Section 1", []); Sec("Section 2", [ Sec("Subsection", []) ]) ])
let testElement4 = ("1 Section, not first", [Par "Bla"; Sec("Section", [])])
let testElement5 = ("2 Sectinos (1 sec, 1 sub)", [Sec("Section", [Sec("Subsection", [])])])

let test1 = noOfSecs testElement1 = 1
let test2 = noOfSecs testElement2 = 2
let test3 = noOfSecs testElement3 = 3
let test4 = noOfSecs testElement4 = 1
let test5 = noOfSecs testElement5 = 2
let test6 = noOfSecs doc = 13

// ##########
// # Task 2 #
// ##########

let rec sizeOfDoc = function 
    | title: Title, [] -> title.Length
    | title, head :: tail -> countChars head + sizeOfDoc(title, tail)
and countChars = function
    | Par p -> p.Length
    | Sec(title, tail) -> sizeOfDoc(title, tail)
    
let test7 = sizeOfDoc testElement1 = 30
let test8 = sizeOfDoc testElement3 = 53
let test9 = sizeOfDoc doc = 212

// ##########
// # Task 3 #
// ##########

let rec titlesInDoc = function
    | title: Title, [] -> []
    | title, head :: tail -> getTitle head @ titlesInDoc(title, tail)
and getTitle = function
    | Par _ -> []
    | Sec(title, tail) -> title :: titlesInDoc(title, tail)

let titles = ["Background"; "The Programming Language"; "Expressions";
   "Arithmetical Expressions"; "Boolean Expressions"; "Statements"; "Basics";
   "Switch statements"; "Repeat statements"; "Programs"; "Tasks"; "Frontend";
   "Backend"]
let test10 = titlesInDoc doc = titles
let test11 = (titlesInDoc doc).Length = titles.Length

// ##########
// # Task 4 #
// ##########

type Prefix = int list;;
type ToC    = (Prefix * Title) list;;

// toc: Document -> ToC
// let rec toc = function
//     | elm : Document -> ""
