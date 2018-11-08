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
    | _: Title, [] -> []
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
//         | title: Title, [] -> [[], title] : ToC
//         | title, head :: tail -> gt head @ toc(title, tail)
//     and gt = function
//         | Par _ -> []
//         | Sec(title, tail) -> toc(title, tail)


let toc = 
    let rec iterDoc (cnt: int) (pfx: Prefix) = function
        | (_, []) -> []
        | (title, head :: tail) -> elms cnt pfx head @ iterDoc (cnt+1) pfx (title, tail)
    and elms cnt pfx = function
        | Par _ -> []
        | Sec (title, lst) -> (pfx@[cnt], title) :: iterDoc 1 (pfx@[cnt]) (title, lst)
    function 
    | (title, elements) -> ([], title) :: iterDoc 0 [] (title, elements)

let output = [([], "Compiler project"); ([1], "Background");
   ([2], "The Programming Language"); ([2; 1], "Expressions");
   ([2; 1; 1], "Arithmetical Expressions"); ([2; 1; 2], "Boolean Expressions");
   ([2; 2], "Statements"); ([2; 2; 1], "Basics");
   ([2; 2; 2], "Switch statements"); ([2; 2; 3], "Repeat statements");
   ([2; 3], "Programs"); ([3], "Tasks"); ([3; 1], "Frontend");
   ([3; 2], "Backend")]
let test12 = toc doc = output

// The method works, though the arguments and return type is 
//   Title * Element list -> (int list * Title) list
// I have tried a bunch of different things, but can't get it
// to say 
//   Document -> ToC
//
// Document is Title * Element list and ToC is (int list * Title) list
// but, I can't figure out how to make it say the right thing.
