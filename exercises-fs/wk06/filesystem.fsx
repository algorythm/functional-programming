type FileSys = Element list
and Element  = | File of string * string // Filename * Extension
               | Dir of string * FileSys;;
let d1 = Dir("d1",[File("a1","java");
                Dir("d2", [File("a2","fsx");
                           Dir("d3", [File("a3","fs")])]);
                File("a4","fsx");
                Dir("d3", [File("a5","pdf")])]);;

let rec namesFileSys = function
    | [] -> []
    | elm::tail -> namesElement elm @ namesFileSys tail
    and namesElement = function
    | File(name, ext) as file -> [(name + "." + ext)]
    | Dir(_, []) -> []
    | Dir(name, files) -> name::namesFileSys files

namesElement d1
