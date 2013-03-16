open Expression

module StringMap = Map.Make(String)


type perl_function = {
    name: string;
    defined: bool;
    args: string list;
    body: expr list;
  }

type t = {
    variables: value StringMap.t;
    functions: perl_function StringMap.t;
    globals: (string, Expression.value) Hashtbl.t;
  }

let empty () = {
  variables=StringMap.empty;
  functions=StringMap.empty;
  globals=Hashtbl.create 100;
}

let find_var symtable name =
  try 
    StringMap.find name symtable.variables
  with
    Not_found -> Undef

let set_var symtable name value = 
  {symtable with variables=(StringMap.add name value symtable.variables)}


let find_fun symtable name =
  try
    StringMap.find name symtable.functions
  with
    Not_found -> {name=name; defined=false; args=[]; body=[]}

let set_fun symtable name value =
  {symtable with functions=(StringMap.add name value symtable.functions)}

let find_global symtable name =
  try
    Hashtbl.find symtable.globals name
  with
    Not_found -> Undef

let set_global symtable name value =
  Hashtbl.replace symtable.globals name value;
  symtable

