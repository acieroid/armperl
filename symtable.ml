module StringMap = Map.Make(String)

type t = Expression.value StringMap.t

let empty = StringMap.empty

let find symtable name =
  try 
    Some (StringMap.find name symtable)
  with
    Not_found -> None

let add symtable name value = 
  StringMap.add name value symtable
