type t = (string, int) Hashtbl.t

let create () =
  Hashtbl.create 16

let iter st f =
  let a = Array.make (Hashtbl.length st) (-1, "") in
  Hashtbl.iter (fun str id ->
    a.(id) <- (id, str)) st;
  Array.sort (fun (a, _) (b, _) -> compare a b) a;
  Array.iter (fun (id, str) -> f id str) a

let add st string =
  match Hashtbl.mem st string with
  | true -> ()
  | false ->
      Hashtbl.add st string (Hashtbl.length st)

let get_addr st string =
  try
    let id = Hashtbl.find st string in
    id * 4
  with
    Not_found -> failwith ("String is not in the string table: " ^ string)
