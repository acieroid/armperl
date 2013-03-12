type t = (string, (int * Expression.value)) Hashtbl.t

let create () =
  Hashtbl.create 16

let add st string v =
  match Hashtbl.mem st string with
  | true -> ()
  | false ->
      Hashtbl.add st string (Hashtbl.length st, v)

let find st str =
  let id, v = Hashtbl.find st str in
  v

let get_addr st str =
  let id, v = Hashtbl.find st str in
  id * 4

let iter st f =
  let a = Array.make (Hashtbl.length st) (-1, "", Expression.Undef) in
  Hashtbl.iter (fun str (id, v) ->
    a.(id) <- (id, str, v)) st;
  Array.sort (fun (a, _, _) (b, _, _) -> compare a b) a;
  Array.iter (fun (id, str, v) -> f id str v) a
