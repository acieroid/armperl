exception Already_defined

type t = {
    (** The globals with an identifier to find their addresses *)
    globals: (string, int) Hashtbl.t;
    (** The local variables *)
    mutable locals: string list option;
    (** The functions with their arity *)
    funs: (string, int) Hashtbl.t;
  }

let create () =
  {globals=Hashtbl.create 16; locals=None; funs=Hashtbl.create 16}

let add_global st string =
  match Hashtbl.mem st.globals string with
  | true -> ()
  | false ->
      Hashtbl.add st.globals string (Hashtbl.length st.globals)

let get_global_addr st str =
  let id = Hashtbl.find st.globals str in
  id * 4

let iter_globals st f =
  let a = Array.make (Hashtbl.length st.globals) (-1, "") in
  Hashtbl.iter (fun str id ->
    a.(id) <- (id, str)) st.globals;
  Array.sort (fun (a, _) (b, _) -> compare a b) a;
  Array.iter (fun (id, str) -> f id str) a

let set_locals st vars =
  st.locals <- Some vars

let get_locals st =
  st.locals

let clear_locals st =
  st.locals <- None

let is_local st var =
  match st.locals with
  | None -> false
  | Some vars -> List.mem var vars

let add_var st var =
  if is_local st var then
    ()
  else
    add_global st var

let add_fun st fname arity =
  match Hashtbl.mem st.funs fname with
  | true -> raise Already_defined
  | false -> Hashtbl.add st.funs fname arity

let get_fun st fname =
  Hashtbl.find st.funs fname
