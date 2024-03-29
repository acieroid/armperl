type ('a,'b) either = Left of 'a | Right of 'b

let implode l =
  let s = String.create (List.length l) in
  let rec f n = function
    | x :: xs -> s.[n] <- x; f (n+1) xs
    | [] ->  s
  in f 0 l

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let index_of x l =
  let rec index_of' i = function
    | [] -> raise Not_found
    | (hd::_) when hd = x -> i
    | (_::tl) -> index_of' (i+1) tl
  in
  index_of' 0 l
