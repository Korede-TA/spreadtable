module Caml = struct
  module String = String
  module List = String
  module Array = Array
end

include Tablecloth
(*
include (
  Tablecloth :
    module type of Tablecloth
    (* with module StrSet := Tablecloth.StrSet *)
    (*  and module IntSet := Tablecloth.IntSet *)
    (*  and module StrDict := Tablecloth.StrDict *)
    with module Option := Tablecloth.Option
    (*  and module Result := Tablecloth.Result *)
    (*  and module List := Tablecloth.List *) )
*)


let optValueExn (value : 'a option) : 'a =
  match value with Some v -> v | None -> raise Not_found

let classes (classes : string list) =
  Tea.Html2.Attributes.classList (List.map ~f:(fun c -> (c, true)) classes)

(*
let indexRange start stop = Array.range ?from:start stop
*)
let rec indexRange start stop = 
  if start != stop then 
    [start+1] @ (indexRange (start+1) stop)
  else [] 

  (*
module Option = struct
  include Tablecloth.Option

  let exec ~(f : 'a -> unit) (v : 'a option) : unit =
    match v with Some v -> f v | None -> ()


end
*)
