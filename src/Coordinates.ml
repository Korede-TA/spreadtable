open Prelude

type t = (int * int) list

(*type t =
  { prefix : (int * int) list (* in the case of nested *)
  ; head : (int * int) }
  *)

(* standard base coordinates *)
let coord_ROOT = [(1, 1)]

let coord_META = [(2, 1)]

exception InvalidCoord of (string * t)

let toList (c : t) : (int * int) list = c

type dimensionIndex = (int * int) list * int

let ppDimensionIndex (d : dimensionIndex) : string =
  let p, d = d in
  String.join ~sep:";"
    ( List.map (fun (a, b) -> string_of_int a ^ ", " ^ string_of_int b) p
    @ [string_of_int d] )

let fullRow (c : t) : dimensionIndex =
  match c with
  | [] -> raise (InvalidCoord ("empty coord", c))
  | head :: prefix -> (prefix, Tuple2.first head)

let fullCol (c : t) : dimensionIndex =
  match c with
  | [] -> raise (InvalidCoord ("empty coord", c))
  | head :: prefix -> (prefix, Tuple2.second head)

let row r = fullRow r |> Tuple2.second

let col c = fullCol c |> Tuple2.second

let parent (c : t) : t option =
  match c with _ :: prefix -> Some prefix | [] -> None

let rec showCol (i : int) : string =
  let alphaOffset = 64 in
  let normalizedI = if i == 26 then 26 else i mod 26 in
  let baseChar =
    normalizedI + alphaOffset |> Char.fromCode |> String.fromChar
  in
  baseChar ^ if i > 26 then showCol (i - 26) else ""

let showRow (i : int) : string = string_of_int i

let showRC_ (r, c) = (c |> showCol) ^ (r |> showRow)

(*
let show (c : t) : string =
  let (prefix, r) =
    (match fullRow c with
    | (prefix, r) ->
        ("root-" ^ (String.join ~sep:"-" (List.map ~f:showRC_ prefix)), r)
    | ([], r) ->
        ("root", r))
  in
  let (_, c) = fullCol c in
  prefix ^ "-" ^ showRC_ (r, c)
*)

let show (c : t) : string =
  match c with
  | [] -> raise (InvalidCoord ("empty coord", c))
  | [(1, 1)] -> "root"
  | [(2, 1)] -> "meta"
  | c when List.last c = Some (1, 1) || List.last c = Some (2, 1) ->
      let rest =
        c |> List.reverse |> List.tail |> Option.withDefault ~default:[]
      in
      let prefix =
        if List.last c = Some (1, 1) then "root-"
        else if List.last c = Some (2, 1) then "meta-"
        else raise (InvalidCoord ("root != 1,1 or meta != 2,1", c))
      in
      prefix ^ (rest |> List.map ~f:showRC_ |> String.join ~sep:"-")
  | _ -> raise (InvalidCoord ("root != 1,1 or meta != 2,1", c))

let showPrefix (c : t) : string =
  match c with
  | [] -> raise (InvalidCoord ("empty coord", c))
  | [(1, 1)] -> "root"
  | [(2, 1)] -> "meta"
  | _ :: prefix -> show prefix

let range (prefix : t) (rows : int) (cols : int) (rowOffset : int)
    (colOffset : int) : t list =
  let prefix = toList prefix in
    (String.join ~sep:", "
       (List.map ~f:(fun (r, c) -> showCol c ^ string_of_int r) prefix)) ;
  let rs = indexRange rowOffset (rowOffset + rows) in
  let cs = indexRange colOffset (colOffset + cols) in
  List.map
    ~f:(fun r ->
      List.map
        ~f:(fun c ->
          (r, c) :: prefix
           )
        cs )
    rs
  |> List.flatten

let ppList (cs : t list) : string = String.join ~sep:"\n" (List.map ~f:show cs)

module Decode = struct
  let t json =
    let open Json.Decode in
    json |> list (tuple2 int int)
end

(*
module Encode = struct
  let t t =
    let open! Json.Encode in (
      list (tuple2 (int int)
    )
end
*)

(* TODO: implement this, used for determining if multiple cells coordinates are contiguous
let areContiguous (cs : t list) : bool =
  true
*)

(* TODO: implement neighbors
type neighbors =
  { n: coord option
  ; nw: coord option
  ; ne: coord option
  ; w : coord option
  ; e : coord option
  ; s : coord option
  ; sw : coord option
  ; se : coord option }

let neighbours (c : coord) : neighbors =
  {}
*)
