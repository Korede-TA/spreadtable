open Prelude

type t = 
  { prefix : (int * int) list (* in the case of nested *)
  ; head : (int * int) }

let toList (c : t) : (int * int) list = 
  c.prefix @ [c.head]

type dimensionIndex = ((int * int) list) * int

let ppDimensionIndex (d : dimensionIndex) : string = 
  let p, d = d in
  String.join ~sep: ";" 
    (List.map (fun (a, b) ->  string_of_int a ^ ", " ^ string_of_int b) p @
      [string_of_int d])

let fullRow (c : t) : dimensionIndex = 
    (c.prefix, Tuple2.first c.head)

let fullCol (c : t) : dimensionIndex = 
    (c.prefix, Tuple2.second c.head)

let row r = fullRow r |> Tuple2.second

let col c = fullCol c |> Tuple2.second

let parent (c : t) : t option = 
  match c.prefix with
  | head::tail ->
    Some { prefix = tail
         ; head = head }
  | [] -> 
      None

let rec showCol (i : int) : string =
  let alphaOffset = 64 in
  let normalizedI = if i == 26 then 26 else i mod 26 in
  let baseChar = normalizedI+alphaOffset |> Char.fromCode |> String.fromChar in
  baseChar ^ (if i > 26 then showCol (i-26) else "")

let showRow (i : int) : string = string_of_int i

let rec show (c : t) : string = 
  match prefixString c with
  | "" -> "root"
  | v -> v ^ (col c |> showCol) ^ (row c |> showRow)

and prefixString (c : t) : string =
  match parent c with
  | Some p -> show p ^ "-"
  | None -> ""

let range 
  (prefix : t)
  (rows : int) 
  (cols : int) 
  (rowOffset : int) 
  (colOffset : int) :
  t list
  =
  Js.log2 "prefix: " (show prefix);
  let prefix = toList prefix in
  let rs = indexRange rowOffset (rowOffset+rows) in
  let cs = indexRange colOffset (colOffset+cols) in
  List.map ~f:(fun r -> 
    List.map ~f:(fun c ->
      let v = { prefix; head = (r, c) } in
      Js.log2 "coord " (showCol c ^ (string_of_int r));
      Js.log2 "val: " (show v);
      v
    ) cs) rs 
  |> List.flatten

let ppList (cs : t list) : string = 
  String.join ~sep:"\n" (List.map ~f:show cs)

(* used for determining if multiple cells coordinates are contiguous *)
let areContiguous (cs : t list) : bool =
  true

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
