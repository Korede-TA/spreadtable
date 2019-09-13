open Prelude
module C = Coordinates

type span = int * int

(* grammars===tables, encapsulates the rows/columns and spans of a table *)
type layout = {rows: int; cols: int; spans: (C.t * span) list}

type color = string

type borders =
  { top: color * bool
  ; right: color * bool
  ; left: color * bool
  ; bottom: color * bool
  ; collapse: bool }

let borderAll ?(color = "black") =
  { top= (color, true)
  ; right= (color, true)
  ; left= (color, true)
  ; bottom= (color, true)
  ; collapse= false }

let borderNone =
  { top= ("none", false)
  ; right= ("none", false)
  ; left= ("none", false)
  ; bottom= ("none", false)
  ; collapse= false }

type font = {weight: int; color: color (* ; family : string *)}

type style = {borders: borders; font: font}

type status = Active | Selected | Inactive

(* Main modes (read: types) of grammars in the system
 *)
type mode =
  (* Text is a static piece of text that is displayed *)
  | Text of string
  (* value *)
  (* Input is a html input that can be modified by the user *)
  | Input of string
  (* value *)
  (* Button is a trigger for an operation that can be set off by the user *)
  | Button of string (* name *) * t
  (* Table is a container for other grammars *)
  | Table of layout

and t = {name: string; coord: C.t; style: style; mode: mode; status: status}

type selection = Partial of C.t list | Full of C.t list

module Decode = struct
  open Json.Decode

  let borders json =
    { top= json |> tuple2 string bool
    ; right= json |> tuple2 string bool
    ; left= json |> tuple2 string bool
    ; bottom= json |> tuple2 string bool
    ; collapse= json |> bool }

  let font json = {weight= json |> int; color= json |> string}

  let style json = {borders= json |> borders; font= json |> font}

  (*let mode json =
    oneOf
      [
        Text (json )
      ]*)
end

type map = t StrDict.t

module Map = struct
  type t = map
end

(*
module Debug = struct
  let rec prettyPrint (m : map) ?(depth = 0) ?(visited = StrSet.empty) : string =
    let k =
    StrDict.map m
    |> List.map ~f:(fun k v ->
      let prefix = String.repeat ~count:depth "\t" in
      let visited = StrSet.add ~value:k visited in
      prefix ^ key ^ (prettyPrint m ~depth:(depth+1) ~visited)
    )
end
*)

let defaultBorders =
  { top= ("gray", true)
  ; right= ("gray", true)
  ; left= ("gray", true)
  ; bottom= ("gray", true)
  ; collapse= false }

let defaultStyle =
  {borders= defaultBorders; font= {weight= 400; color= "black"}}

let children (gm : map) (gr : t) : t list =
  StrDict.toList gm |> List.map ~f:Tuple2.second
  |> List.filter ~f:(fun g' -> C.parent g'.coord = Some gr.coord)

let getCSS (g : t) =
  let s = g.style in
  (* border style*)
  let fmtBorder (color, show) =
    if show then "1px solid " ^ color else "none"
  in
  [ ("border-top", fmtBorder s.borders.top)
  ; ("border-right", fmtBorder s.borders.right)
  ; ("border-left", fmtBorder s.borders.left)
  ; ("border-bottom", fmtBorder s.borders.right) ]
  @ ( match g.mode with
    | Table _ when s.borders.collapse -> [("border-collapse", "collapse")]
    | _ -> [] )
  (* font style *)
  @
  match g.mode with
  | Input _ | Text _ ->
      [("color", s.font.color); ("weight", string_of_int s.font.weight ^ "px")]
  | _ -> []

(* grid layout *)
(* @ (match g.mode with
    | Table layout ->
      let rows, cols = string_of_int layout.rows, string_of_int layout.cols in
      let colStart = g.coord |> C.col |> string_of_int in
      let colEnd = (g.coord |> C.col) + 1 |> string_of_int in
      let rowStart = g.coord |> C.row |> string_of_int in
      let rowEnd = (g.coord |> C.row) + 1 |> string_of_int in
      if (g.coord |> List.length) = 1 (* if root table *) then
        [ ("display", "grid")
        ; ("grid-template-columns", "repeat("^cols^", [col] 300px")
        ; ("grid-template-rows", "repeat("^rows^", [row] auto")]
      else
        [ ("display", "grid")
        ; ("grid-template-columns", "repeat("^cols^", 1fr")
        ; ("grid-template-rows", "repeat("^rows^", [row] auto")]
    | _ -> [])
  @ (let colStart = g.coord |> C.col |> string_of_int in
     let colEnd = (g.coord |> C.col) + 1 |> string_of_int in
     let rowStart = g.coord |> C.row |> string_of_int in
     let rowEnd = (g.coord |> C.row) + 1 |> string_of_int in
     [ ("grid-column", colStart ^ " / " ^ colEnd)
     ; ("grid-row", rowStart ^ " / " ^ rowEnd)]) *)

let getGridCSS ~(root : t) (gm : map) =
  (* let defaultColWidth = "300px" in
  let defaultRowHeight = "30px" in
  let rec templateCols ?(accum = []) g =
    let gridCol = C.showPrefix g.coord ^ "-" ^ (g.coord |> C.col |> C.showCol) in
    let cols =
      let children = children gm g in
      if (List.member ~value:gridCol accum)
      then ""
      else if children <> [] then
        "[grid-column-" ^ gridCol ^ "-start]"
        ^ " 0px "
        ^ (List.map ~f:(templateCols ~accum:(gridCol :: accum)) children |> String.join ~sep:" 0px ")
        ^ " 0px "
        ^ "[grid-column-" ^ gridCol ^ "-end]"
      else
        "[grid-column-" ^ gridCol ^ "-start]"
        ^ " " ^ defaultColWidth ^ " "
        ^ "[grid-column-" ^ gridCol ^ "-end]"
    in
    cols
  in
  let rec templateRows ?(accum = []) g =
    let gridRow = C.showPrefix g.coord ^ "-" ^ (g.coord |> C.row |> C.showRow) in
    let rows =
      let children = children gm g in
      if (List.member ~value:gridRow accum)
      then ""
      else if children <> [] then
        "[grid-row-" ^ gridRow ^ "-start]"
        ^ " 0px "
        ^ (List.map ~f:(templateRows ~accum:(gridRow :: accum)) children |> String.join ~sep:" 0px ")
        ^ " 0px "
        ^ "[grid-row-" ^ gridRow ^ "-end]"
      else
        "[grid-row-" ^ gridRow ^ "-start]"
        ^ " " ^ defaultRowHeight ^ " "
        ^ "[grid-row-" ^ gridRow ^ "-end]"
    in
    rows
  in *)
  let colCount, rowCount =
    match root.mode with Table {cols; rows} -> cols, rows | _ -> 1, 1
  in
  let res = [ ("display", "grid")
  ; ("grid-template-columns", String.repeat ~count:colCount "300px " (* templateCols root *))
  ; ("grid-template-rows", String.repeat ~count:rowCount "30px " (* templateRows root *)) ]
  in
  Js.log2 "grid css" res;
  res

let rec copy (gr : t) (toC : C.t) (gm : map) : map =
  children gm gr
  |> List.foldl ~init:gm ~f:(fun c gm ->
         let head = List.head c.coord |> optValueExn in
         copy c (head :: toC) gm )
  |> StrDict.insert ~key:(C.show toC) ~value:{gr with coord= toC}

let copyC (fromC : C.t) (toC : C.t) (gm : map) : map =
  let gr = StrDict.get ~key:(C.show fromC) gm |> optValueExn in
  copy gr toC gm

let singleton (name : string) (coord : C.t) =
  {name; coord; mode= Input ""; style= defaultStyle; status= Inactive}

let def_ReadGrammar (c : C.t) (gm : map) : map =
  (* cells within grammar *)
  let symbol =
    { name= "read-grammar-$"
    ; coord= (1, 1) :: c
    ; style= {defaultStyle with borders= borderNone}
    ; mode= Text "$"
    ; status= Inactive }
  in
  let coord =
    { name= "read-grammar-coord"
    ; coord= (1, 2) :: c
    ; style= {defaultStyle with borders= borderNone}
    ; mode= Input ""
    ; status= Inactive }
  in
  let value =
    { name= "read-grammar-value"
    ; coord= (1, 3) :: c
    ; style= {defaultStyle with borders= borderNone}
    ; mode= Text ""
    ; status= Inactive }
  in
  (* full grammar *)
  let gr =
    { name= "read-grammar"
    ; coord= c
    ; style= defaultStyle
    ; mode= Table {rows= 1; cols= 1; spans= []}
    ; status= Inactive }
  in
  [symbol; coord; value; gr]
  |> List.foldl
       ~f:(fun gr gm -> StrDict.insert ~key:(C.show gr.coord) ~value:gr gm)
       ~init:gm

let def_WriteGrammar (c : C.t) (gm : map) : map =
  let symbol =
    { name= "write-grammar-="
    ; coord= (1, 1) :: c
    ; style= {defaultStyle with borders= borderNone}
    ; mode= Text "$"
    ; status= Inactive }
  in
  let coord =
    { name= "write-grammar-coord"
    ; coord= (1, 2) :: c
    ; style= {defaultStyle with borders= borderNone}
    ; mode= Input ""
    ; status= Inactive }
  in
  let value =
    { name= "write-grammar-value"
    ; coord= (1, 3) :: c
    ; style= {defaultStyle with borders= borderNone}
    ; mode= Text ""
    ; status= Inactive }
  in
  let gr =
    { name= "write-grammar"
    ; coord= c
    ; style= defaultStyle
    ; mode= Table {rows= 1; cols= 1; spans= []}
    ; status= Inactive }
  in
  [symbol; coord; value; gr]
  |> List.foldl
       ~f:(fun gr gm -> StrDict.insert ~key:(C.show gr.coord) ~value:gr gm)
       ~init:gm

(*
let defGrammar (grMap : map) (c : C.t) =
  let childCoords =
    C.range c layout.rows layout.cols 0 0
    [
      {name = ""}
  in
  let gr = { name = "definition"
  ; coord = c
  ; mode= Input ""
  ; style = defaultStyle
  ; status = Inactive } in
  grMap.insert ~key:(C.show root.coord) ~value:root grMap
*)

(*
let updateMap (gm : map) (gr : t) : map =
  (* either does a plain update to a grammar
   * or reads a definition from meta *)
  let cs = C.show gr.coord in
  match gr.mode with
  | Table layout ->
    let childCoords =
      StrDict.keys gm
      |> List.filter ~f:(fun k ->
          let len = List.length k in
          let kPar =
            String.split ~on:"-" k
            |> List.take ~count:(len - 1)
            |> String.join ~sep:"-"
          in
          (kPar = cs))
    in
    let newGrammarMap =
      List.foldl.
        ~f:(fun c gm ->
          StrDict.insert ~key:cs
            ~value:(Grammar.singleton "default" c)
            gm )
        ~init:m.grammarMap childCoords
    in
    (* update tableLayout of parent grammar *)
    let newGrammarMap =
      StrDict.update ~key:cs
        ~f:(fun v ->
          match v with
          | _ -> Some {gr with mode = Table layout})
        newGrammarMap
  | _ ->
*)
