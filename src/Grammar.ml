open Prelude
module C = Coordinates

type span = int * int

(* grammars===tables, encapsulates the rows/columns and spans of a table *)
type layout = {rows: int; cols: int; spans: (C.t * span) list}

type color = string

type borders = 
    { top      : color * bool
    ; right    : color * bool
    ; left     : color * bool
    ; bottom   : color * bool 
    ; collapse : bool}

let borderAll ?(color = "black") = 
  { top      = (color, true)
  ; right    = (color, true)
  ; left     = (color, true)
  ; bottom   = (color, true)
  ; collapse = false }

let borderNone = 
  { top      = ("none", false)
  ; right    = ("none", false)
  ; left     = ("none", false)
  ; bottom   = ("none", false)
  ; collapse = false }

type font = 
    { weight : int
    ; color  : color
 (* ; family : string *) }

type style = 
  { borders : borders
  ; font    : font }

type status = Active | Selected | Inactive

(* Main modes (read: types) of grammars in the system
 *)
type mode = 
  (* Text is a static piece of text that is displayed *)
  | Text of string (* value *)
  (* Input is a html input that can be modified by the user *)
  | Input of string (* value *) 
  (* Button is a trigger for an operation that can be set off by the user *)
  | Button of string (* name *) * t 
  (* Table is a container for other grammars *)
  | Table of layout 

and t =
  { name   : string
  ; coord  : C.t
  ; style  : style
  ; mode   : mode
  ; status : status }

type selection = Partial of C.t list | Full of C.t list

type map = t StrDict.t

let defaultBorders = 
  { top      = ("gray", true)
  ; right    = ("gray", true)
  ; left     = ("gray", true)
  ; bottom   = ("gray", true)
  ; collapse = false
  }

let defaultStyle =
  { borders = defaultBorders
  ; font = { weight = 400
           ; color  = "black"}
  }

let getCSS (g : t) = 
  let s = g.style in
  (* border style*)
  let fmtBorder (color, show) =
    if show 
    then "1px solid " ^ color
    else "none"
  in
  [
    ("border-top", fmtBorder s.borders.top)
  ; ("border-right", fmtBorder s.borders.right)
  ; ("border-left", fmtBorder s.borders.left)
  ; ("border-bottom", fmtBorder s.borders.right)
  ]
  @ (match g.mode with 
    | Table _ when s.borders.collapse -> [("border-collapse", "collapse")] 
    | _ -> [])
  (* font style *)
  @ (match g.mode with
    | Input _ | Text _ -> 
        [ ("color", s.font.color)
        ; ("weight", (string_of_int s.font.weight) ^ "px")]
    | _ -> [])

let children (gm : map) (gr : t) : t list =
  StrDict.toList gm
  |> List.map ~f:Tuple2.second
  |> List.filter ~f:(fun g' -> C.parent g'.coord = Some gr.coord)

let rec copy (fromC : C.t) (toC : C.t) (gm : map) : map =
  (* TODO: implement *)
  let gr = StrDict.get ~key:(C.show fromC) gm |> optValueExn in
  (children gm gr)
  |> List.foldl 
      ~init:gm
      ~f:(fun c gm ->
            let head = List.head c.coord |> optValueExn in
            copy c.coord (head::toC) gm)
  |> StrDict.insert ~key:(C.show toC) ~value:({ gr with coord = toC })

let singleton (name : string) (coord : C.t) =
  { name
  ; coord
  ; mode = Input ""
  ; style = defaultStyle
  ; status = Inactive }

let def_ReadGrammar (c : C.t) (gm : map) : map =
  let symbol = 
    { name   = "read-grammar-$"
    ; coord  = (1,1) :: c
    ; style  = { defaultStyle with borders = borderNone }
    ; mode   = Text "$"
    ; status = Inactive }
  in
  let coord = 
    { name   = "read-grammar-coord"
    ; coord  = (1,2) :: c
    ; style  = { defaultStyle with borders = borderNone }
    ; mode   = Input ""
    ; status = Inactive }
  in
  let value = 
    { name   = "read-grammar-value"
    ; coord  = (1,3) :: c
    ; style  = { defaultStyle with borders = borderNone }
    ; mode   = Text ""
    ; status = Inactive }
  in
  let gr = 
    { name   = "read-grammar"
    ; coord  = c
    ; style  = defaultStyle 
    ; mode   = Table {rows = 1; cols = 1; spans = []}
    ; status = Inactive }
  in
  [symbol; coord; value; gr]
  |> List.foldl
    ~f:(fun gr gm ->
      StrDict.insert 
        ~key:(C.show gr.coord)
        ~value:gr
        gm )
    ~init:gm

let def_WriteGrammar (c : C.t) (gm : map) : map=
  let symbol = 
    { name   = "write-grammar-="
    ; coord  = (1,1) :: c
    ; style  = { defaultStyle with borders = borderNone }
    ; mode   = Text "$"
    ; status = Inactive }
  in
  let coord = 
    { name   = "write-grammar-coord"
    ; coord  = (1,2) :: c
    ; style  = { defaultStyle with borders = borderNone }
    ; mode   = Input ""
    ; status = Inactive }
  in
  let value = 
    { name   = "write-grammar-value"
    ; coord  = (1,3) :: c
    ; style  = { defaultStyle with borders = borderNone }
    ; mode   = Text ""
    ; status = Inactive }
  in
  let gr = 
    { name   = "write-grammar"
    ; coord  = c
    ; style  = defaultStyle 
    ; mode   = Table {rows = 1; cols = 1; spans = []}
    ; status = Inactive }
  in
  [symbol; coord; value; gr]
  |> List.foldl
    ~f:(fun gr gm ->
      StrDict.insert 
        ~key:(C.show gr.coord)
        ~value:gr
        gm )
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

