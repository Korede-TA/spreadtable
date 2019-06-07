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

let borderAll color = 
  { top      = ("black", true)
  ; right    = ("black", true)
  ; left     = ("black", true)
  ; bottom   = ("black", true)
  ; collapse = false }

let borderNone color = 
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

type mode = 
  | Input of string (* value *) 
  | Button of string (* name *) * t 
  | Table of layout 
  | Text of string (* value *)

and t =
  { name   : string
  ; coord  : C.t
  ; style  : style
  ; mode   : mode
  ; status : status }

type selection = Partial of C.t list | Full of C.t list

type map = t StrDict.t

let defaultBorders = 
  { top      = ("black", true)
  ; right    = ("black", true)
  ; left     = ("black", true)
  ; bottom   = ("black", true)
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

let singleton (name : string) (c : C.t) =
  { name
  ; coord= c
  ; mode= Input ""
  ; style = defaultStyle
  ; status = Inactive }

let ifGrammar (grMap : map) (c : C.t) =
  { name = "if-conditional"
  ; coord = c
  ; mode= Input ""
  ; style = defaultStyle
  ; status = Inactive }
