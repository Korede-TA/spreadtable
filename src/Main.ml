open Prelude
open Tea.App
open Tea.Html
open Tea.Html2.Attributes
module C = Coordinates 
open Keyboard

type span = (int * int) (* (spannedRows, spannedCols) *)

type data =
  | RawStr of string

(* grammars===tables, encapsulates the rows/columns and spans of a table *)
type tableLayout =  
  { rows : int
  ; cols : int
  ; spans : (C.t * span) list }

let defaultNestedTableLayout = 
  { rows = 3; cols = 3; spans = [] }

type grammarMode = Write | ReadOnly

type grammar = 
  { name : string
  ; layout : tableLayout 
  ; coord : C.t
  (*; fromString : string -> grammar option (* parse from string, for testing
   * autocomplete *) *)
  (*;  eval : grammar -> grammar (* method for evaluating the lambda *) *)
  (*; childCoords : C.t list *)
  ; mode : grammarMode
  (* ; asSuggestion : (grammar -> string) *)
  }

type selection = Partial of C.t list | Full of C.t list

type msg =
  | ChangeCellData of C.t
  | AddNestedTable of C.t * tableLayout
  | SelectCells of selection
  | AutoCompeleteGrammar of C.t
  | ToggleContextMenu of C.t * bool
  | Noop
  [@@bs.deriving {accessors}] 


type model = 
  { rootGrammar : grammar
  ; grammarMap :  grammar StrDict.t 
  ; selectedCells : C.t list 
  ; contextMenu : contextMenu }

and contextMenu = 
  { actions : (string * msg) list
  ; isVisible : bool }

let defaultContextMenu = 
  { actions = [ ("Save", Noop)
              ; ("Reload", Noop)
              ; ("Undo", Noop)
              ; ("Redo", Noop) ]
  ; isVisible = false }

(* when printing a grammar as a suggestion, print it's name as well as the names
 * of each of it's `Write`-able childGrammars *)
let grammarAsSuggestion (m : model) (g : grammar) : string =
  let children = 
    StrDict.toList m.grammarMap
    |> List.map ~f:Tuple2.second
    |> List.filter ~f:(fun g -> C.parent g.coord = Some (g.coord))
  in
  let writeableChildren = 
    List.filter ~f:(fun c -> 
      match c with
      | {mode = Write; _} -> true
      | _ -> false) children
    |> List.map ~f:(fun g -> g.name)
  in
  g.name ^ String.join ~sep:" " writeableChildren


let spanHiddenCells (cellWithSpan : C.t * span) : C.t list = 
  let coord = Tuple2.first cellWithSpan in
  let row = C.row coord in
  let col = C.col coord in
  let spanRow, spanCol = Tuple2.second cellWithSpan in
  let rangeRow = indexRange row (row+spanRow) in
  let rangeCol = indexRange col (col+spanCol) in
  List.map2 ~f:(fun r c -> { coord with head = (r, c) }) rangeRow rangeCol


let singletonGrammar (name : string) (c : C.t) = 
  let row = C.row c in
  let col = C.col c in
  let parentCoord = C.parent c in
  let prefix = (match parentCoord with
    | Some p -> p.prefix @ [p.head]
    | None -> []) in
  { name 
  ; layout = 
    { rows = 1
    ; cols = 1
    ; spans = [] }
    ; coord = { prefix = prefix; head = (row, col) }
    ; mode = Write }


let update (m : model) = function
  | Noop -> m
  | ToggleContextMenu (c, isVisible) ->
      { m with contextMenu = { m.contextMenu with isVisible } }
  | ChangeCellData _ -> m
  | AddNestedTable (c, layout) ->
      Js.log2 "adding nested table to: \n" (C.show c);
      let cs = C.show c in
      Js.log2 "coord: "  cs;
      let parentGrammar = StrDict.get ~key:cs m.grammarMap |> optValueExn in
      Js.log2 "parentGrammar.coord: "  (C.show parentGrammar.coord);
      let newChildCoords = 
        (C.range parentGrammar.coord layout.rows layout.cols 0 0) 
      in
      Js.log2 "newChildCoords: \n" (C.ppList newChildCoords);
      (* add new coords to grammarMap *)
      let newGrammarMap =  
        List.foldl
        ~f:(fun c gm ->
          StrDict.insert 
            ~key:(C.show c) 
            ~value:(singletonGrammar "default" c)
            gm)
        ~init:m.grammarMap
        newChildCoords
      in
      (* update tableLayout of parent grammar *)
      let newGrammarMap = 
        StrDict.update
          ~key:cs
          ~f:(fun v -> 
            match v with
            | Some gm -> 
                Some { gm with layout }
            | None -> Some parentGrammar)
          newGrammarMap
      in
      (*Js.log2 
        "new grammarMap: "
        (StrDict.keys newGrammarMap |> String.join ~sep:",");*)
      let r = { m with grammarMap = newGrammarMap } in
      Js.log2 
        "new grammarMap: "
        (StrDict.keys newGrammarMap |> String.join ~sep:",");
      r
  | SelectCells _ -> m
  | AutoCompeleteGrammar _ -> m

let init () = 
  let rc : C.t =
    { prefix = []
    ; head = (1,1) } in
  let childCoords = (C.range rc 100 26 0 0) in
  let grammarMap = 
    List.map 
    ~f:(fun c -> (C.show c, singletonGrammar "default" c))
    childCoords
    |> StrDict.fromList
  in
  let defaultRootGrammar = 
    { name = "root"
    ; layout = 
      { rows = 100
      ; cols = 26 (* A-Z *)
      ; spans = [] }
    ; coord = rc
    ; mode = Write } in
  let grammarMap = 
    StrDict.insert ~key:(C.show defaultRootGrammar.coord) ~value:defaultRootGrammar grammarMap
  in
  { rootGrammar = defaultRootGrammar
  ; grammarMap
  ; selectedCells = []
  ; contextMenu = defaultContextMenu
  }

let init2 () = 
  let rc : C.t =
    { prefix = []
    ; head = (1,1) } in
  let childCoords = (C.range rc 5 5 0 0) in
  let grammarMap = 
    List.map 
    ~f:(fun c -> (C.show c, singletonGrammar "default" c))
    childCoords
    |> StrDict.fromList
  in
  let root = { name = "root"
  ; layout = { rows = 5; cols = 5; spans = [] }
  ; coord = rc
  ; mode = Write } in
  let grammarMap = 
    StrDict.insert ~key:(C.show root.coord) ~value:root grammarMap
  in
  { rootGrammar = root
  ; grammarMap
  ; selectedCells = []
  ; contextMenu = defaultContextMenu
  }

let onRightClick msg = 
  let opts = 
    { stopPropagation = true
    ; preventDefault = true } in
  onWithOptions "contextmenu" opts (Tea_json.Decoder.succeed msg)

let onCtrlEnter msg = 
  let isCtrlEnter {ctrl; key_code} = 
    if ctrl && key_code = 13 then 
      Tea_json.Decoder.succeed msg 
    else
      Tea_json.Decoder.fail "notEnter"
  in
  on "keypress" (Tea_json.Decoder.andThen isCtrlEnter key_event) 

let rec viewGrammar (m : model) (coord : C.t) : msg Vdom.t =
  Js.log2 
    "initial grammarMap: "
    (StrDict.keys m.grammarMap |> String.join ~sep:",");
  let grammar = StrDict.get ~key:(C.show coord) m.grammarMap  in
  let grammar = (match grammar with
                | Some g -> g
                | None -> singletonGrammar "default" coord) in
  (* let rowPrefix = (match C.parent coord with 
                  | None -> [] 
                  | Some r -> C.fullRow r) in*)
  let rows = 
    List.map 
      ~f:(fun i -> viewRow m ((C.toList coord, i))) 
      ((List.initialize grammar.layout.rows succ)) 
  in
  table
    [ class' "sheet"; id (C.prefixString grammar.coord ^ "sheet")]
    rows

and viewRow (m : model) (rowIndex : C.dimensionIndex) : msg Vdom.t =
  let cells : msg Vdom.t list = StrDict.toList m.grammarMap 
    |> List.map ~f:Tuple2.second
    |> List.filter 
        ~f:(fun g -> 
          (*Js.log2 "rowIndex: " (C.ppDimensionIndex rowIndex);
          Js.log2 "coordRow: " (C.ppDimensionIndex (C.fullRow g.coord));*)
          ((C.fullRow g.coord) = rowIndex) && ((C.show g.coord) != "root"))
    |> List.map ~f:(fun g -> 
        viewCell m g.coord)
  in
  tr
    []
    cells

and viewCell (m : model) (coord : C.t) : msg Vdom.t = 
  let grammar = StrDict.get ~key:(C.show coord) m.grammarMap in
  let grammar = (match grammar with
                | Some g -> g
                | None -> singletonGrammar "default" coord) in
  let cellId = ("cell-" ^ C.show coord) in
  let opts = 
    { stopPropagation = true
    ; preventDefault = true } in
  td 
    [ classes
      [ "cell"
      ; "dropdown"
      ; ( "row-" 
        ^ C.prefixString coord 
        ^ (coord |> C.row |> C.showRow))
      ; ( "col-" 
        ^ C.prefixString coord 
        ^ (coord |> C.col |> C.showCol)) ]
    ; id cellId]
    (*; onWithOptions "click" opts (SelectCells coord) ]*)
      (if grammar.layout.rows > 1 || grammar.layout.cols > 1 then
        [ viewGrammar m coord ]
      else
        [
          input'
            [ classes
              [ "cell-data"
              ; "cell-data-" ^ C.show coord]
            ; id ("cell-data-" ^ C.show coord) 
            ; type' "text" 
            ; onCtrlEnter (AddNestedTable (coord, defaultNestedTableLayout)) ]
            []
        ; (viewAutocomplete m coord)
        ])

and viewAutocomplete (m : model) (coord : C.t) : msg Vdom.t = 
  let suggestions = ["root-A2-A2"; "root-A2-A3"] in
  let suggestionGrammars = 
    List.map ~f:(fun s -> StrDict.get ~key:s m.grammarMap) suggestions |> Option.values
  in
  div
    [ class' "dropdown-content" ]
    (List.map ~f:(grammarAsSuggestion m) suggestionGrammars
    |> List.map 
        ~f:(fun s -> 
          a 
            [ href "#"
            ; onClick (AutoCompeleteGrammar coord)
            ; class' "dropdown-option"] 
            [ text s ] ))

let viewContextMenu (m : model) : msg Vdom.t =
  div
    [ class' "menu" ]
    [
      ul 
      [ class' "menu-options" ]
      (List.map
        ~f:(fun (name, msg) ->
          li
            ?key:None ?unique:None 
            [ class' "menu-option" 
            ; onClick msg
            ]
            [ text name ])
        m.contextMenu.actions)
    ]

let view (m : model) : msg Vdom.t =
  let opts = 
    { stopPropagation = true
    ; preventDefault = true } in
  div
    [ onRightClick (ToggleContextMenu (m.rootGrammar.coord, true)) ]
    [ viewGrammar m m.rootGrammar.coord
    ; if m.contextMenu.isVisible then viewContextMenu m else noNode
    ]

let main =
  beginnerProgram 
  { model = init2 ()
  ; update
  ; view
  }
