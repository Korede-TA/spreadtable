open Prelude
open Tea.App
open Tea.Html
open Tea.Html2.Attributes
module C = Coordinates
open Keyboard

type span = int * int

(* (spannedRows, spannedCols) *)

let defaultNestedTableLayout : Grammar.layout = {rows= 3; cols= 3; spans= []}

type selection = None | Partial of C.t list | Full of C.t list

type msg =
  | ChangeCellData of C.t * string * int
  | SelectBelow of C.t
  | AddNestedTable of C.t * Grammar.layout
  | SelectCells of selection
  | AutoCompeleteGrammar of C.t * Grammar.t
  | ToggleContextMenu of C.t * bool
  | ActivateCell of C.t
  | Noop
[@@bs.deriving {accessors}]

type model =
  { rootGrammar: Grammar.t
  ; grammarMap: Grammar.map
  ; selectedCells: selection
  ; contextMenu: contextMenu }

and contextMenu = {actions: (string * msg) list; isVisible: bool}

let defaultContextMenu =
  { actions= [("Save", Noop); ("Reload", Noop); ("Undo", Noop); ("Redo", Noop)]
  ; isVisible= false }

(* when printing a grammar as a suggestion, print it's name as well as the names
 * of each of it's `Write`-able childGrammars *)
let grammarAsSuggestion (m : model) (g : Grammar.t) : string =
  let open Grammar in
  let children =
    StrDict.toList m.grammarMap
    |> List.map ~f:Tuple2.second
    |> List.filter ~f:(fun g -> C.parent g.coord = Some g.coord)
  in
  let writeableChildren =
    List.filter
      ~f:(fun c -> match c with {mode = Input _; _} -> true | _ -> false)
      children
    |> List.map ~f:(fun g -> g.name)
  in
  g.name ^ String.join ~sep:" " writeableChildren

let spanHiddenCells ((coord, (spanRow, spanCol)) : C.t * span) : C.t list =
  let row = C.row coord in
  let col = C.col coord in
  let rangeRow = indexRange row (row + spanRow) in
  let rangeCol = indexRange col (col + spanCol) in
  List.map2
    ~f:(fun r c ->
      (r, c) :: (List.tail coord |> Option.withDefault ~default:[]) )
    rangeRow rangeCol

let update (m : model) = function
  | Noop -> m
  | ToggleContextMenu (_, isVisible) ->
      {m with contextMenu= {m.contextMenu with isVisible}}
  | ChangeCellData (c, data, _width) ->
      let open Grammar in
      let grammarMap =
        StrDict.update ~key:(C.show c)
          ~f:(fun v ->
            match v with 
            | Some gm -> Some {gm with mode = Input data} 
            | None -> None )
          m.grammarMap
      in
      (*let cellId = "cell-data-" ^ C.show c in
      let elem = Web_document.getElementById cellId in
      Js.Null_undefined.iter elem (fun elem ->
          Web_node.setStyle elem "width" width ) ;
        *)
      {m with grammarMap}
  | AddNestedTable (c, layout) ->
      let open Grammar in
      let cs = C.show c in
      let parentGrammar = StrDict.get ~key:cs m.grammarMap |> optValueExn in
      let newChildCoords =
        C.range parentGrammar.coord layout.rows layout.cols 0 0
        (*[ { C.prefix = [(1,1);(2,2)]; C.head = (1,1) }
        ; { C.prefix = [(1,1);(2,2)]; C.head = (1,2) }
        ; { C.prefix = [(1,1);(2,2)]; C.head = (2,1) }
        ; { C.prefix = [(1,1);(2,2)]; C.head = (2,2) }
        ]*)
      in
      (* add new coords to grammarMap *)
      let newGrammarMap =
        List.foldl
          ~f:(fun c gm ->
            StrDict.insert ~key:(C.show c)
              ~value:(Grammar.singleton "default" c)
              gm )
          ~init:m.grammarMap newChildCoords
      in
      (* update tableLayout of parent grammar *)
      let newGrammarMap =
        StrDict.update ~key:cs
          ~f:(fun v ->
            match v with
            | Some gm -> Some {gm with mode = Table layout}
            | None -> Some parentGrammar )
          newGrammarMap
      in
      (*Js.log2 
        "new grammarMap: "
        (StrDict.keys newGrammarMap |> String.join ~sep:",");*)
      let r = {m with grammarMap= newGrammarMap} in
      Js.log2 "new grammarMap: "
        (StrDict.keys newGrammarMap |> String.join ~sep:",") ;
      r
  | SelectCells _ -> m
  | AutoCompeleteGrammar _ -> 
      (* Web_node. *)
      m
  | SelectBelow _ -> 
      m
  | ActivateCell c ->
      let open Grammar in
      let cs = (C.show c) in
      let gr = StrDict.get ~key:cs m.grammarMap |> optValueExn in
      let grMap = StrDict.update ~key:cs ~f:(fun v -> 
        match v with 
        | Some _ -> Some { gr with status = Active }
        | _ -> None)
        m.grammarMap
      in
      { m with grammarMap = grMap }


let init2 () =
  let rc : C.t = [(1, 1)] in
  let childCoords = C.range rc 5 5 0 0 in
  let grammarMap =
    List.map ~f:(fun c -> (C.show c, Grammar.singleton "default" c)) childCoords
    |> StrDict.fromList
  in
  let open Grammar in
  let root =
    { name= "root"
    ; mode = Table {rows= 5; cols= 5; spans= []}
    ; coord= rc
    ; status = Inactive 
    ; style = Grammar.defaultStyle }
  in
  let grammarMap =
    StrDict.insert ~key:(C.show root.coord) ~value:root grammarMap
  in
  { rootGrammar= root
  ; grammarMap
  ; selectedCells= None
  ; contextMenu= defaultContextMenu }

let onRightClick msg =
  let opts = {stopPropagation= true; preventDefault= true} in
  onWithOptions "contextmenu" opts (Tea_json.Decoder.succeed msg)

let onCtrlEnter msg =
  let isCtrlEnter {ctrl; key_code} =
    if ctrl && key_code = 13 then Tea_json.Decoder.succeed msg
    else Tea_json.Decoder.fail "notEnter"
  in
  on "keypress" (Tea_json.Decoder.andThen isCtrlEnter key_event)

let onKeyEnter msg = 
  let isEnter {ctrl; key_code} =
    if key_code = 13 && (not ctrl) then Tea_json.Decoder.succeed msg
    else Tea_json.Decoder.fail "notEnter"
  in
  on "keypress" (Tea_json.Decoder.andThen isEnter key_event)

let onCellChange (handler : string -> int -> msg) =
  on "change" (Tea_json.Decoder.map2 handler
    (Tea_json.Decoder.at ["target"; "value"] Tea_json.Decoder.string)
    (Tea_json.Decoder.at ["target"; "width"] Tea_json.Decoder.int))

let onShiftTab msg =
  let isCtrlEnter {shift; key_code} =
    if shift && key_code = 13 then Tea_json.Decoder.succeed msg
    else Tea_json.Decoder.fail "notEnter"
  in
  on "keypress" (Tea_json.Decoder.andThen isCtrlEnter key_event)

let rec viewGrammar (m : model) (coord : C.t) : msg Vdom.t =
  Js.log2 "initial grammarMap: "
    (StrDict.keys m.grammarMap |> String.join ~sep:",") ;
  let grammar = StrDict.get ~key:(C.show coord) m.grammarMap in
  let grammar =
    match grammar with Some g -> g | None -> Grammar.singleton "default" coord
  in
  let open Grammar in
  let wrapTD node =
    td
      [ classes
          [ "cell"
          ; "dropdown"
          ; "row-" ^ C.showPrefix coord ^ (coord |> C.row |> C.showRow)
          ; "col-" ^ C.showPrefix coord ^ (coord |> C.col |> C.showCol) ] 
      ; styles (Grammar.getCSS grammar)
      ; id ("cell-" ^ C.show coord) ]
      [ node ]
  in
  (match grammar.mode with
  | Input v ->
      input'
        [ classes
            ["cell-data"; "cell-data-" ^ C.show coord]
        ; id ("cell-data-" ^ C.show coord)
        ; type' "text"
        ; onKeyEnter (ActivateCell (coord)) 
        ; value v ]
        []
  | Text v ->
      p
        [ classes
            ["cell-data"; "cell-data-" ^ C.show coord]
        ; id ("cell-data-" ^ C.show coord)]
        [ text v ]
  | Button (name, gr) ->
      button
        [ ]
        [ text (name ^ gr.name) ]
  | Table layout ->
    (* let rowPrefix = (match C.parent coord with 
                    | None -> [] 
                    | Some r -> C.fullRow r) in*)
    let rows =
      List.map
        ~f:(fun i -> viewRow m (C.toList coord, i))
        (List.initialize layout.rows succ)
    in
    table [class' "sheet"; id (C.showPrefix grammar.coord ^ "sheet")] rows)
  |> wrapTD

and viewRow (m : model) (rowIndex : C.dimensionIndex) : msg Vdom.t =
  let open Grammar in
  let cells : msg Vdom.t list =
    StrDict.toList m.grammarMap
    |> List.map ~f:Tuple2.second
    |> List.filter ~f:(fun g ->
           (*Js.log2 "rowIndex: " (C.ppDimensionIndex rowIndex);
          Js.log2 "coordRow: " (C.ppDimensionIndex (C.fullRow g.coord));*)
           C.fullRow g.coord = rowIndex && C.show g.coord != "root" )
    |> List.map ~f:(fun g -> viewCell m g.coord)
  in
  tr [] cells

and viewCell (m : model) (coord : C.t) : msg Vdom.t =
  td
    [ classes
        [ "cell"
        ; "dropdown"
        ; "row-" ^ C.showPrefix coord ^ (coord |> C.row |> C.showRow)
        ; "col-" ^ C.showPrefix coord ^ (coord |> C.col |> C.showCol) ]
    ; id ("cell-" ^ C.show coord) ]
    (*; onWithOptions "click" opts (SelectCells coord) ]*)
    [viewGrammar m coord]
    (* 
    ( match grammar with
    | {layout= {rows; cols; _}; _} when rows > 1 || cols > 1 ->
        [viewGrammar m coord]
    | {active = false} ->
        [ input'
            [ classes
                ["cell-data"; "cell-data-inactive"; "cell-data-" ^ C.show coord]
            ; id ("cell-data-" ^ C.show coord)
            ; type' "text"
            ; disabled true
            ; onKeyEnter (ActivateCell (coord)) ]
            [] ]
    | {active = true} ->
        [ input'
            [ classes
                ["cell-data"; "cell-data-active"; "cell-data-" ^ C.show coord]
            ; id ("cell-data-" ^ C.show coord)
            ; type' "text"
            ; onCtrlEnter (AddNestedTable (coord, defaultNestedTableLayout))
            ; onCellChange (fun v w -> ChangeCellData (coord, v, w)) ]
            []
        ; viewAutocomplete m coord ] )*)

and viewAutocomplete (m : model) (coord : C.t) : msg Vdom.t =
  let suggestions = ["root-A1"; "root-A2-A2"; "root-A2-A3"] in
  let suggestionGrammars =
    List.map ~f:(fun s -> StrDict.get ~key:s m.grammarMap) suggestions
    |> Option.values
  in
  let suggestionGrammars = suggestionGrammars in
  if false then
    div
      [class' "dropdown-content"]
      ( suggestionGrammars
      |> List.map ~f:(fun s ->
             a
               [ href "#"
               ; onClick (AutoCompeleteGrammar (coord, s))
               ; class' "dropdown-option" ]
               [text (grammarAsSuggestion m s)] ) )
  else Vdom.noNode

let viewContextMenu (m : model) : msg Vdom.t =
  div [class' "menu"]
    [ ul [class' "menu-options"]
        (List.map
           ~f:(fun (name, msg) ->
             li ?key:None ?unique:None
               [class' "menu-option"; onClick msg]
               [text name] )
           m.contextMenu.actions) ]

let view (m : model) : msg Vdom.t =
  div
    [onRightClick (ToggleContextMenu (m.rootGrammar.coord, true))]
    [ viewGrammar m m.rootGrammar.coord
    ; (if m.contextMenu.isVisible then viewContextMenu m else noNode) ]

let main = beginnerProgram {model= init2 (); update; view}
