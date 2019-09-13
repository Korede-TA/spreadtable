open Prelude
open Tea.App
open Tea.Html
open Tea.Html2.Attributes
module C = Coordinates
open Keyboard

let glob_autocompleteEnabled = true

type span = (* (spannedRows, spannedCols) *) int * int

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
  let writeableChildren =
    List.filter
      ~f:(fun c -> match c with {mode= Input _; _} -> true | _ -> false)
      (children m.grammarMap g)
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
            | Some ({mode= Input _} as gr) -> Some {gr with mode= Input data}
            | Some gr -> Some gr
            | None -> None )
          m.grammarMap
      in
      {m with grammarMap}
  | AddNestedTable (c, layout) ->
      let open Grammar in
      let cs = C.show c in
      let parentGrammar = StrDict.get ~key:cs m.grammarMap |> optValueExn in
      let newChildCoords =
        C.range parentGrammar.coord layout.rows layout.cols 0 0
      in
      (* update tableLayout of parent grammar *)
      let newGrammarMap =
        StrDict.update ~key:cs
          ~f:(fun v ->
            match v with
            | Some gm -> Some {gm with mode= Table layout}
            | None -> Some parentGrammar )
          m.grammarMap
      in
      (* add new coords to grammarMap *)
      let newGrammarMap =
        List.foldl
          ~f:(fun c gm ->
            StrDict.insert ~key:(C.show c) ~value:(singleton "default" c) gm )
          ~init:newGrammarMap newChildCoords
      in
      Js.log2 "new grammarMap: "
        (StrDict.keys newGrammarMap |> String.join ~sep:",") ;
      {m with grammarMap = newGrammarMap}
  | SelectCells _ -> m
  | AutoCompeleteGrammar (c, gr) ->
      (* Web_node. *)
      let grammarMap = Grammar.copy gr c m.grammarMap in
      {m with grammarMap}
  | SelectBelow _ -> m
  | ActivateCell c ->
      let open Grammar in
      let cs = C.show c in
      let gr = StrDict.get ~key:cs m.grammarMap |> optValueExn in
      let grMap =
        StrDict.update ~key:cs
          ~f:(fun v ->
            match v with Some _ -> Some {gr with status= Active} | _ -> None )
          m.grammarMap
      in
      {m with grammarMap= grMap}

let init2 () =
  (* default displayed root grammar cells *)
  let childCoords = C.range C.coord_ROOT 5 5 0 0 in
  let grammarMap =
    childCoords
    |> List.map ~f:(fun c -> (C.show c, Grammar.singleton "default" c))
    |> StrDict.fromList
  in
  (* add read and write grammars *)
  let grammarMap =
    grammarMap
    |> Grammar.def_ReadGrammar ((1, 1) :: C.coord_META)
    |> Grammar.def_WriteGrammar ((2, 1) :: C.coord_META)
  in
  let open Grammar in
  let root =
    { name= "root"
    ; mode= Table {rows= 5; cols= 5; spans= []}
    ; coord= C.coord_ROOT
    ; status= Inactive
    ; style= defaultStyle }
  in
  let meta =
    { name= "meta"
    ; mode= Table {rows= 10; cols= 5; spans= []}
    ; coord= C.coord_META
    ; status= Inactive
    ; style= defaultStyle }
  in
  let grammarMap =
    let rootA1 = [(1, 1); (1, 1)] in
    let rootA1Layout = Table {rows= 2; cols= 2; spans= []} in
    grammarMap
    |> StrDict.insert ~key:(C.show root.coord) ~value:root
    |> StrDict.insert ~key:(C.show meta.coord) ~value:meta
    |> StrDict.update ~key:(C.show rootA1) ~f:(function
         | Some v -> Some {v with mode= rootA1Layout}
         | None -> None )
    (*|> (fun m ->
      (C.range rootA1 2 2 0 0)
      |> List.foldl ~init:m ~f:(fun c m ->
        StrDict.insert ~key:(C.show c) ~value:(singleton (C.show rootA1) c) m))
        *)
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
    if key_code = 13 && not ctrl then Tea_json.Decoder.succeed msg
    else Tea_json.Decoder.fail "notEnter"
  in
  on "keypress" (Tea_json.Decoder.andThen isEnter key_event)

let onCellChange (handler : string -> int -> msg) =
  on "change"
    (Tea_json.Decoder.map2 handler
       (Tea_json.Decoder.at ["target"; "value"] Tea_json.Decoder.string)
       (Tea_json.Decoder.at ["target"; "width"] Tea_json.Decoder.int))

let onShiftTab msg =
  let isCtrlEnter {shift; key_code} =
    if shift && key_code = 13 then Tea_json.Decoder.succeed msg
    else Tea_json.Decoder.fail "notEnter"
  in
  on "keypress" (Tea_json.Decoder.andThen isCtrlEnter key_event)

let rec viewGrammar (m : model) (coord : C.t) : msg Vdom.t =
  let grammar = StrDict.get ~key:(C.show coord) m.grammarMap in
  let grammar =
    match grammar with
    | Some g -> g
    | None -> Grammar.singleton "default" coord
  in
  let open Grammar in
  let key = "key-" ^ C.show coord in
  let _coordLabel =
    key |> String.split ~on:"-" |> List.head |> Option.withDefault ~default:key
  in
  let wrap nodes =
    div
      [ classes
          [ "cell"
          ; "dropdown"
          ; "row-" ^ C.showPrefix coord ^ (coord |> C.row |> C.showRow)
          ; "col-" ^ C.showPrefix coord ^ (coord |> C.col |> C.showCol) ]
      ; styles (Grammar.getCSS grammar)
      ; id ("cell-" ^ C.show coord) ]
      nodes
  in
  let key = key ^ "-data" in
  ( match grammar.mode with
  | Input v ->
      [ input' ~key:(key ^ "_input")
          [ classes ["cell-data"; key]
          ; id key
          ; type' "text"
          ; onKeyEnter (ActivateCell coord)
          ; onCtrlEnter (AddNestedTable (coord, defaultNestedTableLayout))
          ; value v ]
          []
        (*; span [class' "cell-data-coord"] [text coordLabel]*)
      ; viewAutocomplete m coord ]
  | Text v ->
      [ p ~key:(key ^ "_text")
          [classes ["cell-data"; "cell-data-" ^ C.show coord]; id key]
          [text v]
      (*; span [class' "cell-data-coord"] []*) ]
  | Button (name, gr) ->
      [button ~key:(key ^ "_button") [] [text (name ^ gr.name)]]
  | Table layout ->
      let _rows =
        (* TODO: delete this *)
        List.map
          ~f:(fun i -> viewRow m (C.toList coord, i))
          (List.initialize layout.rows succ)
      in
      let children =
        Grammar.children m.grammarMap grammar
        |> List.map ~f:(fun g -> viewGrammar m g.coord)
      in
      let _t =
        [ div
            [class' "sheet"; id (C.showPrefix grammar.coord ^ "sheet")]
            children ]
      in
      children )
  |> wrap

and viewRow (m : model) (rowIndex : C.dimensionIndex) : msg Vdom.t =
  let open Grammar in
  let cells : msg Vdom.t list =
    StrDict.toList m.grammarMap
    |> List.map ~f:Tuple2.second
    |> List.filter ~f:(fun g ->
           C.fullRow g.coord = rowIndex && C.show g.coord != "root" )
    |> List.map ~f:(fun g -> viewGrammar m g.coord)
  in
  div [class' "row"] cells

and viewAutocomplete (m : model) (coord : C.t) : msg Vdom.t =
  let readGrammarCoord = (1, 1) :: C.coord_META |> C.show in
  let writeGrammarCoord = (2, 1) :: C.coord_META |> C.show in
  let suggestions = ["root-A1"; readGrammarCoord; writeGrammarCoord] in
  let suggestionGrammars =
    List.map ~f:(fun s -> StrDict.get ~key:s m.grammarMap) suggestions
    |> Option.values
  in
  let suggestionGrammars = suggestionGrammars in
  if glob_autocompleteEnabled then
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

let rec viewGrid (m : model) (coord : C.t) : msg Vdom.t list =
  let open Grammar in
  let grammar = StrDict.get ~key:(C.show coord) m.grammarMap in
  let cells : msg Vdom.t list =
    grammar
    |> Option.map ~f:(children m.grammarMap)
    |> Option.withDefault ~default:[]
    (* sort by row for proper grid ordering *)
    |> List.sortBy ~f:(fun g -> g.coord |> C.row)
    |> List.map ~f:(fun g ->
           let coordStr = C.show g.coord in
           let key = "key-" ^ coordStr in
           let gridRow = C.showPrefix g.coord ^ "-" ^ (g.coord |> C.row |> C.showRow) in
           let gridCol = C.showPrefix g.coord ^ "-" ^ (g.coord |> C.col |> C.showCol) in
           div
             [ classes
                 [ (*"cell"
                 ; *)"dropdown"
                 ; "row-" ^ gridRow
                 ; "col-" ^ gridCol
                 ]
             ; styles
                 (( grammar |> Option.map ~f:getCSS
                 |> Option.withDefault ~default:[] )
                 (*@ [ ("grid-column", "grid-column-"^gridCol)
                   ; ("grid-row", "grid-row-"^gridRow)] *))
             ; id ("cell-" ^ coordStr) ]
             ( match g.mode with
             | Input v ->
                 [ input' ~key:(key ^ "_input")
                     [ classes ["cell-data"; key]
                     ; id key
                     ; type' "text"
                     ; onKeyEnter (ActivateCell coord)
                     ; onCtrlEnter
                         (AddNestedTable (coord, defaultNestedTableLayout))
                     ; value v ]
                     []
                   (*; span [class' "cell-data-coord"] [text coordLabel]*)
                 ; viewAutocomplete m coord ]
             | Text v ->
                 [ p ~key:(key ^ "_text")
                     [classes ["cell-data"; "cell-data-" ^ coordStr]; id key]
                     [text v]
                 (*; span [class' "cell-data-coord"] []*) ]
             | Button (name, _) ->
                 [button ~key:(key ^ "_button") [] [text (name ^ g.name)]]
             | Table _ -> (viewGrid m g.coord) ) )
  in
  cells

let view (m : model) : msg Vdom.t =
  div
    [ onRightClick (ToggleContextMenu (m.rootGrammar.coord, true))
    ; styles (Grammar.getGridCSS ~root:m.rootGrammar m.grammarMap) ]
    ( [ (* viewGrammar m m.rootGrammar.coord *)
        (if m.contextMenu.isVisible then viewContextMenu m else noNode) ]
    @ viewGrid m m.rootGrammar.coord )

let main = beginnerProgram {model= init2 (); update; view}
