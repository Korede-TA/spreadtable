open Jest
open Expect
open! Expect.Operators
open Main

let testInit () = 
  let rc : Coordinates.t =
    { prefix = []
    ; head = (1,1) } in
  let childCoords = (Coordinates.range rc 2 2 0 0) in
  let grammarMap = 
    List.map 
    ~f:(fun c -> (Coordinates.show c, singletonGrammar "default" c))
    childCoords
    |> StrDict.fromList
  in
  let root = { name = "root"
  ; layout = { rows = 2; cols = 2; spans = [] }
  ; coord = rc
  ; childCoords 
  ; mode = Write } in
  let grammarMap = 
    StrDict.insert ~key:(Coordinates.show root.coord) ~value:root grammarMap
  in
  { rootGrammar = root
  ; grammarMap
  ; selectedCells = []
  ; contextMenu = defaultContextMenu
  }

let () = 

describe "Coordinates.prefixString works" (fun () -> 
  true
  (* let m = update *)
)
