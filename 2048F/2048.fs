module _2048

open System
open FSharpx.Prelude
open FSharpx.Option

type 'a cell = 'a option
type 'a row = 'a cell list
type 'a board = 'a row list

let rec pad n xs
    = if List.length xs = n 
      then xs 
      else pad n (None::xs)

let SIZE : int = 4
let shift<'a when 'a:equality>  = List.filter ((<>) Option<'a>.None)

let rec merge f xs
    = match xs with
        | [] -> []
        | [x] -> [x]
        | (x :: y :: xs) -> if x = y 
                            then (f <!> x <*> y) :: merge f xs
                            else x :: merge f (y :: xs)

let move f : 'a row -> 'a row
    = pad SIZE << List.rev << merge f << List.rev << shift

let rec transpose (board: 'a board) : 'a board
    = match board with
        | xs when List.concat xs = [] -> []
        | _ -> List.map List.head board :: transpose (List.map List.tail board)

let moveRight f : 'a board -> 'a board
    = move f |> List.map
let moveLeft f : 'a board -> 'a board
    = List.rev << move f << List.rev |> List.map
let moveUp f : 'a board -> 'a board
    = transpose >> moveLeft f >> transpose
let moveDown f : 'a board -> 'a board
    = transpose << moveRight f << transpose

type Direction = Up | Left | Right |Down

let moveDir f dir 
    = match dir with
        | Some Left -> moveLeft f
        | Some Down -> moveDown f
        | Some Up -> moveUp f
        | Some Right -> moveRight f
        | None -> id

type Board<'a when 'a : equality>(board: 'a board, moveDir: Direction option -> 'a board -> 'a board) = 
    member this.board = board
    member this.moveDir dir = Board(moveDir dir this.board, moveDir)

let constructBoard<'a when 'a : equality> (size: int, f: 'a -> 'a -> 'a) =
    let size = size
    let board = List.init size (fun x -> List.init size (fun y -> Option<'a>.None))
    Board(board, moveDir f)


//n param expects the i from List.mapi in boardEmpty, builds the coordinates of the empty cells this way
let rowEmpty (n:int) ns
    = List.mapi (fun i x -> match x with 
                              | None -> Some (n, i) 
                              | Some _ -> None) ns
let boardEmpty<'a when 'a : equality> (xs : 'a option list list)
    = (List.concat << List.mapi rowEmpty) xs |> List.filter (Option.isSome)

let insertNewCell<'a> (k:'a) (i,j) : 'a board -> 'a board
    =
    let replace j k
        = List.mapi (fun idx x -> if idx = j 
                                  then k 
                                  else x)

    List.mapi (fun idx row -> if idx = i 
                                then replace j (Some k) row 
                                else row)

let newCellCoord r b
    = List.tryItem r (boardEmpty b) |> concat 

let isWin win x = not <| List.isEmpty (List.choose (List.tryFind ((=) <| Some win)) x)

let boardFull<'a when 'a : equality> :('a board -> bool)
    = List.isEmpty << boardEmpty

let rec rowHasMerges (row: 'a row)
    = match row with
        | [] -> false
        | [x] -> false
        | (x :: y :: xs) -> if x = y then true else rowHasMerges (y::xs)

let rec boardHasMerges (b: 'a board)
    = List.exists ((=) true) (List.map rowHasMerges <| b) 
    || List.exists ((=) true) (List.map rowHasMerges <| transpose b)

let hasNextMove b = not (boardFull b) || (boardHasMerges b)

let insertAtRandom (x,y) (rnum : Random) (movedBoard: 'a board) : 'a board option
    = let value = match rnum.Next 9 with
                    | 0 -> y 
                    | _ -> x
      let emptyCell = boardEmpty movedBoard |> List.length |> rnum.Next 
      insertNewCell value <!> (newCellCoord emptyCell movedBoard) <*> returnM movedBoard
