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

let shift<'a when 'a:equality>  = List.filter ((<>) Option<'a>.None)

let rec merge f xs
    = match xs with
        | [] -> []
        | [x] -> [x]
        | (x :: y :: xs) -> if x = y 
                            then (f <!> x <*> y) :: merge f xs
                            else x :: merge f (y :: xs)

let move f size : 'a row -> 'a row
    = pad size << List.rev << merge f << List.rev << shift

let rec transpose (board: 'a board) : 'a board
    = match board with
        | xs when List.concat xs = [] -> []
        | _ -> List.map List.head board :: transpose (List.map List.tail board)

let moveRight f size : 'a board -> 'a board
    = move f size |> List.map
let moveLeft f size: 'a board -> 'a board
    = List.rev << move f size << List.rev |> List.map
let moveUp f size : 'a board -> 'a board
    = transpose >> moveLeft f size >> transpose
let moveDown f size : 'a board -> 'a board
    = transpose << moveRight f size << transpose

type Direction = Up | Left | Right |Down

let moveDir f size dir 
    = match dir with
        | Some Left -> moveLeft f size
        | Some Down -> moveDown f size 
        | Some Up -> moveUp f size 
        | Some Right -> moveRight f size 
        | None -> id

//n param expects the i from List.mapi in boardEmpty, builds the coordinates of the empty cells this way
let boardEmpty<'a when 'a : equality> (xs : 'a option list list) = 
    let rowEmpty (n:int) ns
        = List.mapi (fun i x -> match x with 
                                  | None -> Some (n, i) 
                                  | Some _ -> None) ns
    (List.concat << List.mapi rowEmpty) xs |> List.filter (Option.isSome)

let insertNewCell<'a> (k:'a) (i,j) : 'a board -> 'a board
    =
    let replace j k
        = List.mapi (fun idx x -> if idx = j 
                                  then k 
                                  else x)

    List.mapi (fun idx row -> if idx = i 
                                then replace j (Some k) row 
                                else row)

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
      let newCellCoord r b
          = List.item r (boardEmpty b) 
      let emptyCell = boardEmpty movedBoard |> List.length |> rnum.Next 
      (flip (insertNewCell value) movedBoard) <!> (newCellCoord emptyCell movedBoard) 

type Board<'a when 'a : equality>(board: 'a board option, moveDir: Direction option -> 'a board -> 'a board, values: 'a*'a, size: int, win: 'a) = 
    member this.Board = board
    member this.MoveDir dir = Board(moveDir dir <!> this.Board, moveDir, values, size, win)
    member this.InsertAtRandom rnum = Board(this.Board >>= (insertAtRandom values rnum), moveDir, values, size, win)
    member this.HasNextMove = hasNextMove <!> this.Board |> Option.exists id
    member this.IsWin = isWin win <!> this.Board |> Option.exists id
    static member constructBoard (size: int) (values: 'a*'a) (win: 'a) (op: 'a -> 'a -> 'a) : 'a Board =
        let size = size
        let board = List.init size (fun x -> List.init size (fun y -> Option<'a>.None))
        Board(Some board, moveDir op size, values, size, win)