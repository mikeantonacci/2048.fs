module _2048

open System
open FSharpx.Option

type 'a cell = 'a option
type 'a row = 'a cell list
type 'a board = 'a row list
type Direction = Up | Left | Right |Down

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

let moveDir f size dir : 'a board -> 'a board
    = match dir with
        | Left -> moveLeft f size
        | Down -> moveDown f size 
        | Up -> moveUp f size 
        | Right -> moveRight f size 

//n param expects the i from List.mapi in boardEmpty, builds the coordinates of the empty cells this way
//Option.get must be safe here because of the Option.isSome
let boardEmpty<'a when 'a : equality> : 'a option list list -> (int*int) list = 
    let rowEmpty (n:int) ns
        = List.mapi (fun i x -> match x with 
                                  | None -> Some (n, i) 
                                  | Some _ -> None) ns
    List.map Option.get << List.filter Option.isSome << List.concat << List.mapi rowEmpty

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

let insertAtRandom (x,y) (rnum : Random) (movedBoard: 'a board) : 'a board
    = let value = match rnum.Next 9 with
                    | 0 -> y 
                    | _ -> x
      let newCellCoord r b
          = List.nth (boardEmpty b) r
      let emptyCell = boardEmpty movedBoard |> List.length |> rnum.Next 
      (insertNewCell value) (newCellCoord emptyCell movedBoard) movedBoard

type Board<'a when 'a : equality>(board: 'a board, moveDir: Direction -> 'a board -> 'a board, values: 'a*'a, size: int, win: 'a, show: 'a board -> string) = 
    member this.Board = board
    member this.MoveDir dir = Board(moveDir dir this.Board, moveDir, values, size, win, show)
    member this.InsertAtRandom rnum = Board(this.Board |> (insertAtRandom values rnum), moveDir, values, size, win, show)
    member this.HasNextMove = hasNextMove this.Board
    member this.IsWin = isWin win this.Board
    member this.Show = show this.Board
    override x.Equals(yobj) = 
        match yobj with
          | :? Board<'a> as y -> x.Board = y.Board
          | _ -> false 
    override x.GetHashCode() = hash x.Board
    static member construct (size: int) (values: 'a*'a) (win: 'a) (op: 'a -> 'a -> 'a) (show: 'a board -> string) : 'a Board =
        let size = size
        let board = List.init size (fun x -> List.init size (fun y -> Option<'a>.None))
        Board(board, moveDir op size, values, size, win, show)