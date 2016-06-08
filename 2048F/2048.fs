module _2048

open System
open FSharpx.Option
open Aether
open Aether.Operators
let transpose = FSharpx.Collections.List.transpose

type 'a cell = 'a option
type 'a row = 'a cell list
type 'a board = 'a row list
type Direction = Up | Left | Right |Down
 
let rec leftpad n xs
    = if List.length xs = n 
      then xs 
      else leftpad n (None::xs)

let rec merge f xs
    = match xs with
        | [] -> []
        | [x] -> [x]
        | (x :: y :: xs) -> if x = y 
                            then (f <!> x <*> y) :: merge f xs
                            else x :: merge f (y :: xs)

let move f size : 'a row -> 'a row
    = leftpad size << List.rev << merge f << List.rev << List.filter Option.isSome

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
let findOpenCells<'a when 'a : equality> : 'a option list list -> (int*int) list = 
    let rowEmpty (i:int) (ns: 'a option list): (int*int) option list
        = List.mapi (fun j x -> match x with 
                                  | None -> Some (i, j) 
                                  | Some _ -> None) ns
    List.map Option.get << List.filter Option.isSome << List.concat << List.mapi rowEmpty

let insertNewCell k (i,j): 'a list list -> 'a list list =
    k ^= (List.pos_ i >?> List.pos_ j)

let isWin win =
    List.reduce (||) << List.map (List.exists ((=) <| Some win))

let boardFull<'a when 'a : equality> :('a board -> bool)
    = List.isEmpty << findOpenCells

let rec rowHasMerges (row: 'a list): bool
    = match row with
        | [] -> false
        | [x] -> false
        | (x :: y :: xs) -> if x = y then true else rowHasMerges (y::xs)

let rec boardHasMerges (b: 'a board)
    = List.reduce (||) (List.map rowHasMerges <| b) 
    || List.reduce (||) (List.map rowHasMerges <| transpose b)

let hasNextMove b = not (boardFull b) || (boardHasMerges b)

let insertAtRandom (x,y) (rnum: Random) (board: 'a board) : 'a board
    = let value = match rnum.Next 9 with
                    | 0 -> Some y 
                    | _ -> Some x
      let openCells = findOpenCells board
      let newCell = openCells.[rnum.Next openCells.Length] 
      insertNewCell value newCell board

type Board<'a when 'a : equality>(board: 'a board, moveDir: Direction -> 'a board -> 'a board, values: 'a*'a, size: int, win: 'a, str: 'a board -> string) = 
    member this.Board = board

    member this.MoveDir dir
        = Board(moveDir dir this.Board, moveDir, values, size, win, str)

    member this.InsertAtRandom rnum
        = Board(insertAtRandom values rnum this.Board, moveDir, values, size, win, str)

    member this.HasNextMove = hasNextMove this.Board

    member this.IsWin = isWin win this.Board

    override this.ToString() = str this.Board

    override x.Equals(yobj) = 
        match yobj with
          | :? Board<'a> as y -> x.Board = y.Board
          | _ -> false 

    override x.GetHashCode() = hash x.Board

    static member construct((size: int), (values: 'a*'a), (win: 'a), (op: 'a -> 'a -> 'a), (?str: 'a board -> string)) : 'a Board =
        let size = size
        let board = List.init size (fun x -> List.init size (fun y -> Option<'a>.None))
        let toString = defaultArg str <| fun b -> b.ToString()
        Board(board, moveDir op size, values, size, win, toString)