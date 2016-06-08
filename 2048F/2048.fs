module _2048

open FSharpx.Option
open Aether
open Aether.Operators

let fill = FSharpx.Collections.List.fill
let transpose = FSharpx.Collections.List.transpose

type 'a row = 'a option list
type 'a board = 'a option list list
type Direction = Up | Left | Right |Down
 
let rec merge f xs
    = match xs with
        | [] -> []
        | [x] -> [x]
        | (x :: y :: xs) -> if x = y 
                            then (f <!> x <*> y) :: merge f xs
                            else x :: merge f (y :: xs)

let move f (row : 'a row) : 'a row
    = fill row.Length None << merge f << List.filter Option.isSome <| row

let moveLeft f
    = move f |> List.map
let moveRight f
    = List.rev << move f << List.rev |> List.map
let moveUp f
    = transpose >> moveLeft f >> transpose
let moveDown f
    = transpose << moveRight f << transpose

let moveDir f dir
    = match dir with
        | Left -> moveLeft f
        | Down -> moveDown f 
        | Up -> moveUp f 
        | Right -> moveRight f 

//n param expects the i from List.mapi in boardEmpty, builds the coordinates of the empty cells this way
//Option.get must be safe here because of the Option.isSome
let findOpenCells<'a when 'a : equality> : 'a board -> (int*int) list = 
    let rowEmpty (i : int) (ns : 'a row) : (int*int) option list
        = List.mapi (fun j x -> match x with 
                                  | None -> Some (i, j) 
                                  | Some _ -> None) ns
    List.map Option.get << List.filter Option.isSome << List.concat << List.mapi rowEmpty

let insertNewCell (i,j) =
    Optic.set (List.pos_ i >?> List.pos_ j) << returnM

let isWin win =
    List.reduce (||) << List.map (List.exists ((=) <| Some win))

let boardFull<'a when 'a : equality> : 'a board -> bool
    = List.isEmpty << findOpenCells

let rec rowHasMerges row
    = match row with
        | [] -> false
        | [x] -> false
        | (x :: y :: xs) -> if x = y then true else rowHasMerges (y::xs)

let rec boardHasMerges board
    = List.reduce (||) (List.map rowHasMerges <| board) 
    || List.reduce (||) (List.map rowHasMerges <| transpose board)

let hasNextMove b = not (boardFull b) || (boardHasMerges b)

let insertAtRandom (x,y) (rnum: System.Random) board
    = let value = match rnum.Next 9 with
                    | 0 -> y 
                    | _ -> x
      let openCells = findOpenCells board
      let newCell = openCells.[rnum.Next openCells.Length] 
      insertNewCell newCell value board

type Board<'a when 'a : equality>(board:'a board, moveDir:Direction -> 'a board -> 'a board, values:'a*'a, size:int, win:'a, str:'a board -> string) = 
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

    static member construct(size:int, values:'a*'a, win:'a, op:'a -> 'a -> 'a, ?str:'a board -> string) : 'a Board =
        let size = size
        let board = List.init size (fun x -> List.init size (fun y -> Option<'a>.None))
        let toString = defaultArg str <| fun b -> b.ToString()
        Board(board, moveDir op, values, size, win, toString)