namespace _2048

open FSharpx.Collections.List
open Aether
open Aether.Operators

type Direction = Up | Left | Right |Down

module private __2048 =

    type 'a board = 'a option list list
     
    let rec merge f xs
        = match xs with
            | (x :: y :: xs) when x = y -> (f x y) :: merge f xs
            | (x :: xs) -> x :: merge f (xs)
            | x -> x

    let move f (row : 'a option list) : 'a option list
        = fill row.Length None << List.map Option.Some << merge f << List.choose id <| row

    let moveLeft f
        = List.map (move f)
    let moveRight f
        = List.map (List.rev << move f << List.rev)
    let moveUp f
        = transpose << moveLeft f << transpose
    let moveDown f
        = transpose << moveRight f << transpose

    let moveDir f dir
        = match dir with
            | Left -> moveLeft f
            | Down -> moveDown f 
            | Up -> moveUp f 
            | Right -> moveRight f 

    let findOpenCells<'a when 'a : equality> : 'a board -> (int*int) list = 
        let rowEmpty (i : int) (ns : 'a option list) : (int*int) option list
            = List.mapi (fun j x -> match x with 
                                      | None -> Some (i, j) 
                                      | Some _ -> None) ns
        List.choose id << List.concat << List.mapi rowEmpty

    let insertNewCell (i,j) =
        Optic.set (List.pos_ i >?> List.pos_ j) << Option.Some

    let rowHasMerges<'a when 'a : equality> : 'a option list -> bool
         = Seq.exists ((<||) (=)) << Seq.pairwise << List.toSeq

    let boardHasMerges board = 
        let hasRowMerges = List.exists id << List.map rowHasMerges
        hasRowMerges board || hasRowMerges <| transpose board 

    let insertAtRandom (x,y) (rnum: System.Random) board
        = let value = match rnum.Next 9 with
                        | 0 -> y 
                        | _ -> x
          let openCells = findOpenCells board
          let (i,j) = openCells.[rnum.Next openCells.Length] 
          insertNewCell (i,j) value board

open __2048

type Board<'a when 'a : equality> private (board:'a board, moveDir:Direction -> 'a board -> 'a board, values:'a*'a, size:int, win:'a, str:'a board -> string, rand:System.Random) = 

    member val Board = board

    member this.MoveDir dir
        = Board(moveDir dir board, moveDir, values, size, win, str, rand)

    member this.InsertAtRandom
        = Board(insertAtRandom values rand board, moveDir, values, size, win, str, rand)

    member this.HasNextMove
        = not (List.isEmpty << findOpenCells <| board) || (boardHasMerges board)

    member this.IsWin =
        List.exists id << List.map (List.exists ((=) <| Some win)) <| board

    override this.ToString() = str board

    override x.Equals(yobj) = 
        match yobj with
          | :? Board<'a> as y -> x.Board = y.Board
          | _ -> false 

    override x.GetHashCode() = hash x.Board

    static member construct(size:int, values:'a*'a, win:'a, op:'a -> 'a -> 'a, rand: System.Random, ?str:'a board -> string) : 'a Board =
        let board = List.init size (fun x -> List.init size (fun y -> Option<'a>.None))
        let toString = defaultArg str <| fun b -> b.ToString()
        Board(board, moveDir op, values, size, win, toString, rand)