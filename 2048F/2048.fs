namespace _2048

open FSharpx.Collections
open Aether
open Aether.Operators

type Direction = Up | Left | Right |Down

module private Game =

    let rec merge f xs
        = match xs with
            | (x :: y :: xs) when x = y -> (f x y) :: merge f xs
            | (x :: xs) -> x :: merge f (xs)
            | x -> x

    let move f (row : 'a option list) : 'a option list
        = List.fill row.Length None << List.map Option.Some << merge f << List.choose id <| row

    let moveLeft f
        = List.map (move f)
    let moveRight f
        = List.map (List.rev << move f << List.rev)
    let moveUp f
        = List.transpose << moveLeft f << List.transpose
    let moveDown f
        = List.transpose << moveRight f << List.transpose

    let moveDir f dir
        = match dir with
            | Left -> moveLeft f
            | Down -> moveDown f 
            | Up -> moveUp f 
            | Right -> moveRight f 

    let findOpenCells<'a when 'a : equality> : 'a option list list -> (int*int) list = 
        let rowEmpty (i : int) (ns : 'a option list) : (int*int) option list
            = List.mapi (fun j x -> match x with 
                                      | None -> Some (i, j) 
                                      | Some _ -> None) ns
        List.choose id << List.concat << List.mapi rowEmpty

    let insertNewCell (i,j) =
        Optic.set (List.pos_ i >?> List.pos_ j) << Option.Some

    let rowHasMerges<'a when 'a : equality> : 'a option list -> bool
         = Seq.exists <| (<||) (=) << Seq.pairwise << List.toSeq

    let boardHasMerges board = 
        let hasRowMerges = List.exists id << List.map rowHasMerges
        hasRowMerges board || hasRowMerges <| List.transpose board 

    let insertAtRandom (a,b) (rnum: System.Random) board
        = let value = match rnum.Next 9 with
                        | 0 -> b 
                        | _ -> a
          let openCells = findOpenCells board
          let (i,j) = openCells.[rnum.Next openCells.Length] 
          insertNewCell (i,j) value board

type Board<'a when 'a : equality> private (board:'a option list list, moveDir:Direction -> 'a option list list -> 'a option list list, values:'a*'a, size:int, win:'a, str:'a option list list -> string, rand:System.Random) = 

    member val Board = board

    member this.MoveDir dir
        = Board(moveDir dir board, moveDir, values, size, win, str, rand)

    member this.InsertAtRandom
        = Board(Game.insertAtRandom values rand board, moveDir, values, size, win, str, rand)

    member this.HasNextMove
        = not (List.isEmpty << Game.findOpenCells <| board) || (Game.boardHasMerges board)

    member this.IsWin =
        List.exists id << List.map (List.exists ((=) <| Some win)) <| board

    override this.ToString() = str board

    override x.Equals(yobj) = 
        match yobj with
          | :? Board<'a> as y -> x.Board = y.Board
          | _ -> false 

    override x.GetHashCode() = hash x.Board

    static member construct(size:int, values:'a*'a, win:'a, op:'a -> 'a -> 'a, rand: System.Random, ?str:'a option list list -> string) : 'a Board =
        let board = List.init size (fun x -> List.init size (fun y -> Option<'a>.None))
        let toString = defaultArg str <| fun b -> b.ToString()
        Board(board, Game.moveDir op, values, size, win, toString, rand)