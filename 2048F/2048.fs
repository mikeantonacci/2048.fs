namespace _2048

open FSharpx
open FSharpx.Collections
open FSharpx.Functional.Prelude
open Aether
open Aether.Operators

type Direction = Up | Left | Right |Down

module private Game =

    let merge (monoid: Monoid<'a>) xs =
        let mutable score = monoid.Zero()
        let (++) = curry monoid.Combine
        let rec merge' xs =
            match xs with
              | (x :: y :: xs) when x = y
                  -> score <- score ++ x ++ y
                     x ++ y :: merge' xs
              | (x :: xs) -> x :: merge' xs
              | x -> x
        List.map Some << merge' <| xs, score 

    let move m (row : 'a option list) =
        (List.fill row.Length None) ^% Optics.fst_ << merge m << List.choose id <| row

    let moveLeft m board =
        let board, scores = List.unzip << List.map (move m) <| board
        (board, List.reduce (curry m.Combine) scores)
    let moveRight m = 
        (List.map List.rev) ^% Optics.fst_ << (moveLeft m << List.map List.rev)
    let moveUp m =
        List.transpose ^% Optics.fst_ << moveLeft m << List.transpose
    let moveDown m =
        List.transpose ^% Optics.fst_ << moveRight m << List.transpose

    let moveDir m dir
        = match dir with
            | Left -> moveLeft m
            | Down -> moveDown m 
            | Up -> moveUp m 
            | Right -> moveRight m 

    let findOpenCells<'a when 'a : equality> : 'a option list list -> (int*int) list = 
        let rowEmpty (i : int) (ns : 'a option list) : (int*int) option list
            = List.mapi (fun j x -> match x with 
                                      | None -> Some (i, j) 
                                      | Some _ -> None) ns
        List.choose id << List.concat << List.mapi rowEmpty

    let insertNewCell (i,j) =
        Optic.set (List.pos_ i >?> List.pos_ j) << Some

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

type Board<'a when 'a : equality> private (board:'a option list list, values:'a*'a, size:int, win:'a, str:'a option list list -> string, rand:System.Random, score: 'a, monoid: Monoid<'a>) = 

    member val Board = board

    member this.Score = score

    member this.MoveDir dir =
        let moved,scoreDiff = Game.moveDir monoid dir board
        Board(moved, values, size, win, str, rand, monoid.Combine(score,scoreDiff),monoid)

    member this.InsertAtRandom
        = Board(Game.insertAtRandom values rand board, values, size, win, str, rand, score, monoid)

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

    static member construct(size:int, values:'a*'a, win:'a, m:Monoid<'a>, rand: System.Random, ?str:'a option list list -> string) : 'a Board =
        let board = List.init size (fun x -> List.init size (fun y -> Option<'a>.None))
        let toString = defaultArg str <| fun b -> b.ToString()
        Board(board, values, size, win, toString, rand, m.Zero(), m)