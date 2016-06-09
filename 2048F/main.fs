module main

open System
open FSharpx.Option
open _2048

let hjkl key : Direction option 
    = match key with
        | ConsoleKey.H | ConsoleKey.A | ConsoleKey.LeftArrow -> Some Left
        | ConsoleKey.J | ConsoleKey.S | ConsoleKey.DownArrow -> Some Down 
        | ConsoleKey.K | ConsoleKey.W | ConsoleKey.UpArrow -> Some Up
        | ConsoleKey.L | ConsoleKey.D | ConsoleKey.RightArrow -> Some Right
        | _ -> None

let boardFormat =
    let cellFormat x : string
        = match x with 
            | Some n -> sprintf "%-4i" n 
            | None -> "    "

    String.concat "\n" << List.map (String.concat "|" << List.map cellFormat)

[<EntryPoint>]
let main argv = 
    let start = Board.construct(size=4,values=(2,4),win=2048,op=(+),str=boardFormat,rand=System.Random())

    let rec game (rnum : Random) (board : 'a Board) : unit =
          do
              if not board.HasNextMove then gameOver rnum true start
              Console.Clear()
              printfn "%s" <| board.ToString()
              let key = Console.ReadKey().Key
              let movedBoard = board.MoveDir <!> (hjkl key) |> getOrElse board
              Console.Clear()
              printfn "%s" <| movedBoard.ToString()
              let newBoard
                  = if board <> movedBoard
                    then 
                        do async {do! Async.Sleep 100} |> Async.RunSynchronously
                        movedBoard.InsertAtRandom
                    else movedBoard
              Console.Clear()
              printfn "%s" <| newBoard.ToString()
              if newBoard.IsWin
              then gameOver rnum false start
              else game rnum newBoard

    and gameOver (rand : Random) (b : bool) start : unit
        = do 
            printfn "%s" (if b 
                          then "Game Over. Play Again? (y/n)" 
                          else "2048! Play Again? (y/n)")
            let key = Console.ReadKey().KeyChar
            Console.Clear()
            match key with
              | 'y' -> game rand start.InsertAtRandom.InsertAtRandom
              | 'n' -> Environment.Exit 0
              | _ -> gameOver rand true start.InsertAtRandom.InsertAtRandom

    do
        printfn "%s" "Press any key to play."
        Console.ReadKey() |> ignore
        let rnum = Random()
        game rnum start.InsertAtRandom.InsertAtRandom
    0 // return an integer exit code