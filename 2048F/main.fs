module main

open System
open FSharpx.Option
open _2048

let initInsert rnum (board: 'a Board) = board.InsertAtRandom rnum

let initialize rnum = initInsert rnum << initInsert rnum 

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

    let rowFormat: int option list -> string
        = String.concat "|" << List.map cellFormat

    String.concat "\n" << List.map rowFormat 

[<EntryPoint>]
let main argv = 
    let start = Board.construct 4 (2,4) 2048 (+) boardFormat

    let rec game (rnum : Random) (board : 'a Board) : unit =
          do
              if not board.HasNextMove then gameOver rnum true start
              Console.Clear()
              printfn "%s" board.Show
              let key = Console.ReadKey().Key
              let movedBoard = board.MoveDir <!> (hjkl key) |> getOrElse board
              Console.Clear()
              printfn "%s" movedBoard.Show
              let newBoard
                  = if board <> movedBoard
                    then 
                        do async {do! Async.Sleep 100} |> Async.RunSynchronously
                        movedBoard.InsertAtRandom rnum
                    else movedBoard
              Console.Clear()
              printfn "%s" newBoard.Show
              if newBoard.IsWin 
              then gameOver rnum false start
              else game rnum newBoard

    and gameOver (rand : Random) (b : bool) start : unit
        = do printfn "%s" (if b 
                           then "Game Over. Play Again? (y/n)" 
                           else "2048! Play Again? (y/n)")
          let key = Console.ReadKey().KeyChar
          do
              Console.Clear()
              match key with
                | 'y' -> game rand <| initialize rand start
                | 'n' -> Environment.Exit 0
                | _ -> gameOver rand true start
          ()

    do
        printfn "%s" "Press any key to play."
        Console.ReadKey() |> ignore
    let rnum = new Random()
    do game rnum (initialize rnum start)
    0 // return an integer exit code