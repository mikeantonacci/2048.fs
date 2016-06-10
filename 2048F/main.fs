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
            | Some n -> sprintf "%6i" n 
            | None -> "      "
    let surCat s b = (fun x -> s + b + x + b + s) << String.concat (b + s + b)
    surCat "+------+------+------+------+" "\n" << List.map ((surCat "|" "") << List.map cellFormat)

let updateScreen board = do
    Console.Clear()
    printfn "%s" <| board.ToString() 

[<EntryPoint>]
let main argv = 
    let start = Board.construct(size=4,values=(2,4),win=2048,op=(+),str=boardFormat,rand=System.Random())

    let rec game (board : 'a Board) : unit =
          do
              if not board.HasNextMove then gameOver true
              updateScreen board
              let key = Console.ReadKey().Key
              let movedBoard = board.MoveDir <!> (hjkl key) |> getOrElse board
              updateScreen movedBoard
              let newBoard
                  = if board <> movedBoard
                    then 
                        do async {do! Async.Sleep 100} |> Async.RunSynchronously
                        movedBoard.InsertAtRandom
                    else movedBoard
              updateScreen newBoard
              match newBoard.IsWin with
                | true -> gameOver false
                | false -> game newBoard

    and gameOver (b : bool) : unit
        = do 
            printfn "%s" (if b 
                          then "Game Over. Play Again? (y/n)" 
                          else "2048! Play Again? (y/n)")
            let key = Console.ReadKey().Key
            Console.Clear()
            match key with
              | ConsoleKey.Y -> game start.InsertAtRandom.InsertAtRandom
              | ConsoleKey.N -> Environment.Exit 0
              | _ -> gameOver true

    do
        printfn "%s" "Press any key to play."
        Console.ReadKey() |> ignore
        game start.InsertAtRandom.InsertAtRandom
    0 // return an integer exit code