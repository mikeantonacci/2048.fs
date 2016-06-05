module main

open System
open FSharpx.Option
open FSharpx.Prelude
open _2048

let initInsert rand = insertAtRandom (2,4) rand

let initialize rnum = initInsert rnum >=> initInsert rnum <| start

let movePlus = moveDir (+)

let hjkl key 
    = match key with
        | ConsoleKey.H | ConsoleKey.A | ConsoleKey.LeftArrow -> Some Left
        | ConsoleKey.J | ConsoleKey.S | ConsoleKey.DownArrow -> Some Down 
        | ConsoleKey.K | ConsoleKey.W | ConsoleKey.UpArrow -> Some Up
        | ConsoleKey.L | ConsoleKey.D | ConsoleKey.RightArrow -> Some Right
        | _ -> None

let cellFormat x
    = match x with 
        | Some n -> sprintf "%-4i" n 
        | None -> "    "

let rowformat = List.map cellFormat >> List.reduce (fun x y -> x+"|"+y) 

let showBoard
    = printfn "%s" |> List.iter << List.map rowformat

let rec game (rnum : Random) board : unit
    = if not (hasNextMove board) then gameOver rnum true
      Console.Clear()
      showBoard board
      let key = Console.ReadKey().Key
      let movedBoard = movePlus (hjkl key) board
      let newBoard = if board <> movedBoard || board = start 
                     then insertAtRandom (2,4) rnum movedBoard 
                     else returnM movedBoard
      Console.Clear()
      showBoard movedBoard |> ignore
      Async.Sleep 15000 |> ignore
      Console.Clear()
      Option.iter showBoard newBoard |> ignore
      if isWin 2048 <!> newBoard |> getOrElse false 
      then gameOver rnum false
      game rnum <!> newBoard |> ignore

and gameOver (rand : Random) (b : bool) : unit
    = printfn "%s" (if b 
                    then "Game Over. Play Again? (y/n)" 
                    else "2048! Play Again? (y/n)")
      let key = Console.ReadKey().KeyChar
      Console.Clear()
      let cont = match key with
                   | 'y' -> game rand <!> (initialize rand) |> ignore
                   | 'n' -> Environment.Exit 0
                   | _ -> gameOver rand true
      ()

[<EntryPoint>]
let main argv = 
    printfn "%s" "Press any key to play."
    Console.ReadKey() |> ignore
    let rnum = new Random()
    game rnum <!> (initialize rnum) |> ignore
    0 // return an integer exit code
