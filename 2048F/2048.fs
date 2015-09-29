﻿module _2048

open System
open FSharpx.Prelude
open FSharpx.Option

type 'a cell = 'a option
type 'a row = 'a cell list
type 'a board = 'a row list

let SIZE : int = 4
let start<'a>
    = List.init SIZE (fun x -> List.init SIZE (fun y -> Option<'a>.None))

let nthOrNone (l : 'a list) n = if n < l.Length then Some (List.nth l n) else None

let rec pad n xs = if List.length xs = n 
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

let move f
    = pad SIZE << List.rev << merge f << List.rev << shift

let rec transpose board
    = match board with
        | xs when List.concat xs = [] -> []
        | _ -> List.map List.head board :: transpose (List.map List.tail board)

let moveRight f = move f |> List.map
let moveLeft f = List.rev << move f << List.rev |> List.map
let moveUp f = transpose >> moveLeft f >> transpose
let moveDown f = transpose << moveRight f << transpose

type Direction = Up | Left | Right |Down

let hjkl key 
    = match key with
        | ConsoleKey.H | ConsoleKey.A | ConsoleKey.LeftArrow -> Some Left
        | ConsoleKey.J | ConsoleKey.S | ConsoleKey.DownArrow -> Some Down 
        | ConsoleKey.K | ConsoleKey.W | ConsoleKey.UpArrow -> Some Up
        | ConsoleKey.L | ConsoleKey.D | ConsoleKey.RightArrow -> Some Right
        | _ -> None

let moveDir f dir 
    = match dir with
        | Some Left -> moveLeft f
        | Some Down -> moveDown f
        | Some Up -> moveUp f
        | Some Right -> moveRight f
        | None -> id

let cellFormat x
    = match x with 
        | Some n -> sprintf "%-4i" n 
        | None -> "    "

let rowformat = List.map cellFormat >> List.reduce (fun x y -> x+"|"+y) 

//n param expects the i from List.mapi in boardEmpty, builds the coordinates of the empty cells this way
let rowEmpty (n:int) ns
    = List.mapi (fun i x -> match x with 
                              | None -> Some (n, i) 
                              | Some _ -> None) ns
let boardEmpty<'a when 'a : equality> (xs : 'a option list list)
    = (List.concat << List.mapi rowEmpty) xs |> List.filter (Option.isSome)

//replace the nth element with m, used to insert new cell
let replace n m
    = List.mapi (fun i x -> if i=n 
                            then m 
                            else x)

let insertNewCell<'a> (k:'a) t
    = List.mapi (fun i (x:'a option list) -> if i = fst t 
                                             then replace (snd t) (Some k) x 
                                             else x)

let newCellCoord r b
    = nthOrNone (boardEmpty b) r |> concat 

let isWin win x = not <| List.isEmpty (List.choose (List.tryFind ((=) <| Some win)) x)

let boardFull<'a when 'a : equality> :('a option list list -> bool)
    = List.isEmpty << boardEmpty

let rec rowHasMerges row
    = match row with
        | [] -> false
        | [x] -> false
        | (x :: y :: xs) -> if x = y then true else rowHasMerges (y::xs)

let rec boardHasMerges b 
    = List.exists ((=) true) (List.map rowHasMerges <| b) 
    || List.exists ((=) true) (List.map rowHasMerges <| transpose b)

let hasNextMove b = not (boardFull b) || (boardHasMerges b)

let insertAtRandom (x,y) (rnum : Random) board movedBoard
    = let value = if rnum.Next 9 = 0
                  then y
                  else x
      let emptyCell = boardEmpty movedBoard |> List.length |> rnum.Next 
      if board <> movedBoard || board = start
      then insertNewCell value <!> (newCellCoord emptyCell movedBoard) <*> returnM movedBoard
      else returnM board

let showBoard
    = printfn "%s" |> List.iter << List.map rowformat

let initInsert rand = insertAtRandom (2,4) rand start

let initialize rnum = initInsert rnum >=> initInsert rnum <| start

let rec game (rnum : Random) board : unit
    = if not (hasNextMove board) then gameOver rnum true
      Console.Clear()
      showBoard board
      let key = Console.ReadKey().Key
      let movedBoard = moveDir (+) (hjkl key) board
      let newBoard = insertAtRandom (2,4) rnum board movedBoard 
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
