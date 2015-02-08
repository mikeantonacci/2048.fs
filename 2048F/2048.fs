module _2048

open System
open FSharpx.Prelude
open FSharpx.Option

type cell = int option
type row = cell list
type board = row list

let SIZE : int = 4
let start : board 
    = List.init SIZE (fun x -> List.init SIZE (fun y -> None))

let nthOrNone (l : 'a list) n = if n < l.Length then Some (List.nth l n) else None

let rec pad n xs = if List.length xs = n 
                   then xs 
                   else pad n (None::xs)

let shift  = List.filter Option.isSome

let rec merge f xs
    = match xs with
        | [] -> []
        | [x] -> [x]
        | (x :: y :: xs) -> if x = y 
                            then (f <!> x <*> y) :: merge f xs
                            else x :: merge f (y :: xs)

let move : row -> row 
    = pad SIZE << List.rev << merge (+) << List.rev << shift

let rec transpose board : board
    = match board with
        | xs when List.concat xs = [] -> []
        | _ -> List.map List.head board :: transpose (List.map List.tail board)

let moveRight = move |> List.map
let moveLeft = List.rev << move << List.rev |> List.map
let moveUp = transpose >> moveLeft >> transpose
let moveDown = transpose << moveRight << transpose

let moveDir key 
    = match key with
        | 'h' -> moveLeft
        | 'j' -> moveDown
        | 'k' -> moveUp
        | 'l' -> moveRight
        | _ -> id

let cellFormat x
    = match x with 
        | Some n -> sprintf "%-4i" n 
        | None -> "    "

let rowformat = List.map cellFormat >> List.reduce (fun x y -> x+"|"+y) 

let rowEmpty (n:int) (ns : row)
    = List.mapi (fun i x -> match x with 
                              | None -> Some (n, i) 
                              | Some _ -> None) ns

let boardEmpty
    = (List.concat << List.mapi rowEmpty) >> List.filter (fun x -> match x with 
                                                                     | Some _ -> true 
                                                                     | None -> false)

let replace n (m : cell)
    = List.mapi (fun i x -> if i=n 
                            then m 
                            else x)

let insertNewCell k (t: int*int) 
    = List.mapi (fun i (x:int option list) -> if i = fst t 
                                               then replace (snd t) (Some k) x 
                                               else x)

let newCellCoord r b
    = concat <| nthOrNone (boardEmpty b) r

let isWin x = not (List.choose (List.tryFind (fun x -> x = Some 2048)) x).IsEmpty

let boardFull : (board -> bool)
    = List.isEmpty << boardEmpty

let rec rowHasMerges (row : row) : bool  
    = match row with
        | [] -> false
        | [x] -> false
        | (x :: y :: xs) -> if x = y then true else rowHasMerges (y::xs)

let rec boardHasMerges b 
    = List.exists ((=) true) (List.map rowHasMerges <| b) 
    || List.exists ((=) true) (List.map rowHasMerges <| transpose b)

let hasNextMove b = not (boardFull b) || (boardHasMerges b)

let insertAtRandom (rnum : Random) board movedBoard
    = let value = if rnum.Next 9 = 0 then 4 else 2
      let emptyCell = rnum.Next <| (boardEmpty movedBoard |> List.length)
      if board <> movedBoard || board = start
      then insertNewCell value <!> (emptyCell |> newCellCoord <| movedBoard) <*> returnM movedBoard
      else returnM board

let showBoard : (board -> unit)
    = printfn <| "%s" |> List.iter << List.map rowformat

let initInsert rand : (board -> board option) = insertAtRandom rand start

let rec game (rnum : Random) board : unit
    = if not <| hasNextMove board then rnum |> gameOver <| true
      Console.Clear()
      showBoard board
      let key = Console.ReadKey().KeyChar
      let movedBoard = moveDir key board
      let newBoard = insertAtRandom rnum board movedBoard
      Console.Clear()
      showBoard movedBoard
      Async.Sleep 15000 |> ignore
      Console.Clear()
      Option.iter showBoard newBoard
      if isWin <!> newBoard |> getOrElse false 
      then rnum |> gameOver <| false
      game rnum <!> newBoard |> ignore
        

and gameOver (rand : Random) (b : bool) : unit
    = printfn "%s" (if b 
                    then "Game Over. Play Again? (y/n)" 
                    else "2048! Play Again? (y/n)")
      let key = Console.ReadKey().KeyChar
      Console.Clear()
      let cont = match key with
                   | 'y' -> game rand <!> (initInsert rand >=> initInsert rand <| start) |> ignore
                   | 'n' -> Environment.Exit 0
                   | _ -> gameOver rand true
      ()


[<EntryPoint>]
let main argv = 
    printfn "%s" "Press any key to play."
    Console.ReadKey() |> ignore
    let rnum = new Random()
    game rnum <!> (initInsert rnum >=> initInsert rnum <| start) |> ignore
    0 // return an integer exit code