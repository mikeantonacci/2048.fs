module _2048

open System

type cell = int option
type row = cell list
type board = row list

let SIZE : int = 4
let start : board 
    = List.init SIZE (fun x -> List.init SIZE (fun y -> None))

let nthOrNone (l : 'a list) n = if n < l.Length then Some (List.nth l n) else None

let pure' a = Some a
let ap (f : ('a -> 'b) option) (x : 'a option) : 'b option
    = match f with 
        | Some _ -> match x with | Some _ -> Option.map f.Value x | None -> None
        | None -> None
let (<*>) = ap
let flip f x y = f y x
let (>>=) = flip Option.bind
let concat = id |> Option.bind

let rec pad n xs = if List.length xs = n 
                   then xs 
                   else pad n (None::xs)

let shift  = List.filter (fun (x: int option) -> x <> None) 

let rec merge a 
    = match a with
        | [] -> []
        | [x] -> [x]
        | (x :: y :: xs) -> if x = y 
                            then Option.map ((*) 2) x :: merge xs 
                            else x :: merge (y :: xs)

let move : row -> row 
    = pad SIZE << List.rev << merge << List.rev << shift

let rec transpose board : board
    = match board with
        | [[];[];[];[]] -> []
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

let newCell k (t: int*int) 
    = List.mapi (fun i (x:int option list) -> if i = fst t 
                                               then replace (snd t) (Some k) x 
                                               else x)

let newCellCoord r b
    = concat <| nthOrNone (boardEmpty b) r

let isWin x = not (List.choose (List.tryFind (fun x -> x = Some 2048)) x).IsEmpty


//if board is full and row contains no possible merges and trasposed rows contain no possible merges, game over
let boardFull : (board -> bool)
    = List.isEmpty << boardEmpty

let rec rowHasMerges (row : row) : bool  
    = match row with
        | [] -> false
        | [x] -> false
        | (x :: y :: xs) -> if x = y then true else rowHasMerges (y::xs)

let rec boardHasMerges b 
    = List.exists (fun x -> x=true) (((List.map rowHasMerges) <| b) @ (List.map rowHasMerges) (transpose b))

let hasNextMove b = not (boardFull b) || (boardHasMerges b)

let insertNewCell rnum board movedBoard
    = let value = if rnum 9 = 0 then 4 else 2
      let emptyCell = rnum <| (boardEmpty movedBoard |> List.length)
      if board <> movedBoard || board = start
      then Option.map (newCell value) (emptyCell |> newCellCoord <| movedBoard) <*> pure' movedBoard
      else pure' board

//IO
let showBoard : (board -> unit)
    = printfn <| "%s" |> List.iter << List.map rowformat

let initialInsert rand = insertNewCell rand start

let rec game (rnum : Random) board : unit
    = do 
        if not <| hasNextMove board then rnum |> gameOver <| true
        Console.Clear()
        showBoard board
        let key = Console.ReadKey().KeyChar
        let movedBoard = key |> moveDir <| board
        let newBoard = insertNewCell rnum.Next board movedBoard
        Console.Clear()
        showBoard movedBoard
        Async.Sleep 15000 |> ignore
        Console.Clear()
        Option.iter showBoard newBoard
        if isWin <| newBoard.Value then rnum |> gameOver <| false
        game rnum newBoard.Value

and gameOver (rnum : Random) (b : bool) : unit
    = do 
        printfn "%s" (if b then "Game Over. Play Again? (y/n)" else "2048! Play Again? (y/n)")
        let key = Console.ReadKey().KeyChar
        Console.Clear()
        let cont = match key with
                     | 'y' -> initialInsert rnum.Next start >>= initialInsert rnum.Next |> (Option.map <| game rnum) |> ignore
                     | 'n' -> Environment.Exit 0
                     | _ -> gameOver rnum true
        ()


[<EntryPoint>]
let main argv = 
    printfn "%s" "Press any key to play."
    Console.ReadKey() |> ignore
    let rnum = new Random()
    initialInsert rnum.Next start >>= initialInsert rnum.Next |> (Option.map <| game rnum) |> ignore
    game rnum start
    0 // return an integer exit code