module _2048

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
        | Some _ -> match x with 
                              | Some _ -> Option.map f.Value x 
                              | None -> None
        | None -> None
let (<*>) = ap
let flip f x y = f y x
let (>>=) = flip Option.bind

let rec pad n xs = if List.length xs = n 
                   then xs 
                   else pad n (None::xs)

let shift  = List.filter Option.isSome 

let rec merge a 
    = match a with
        | [] -> []
        | [x] -> [x]
        | (x :: y :: xs) -> if x = y 
                            then (Option.map (+) x <*> y) :: merge xs 
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
                              | None -> (n,Some i) 
                              | Some _ -> (n,None)) ns

let boardEmpty
    = (List.concat << List.mapi rowEmpty) >> List.filter (fun x -> match x with 
                                                                     | (_, Some _) -> true 
                                                                     | _ -> false)

let replace m n
    = List.mapi (fun i x -> if i=n 
                            then m 
                            else x)

let newCell k (t: int*cell) 
    = List.mapi (fun i (x:int option list) -> if i = fst t 
                                              then Option.map replace <| Some k <*> snd t <*> pure' x 
                                              else pure' x) 

let newCellCoord r b
    = nthOrNone (boardEmpty b) r

let optionNewBoard board movedBoard i k
    = if board <> movedBoard || board = start
      then newCell (pure' k) |> Option.map <| (newCellCoord i movedBoard) <*> pure' movedBoard
      else pure' <| List.map pure' board
let newBoard board nextBoard : board option 
    = match nextBoard with
                        | Some _ -> if List.fold (fun x a -> x && Option.isSome a) true (Option.get nextBoard)
                                    then pure' <| List.map Option.get nextBoard.Value
                                    else pure' board
                        | None -> None


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

//IO
let showBoard : (board -> unit)
    = printfn <| "%s" |> List.iter << List.map rowformat

let insertAtRandom r board movedBoard
    =   let value = if r 9 = 0 then 4 else 2
        let space = r <| (boardEmpty movedBoard |> List.length)
        let nextBoard  = optionNewBoard board movedBoard space value
        let nb = newBoard board nextBoard
        nb

let rec game (rnum:System.Random) board : unit
    = do 
        if not <| hasNextMove board then rnum |> gameOver true
        System.Console.Clear()
        showBoard board
        let key = System.Console.ReadKey().KeyChar
        let movedBoard = key |> moveDir <| board
        let nb = insertAtRandom rnum.Next board movedBoard
        showBoard movedBoard
        Async.Sleep 15000 |> ignore
        System.Console.Clear()
        Option.iter showBoard nb
        if isWin <| Option.get nb then rnum |> gameOver false
        game rnum (Option.get nb) 

and gameOver b (rnum:System.Random) : unit
    = do 
        printfn "%s" (if b then "Game Over. Play Again? (y/n)" else "2048! Play Again? (y/n)")
        let key = System.Console.ReadKey().KeyChar
        System.Console.Clear()
        let initialInsert = insertAtRandom rnum.Next start
        let cont = match key with
                     | 'y' -> initialInsert start >>= initialInsert |> (Option.map <| game rnum) |> ignore
                     | 'n' -> System.Environment.Exit 0
                     | _ -> rnum |> gameOver true
        ()


[<EntryPoint>]
let main argv = 
    printfn "%s" "Hit any key to start."
    System.Console.ReadKey() |> ignore
    let rnum = new System.Random()
    let initialInsert = insertAtRandom rnum.Next start
    initialInsert start >>= initialInsert |> (Option.map <| game rnum) |> ignore
    0 // return an integer exit code