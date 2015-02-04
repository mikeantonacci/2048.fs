module _2048

type cell = int option
type row = cell list
type board = row list

let start : board = [[None;None;None;None];
                     [None;None;None;None];
                     [None;None;None;None];
                     [None;None;None;None];]
let SIZE : int = 4


let nthOrNone (l : 'a list) n = if n < l.Length then Some (List.nth l n) else None

let pure' a = Some a
let ap (f : ('a -> 'b) option) (x : 'a option) : 'b option
    = match f with 
        | Some _ -> match x with | Some _ -> Option.map f.Value x | None -> None
        | None -> None
let (<*>) = ap

let rec pad n xs = if List.length xs = n 
                   then xs 
                   else pad n (None::xs)

let shift n = pad n << List.filter (fun (x: int option) -> x <> None) 

let rec merge a 
    = match a with
        | [] -> []
        | [x] -> [x]
        | (x :: y :: xs) -> if x = y 
                            then None :: Option.map ((*) 2) x :: merge xs 
                            else x :: merge (y :: xs)

let move : row -> row 
    = (shift <| SIZE) << List.rev << merge << List.rev << (SIZE |> shift)

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

let replace n (m : cell)
    = List.mapi (fun i x -> if i=n 
                            then m 
                            else x)

let newCell k (t: int*cell) 
    = List.mapi (fun i (x:int option list) -> if i = fst t 
                                              then replace (snd t).Value (Some k) x 
                                              else x) 

let newCellCoord r b
    = nthOrNone (boardEmpty b) r

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

let rec game board (rnum:System.Random) : unit
    = do 
        if not <| hasNextMove board then rnum |> gameOver true
        let key = System.Console.ReadKey().KeyChar
        System.Console.Clear()
        let movedBoard = key |> moveDir <| board
        //if ((boardEmpty movedBoard).Length = 0) then gameOver <| true
        let k = if (rnum.Next 9) = 0 then 4 else 2
        let i = rnum.Next <| (boardEmpty movedBoard |> List.length)
        let newBoard = if board <> movedBoard || board = start
                       then (newCell k) |> Option.map <| (i |> newCellCoord <| movedBoard) <*> pure' movedBoard
                       else pure' board
        List.iter (printfn "%s") (List.map rowformat movedBoard)
        Async.Sleep 10000 |> ignore
        System.Console.Clear()
        List.iter (printfn "%s") (List.map rowformat newBoard.Value)
        if isWin newBoard.Value then rnum |> gameOver false
        game newBoard.Value rnum

and gameOver b (rnum:System.Random) : unit
    = do 
        //System.Console.Clear()
        System.Console.WriteLine(if b then "Game Over.  Play Again? (y/n)" else "2048! Play Again? (y/n)")
        let key = System.Console.ReadKey().KeyChar
        let cont = match key with
                     | 'y' -> game start rnum
                     | 'n' -> System.Environment.Exit 0
                     | _ -> rnum |> gameOver true
        ()


[<EntryPoint>]
let main argv = 
    let rnum = new System.Random()
    game start rnum    
    0 // return an integer exit code