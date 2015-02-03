type cell = int option
type row = cell list
type board = row list

let start : board = [[None;None;None;None];
                     [None;None;None;None];
                     [None;None;None;None];
                     [None;None;None;None];]
let SIZE : int = 4

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

let rec transpose board
    = match board with
        | [[];[];[];[]] -> []
        | _ -> List.map List.head board :: transpose (List.map List.tail board)

let moveRight = move |> List.map
let moveLeft = List.rev << move << List.rev |> List.map
let moveUp = transpose >> moveLeft >> transpose
let moveDown = transpose << moveRight << transpose

let cellFormat x
    = match x with 
        | Some n -> sprintf "%-4i" n 
        | None -> "    "

let rowformat = List.reduce (fun x y -> x+"|"+y) << List.map cellFormat

let rowEmpty n ns
    = List.mapi (fun i x -> match x with 
                              | None -> (n,Some i) 
                              | Some _ -> (n,None)) ns

let boardEmpty
    = (List.concat << List.mapi rowEmpty) >> List.filter (fun x -> match x with 
                                                                     | (_, Some _) -> true 
                                                                     | _ -> false)

let replace n m
    = List.mapi (fun i x -> if i=n 
                            then m 
                            else x)

let newCell (t: int*int option) k
    = List.mapi (fun i (x:int option list) -> if i = fst t 
                                              then replace (snd t).Value (Some k) x 
                                              else x) 

let rec game board (rnum:System.Random) : unit
    = do 
        let key = System.Console.ReadKey().KeyChar
        let dir = match key with
                    | 'h' -> moveLeft
                    | 'j' -> moveDown
                    | 'k' -> moveUp
                    | 'l' -> moveRight
                    | _ -> fun x -> x
        System.Console.Clear()
        let movedBoard = dir board
        let nxcl = (boardEmpty movedBoard).[rnum.Next <| (boardEmpty movedBoard |> List.length)]
        let k = if (rnum.Next 2) = 0 then 2 else 4
        let newBoard = if board <> movedBoard || board = start then newCell nxcl k movedBoard  else board
        ignore <| List.map (printfn "%s") (List.map rowformat movedBoard)
        Async.Sleep 10000 |> ignore
        System.Console.Clear()
        ignore <| List.map (printfn "%s") (List.map rowformat newBoard)
        game newBoard rnum

[<EntryPoint>]
let main argv = 
    let rnum = new System.Random()
    game start rnum    
    0 // return an integer exit code