// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

type board = int option list list

let start : board = [[None;None;None;None];
                     [None;None;None;None];
                     [None;None;None;None];
                     [None;None;None;None];]

let rec pad n xs = if List.length xs = n then xs else pad n (None::xs)
let shift n = pad n << List.filter (fun (x: int option) -> x <> None) 
let rec merge a 
    = match a with
        | [] -> []
        | [x] -> [x]
        | (x :: y :: xs) -> if x = y 
                            then None :: Option.map ((*) 2) x :: merge xs 
                            else x :: merge (y :: xs)
let move = shift 4 << List.rev << merge << List.rev << shift 4
let rec transpose board 
    = match board with
        | [[];[];[];[]] -> []
        | _ -> (List.map List.head board) :: transpose (List.map List.tail board)
let moveRight = List.map <| move
let moveLeft = List.map <| (List.rev << move << List.rev)
let moveDown = transpose << moveRight << transpose
let moveUp = transpose << moveLeft << transpose
let optToString x 
    = match x with 
        | Some n -> sprintf "%-4i" n 
        | None -> "    "
let rowformat = List.reduce (fun x y -> x+"|"+y) << List.map optToString
let rowEmpty (n:int) (ns : int option list) 
    = List.mapi (fun i x -> match x with 
                              | None -> (n,Some i) 
                              | Some _ -> (n,None)) ns
let boardEmpty 
    = (List.concat << List.mapi rowEmpty) >> List.filter (fun x -> match x with 
                                                                     | (_, Some _) -> true 
                                                                     | _ -> false)
let rec replace n (m:int option) = List.mapi (fun i x -> if i=n 
                                                         then m 
                                                         else x)
let newCell (t: int*int option)  
    = List.mapi (fun i (x:int option list) -> if i = fst t 
                                              then replace (snd t).Value (Some 2) x 
                                              else x) 
let rec game board (rnum:System.Random)
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
        let newBoard = if board <> movedBoard || board = start then newCell nxcl movedBoard else board
        ignore <| List.map (printfn "%s") (List.map rowformat newBoard)
        game newBoard rnum
        ()



[<EntryPoint>]
let main argv = 
    let rnum = new System.Random()
    game start rnum    
    0 // return an integer exit code
