// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

let rec pad n xs = if List.length xs = n then xs else pad n (None::xs)

let shift n = pad n << List.filter (fun (x: int option) -> x <> None) 

let rec merge a = match a with
                    | [] -> []
                    | [x] -> [x]
                    | (x :: y :: xs) -> if x = y then None :: Option.map ((*) 2) x :: merge xs else x :: merge (y :: xs)

let move = shift 4 << List.rev << merge << List.rev << shift 4

let moveRight = List.map move
let moveLeft = List.map <| (List.rev << move << List.rev)

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let board = [[None;Some 2; Some 2; Some 4];
                 [Some 2; Some 4; Some 4; Some 8];
                 [None; Some 2; Some 2; None];
                 [Some 8; Some 4; Some 2; Some 2]]
    ignore <| List.map (printfn "%A") board 
    ignore <| List.map (printfn "%A") (List.map move board)
    ignore <| List.map (printfn "%A") (moveLeft board)
    ignore <| List.map (printfn "%A") (moveRight board)
    0 // return an integer exit code
