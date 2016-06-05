module _2048

open System

type 'a board = 'a option list list
type Direction = Up | Left | Right |Down

type Board<'a when 'a : equality> =
    class
        member Board : 'a board option
        member MoveDir : Direction option -> 'a Board
        member InsertAtRandom : Random -> 'a Board 
        member HasNextMove : bool
        member IsWin : bool
        static member constructBoard : size: int -> values: ('a * 'a) -> win: 'a -> op: ('a -> 'a -> 'a) -> Board<'a>
    end 