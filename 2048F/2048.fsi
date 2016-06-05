module _2048

open System

type Direction = Up | Left | Right |Down

type Board<'a when 'a : equality> =
    class
        member MoveDir : Direction -> 'a Board
        member InsertAtRandom : Random -> 'a Board 
        member HasNextMove : bool
        member IsWin : bool
        member Show : string
        static member construct: size: int -> values: ('a * 'a) -> win: 'a -> op: ('a -> 'a -> 'a) -> show: ('a option list list -> string) -> Board<'a>
    end 