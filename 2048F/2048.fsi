module _2048

type Direction = Up | Left | Right |Down

type Board<'a when 'a : equality> =
    class
        member Board : 'a option list list
        member MoveDir : Direction -> 'a Board
        member InsertAtRandom : System.Random -> 'a Board 
        member HasNextMove : bool
        member IsWin : bool
        static member construct: size: int * values: ('a * 'a) * win: 'a * op: ('a -> 'a -> 'a) * ?toString: ('a option list list -> string) -> Board<'a>
    end 