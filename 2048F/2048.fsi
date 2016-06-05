module _2048

open System

type 'a cell = 'a option
type 'a row = 'a cell list
type 'a board = 'a row list
type Direction = Up | Left | Right |Down

val start<'a when 'a : equality> : 'a board

val moveDir<'a when 'a : equality>
    : ('a -> 'a -> 'a) -> Direction option -> ('a board -> 'a board)

val boardFull<'a when 'a : equality>
    : ('a option list list -> bool)

val hasNextMove<'a when 'a : equality>
    : 'a board -> bool

val isWin<'a when 'a : equality> : 'a -> 'a board -> bool

val insertAtRandom<'a when 'a : equality>
    : ('a*'a) -> Random -> 'a board -> 'a board option