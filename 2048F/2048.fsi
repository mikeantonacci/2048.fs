module _2048

open System

type 'a cell = 'a option
type 'a row = 'a cell list
type 'a board = 'a row list

val moveDir<'a when 'a : equality> : ('a -> 'a -> 'a) -> char -> ('a board -> 'a board)

val is2048 : int board -> bool
val boardFull<'a when 'a : equality> :('a option list list -> bool)
val hasNextMove<'a when 'a : equality> : 'a board -> bool
val insertAtRandom : Random -> int board -> int board -> int board option 

val showBoard : (int board -> unit)

val game : Random -> int board ->  unit
val gameOver : Random -> bool -> unit
val main : string[] -> int