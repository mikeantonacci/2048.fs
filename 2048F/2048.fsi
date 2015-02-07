module _2048

open System

type cell = int option
type row = cell list
type board = row list

val start : board
val SIZE : int

val moveDir : char -> (board -> board)

val isWin : board -> bool
val boardFull : (board -> bool)
val hasNextMove : board -> bool
val insertAtRandom : Random -> board -> board -> board option 

val showBoard : (board -> unit)

val game : Random -> board ->  unit
val gameOver : Random -> bool -> unit
val main : string[] -> int