module _2048

type cell = int option
type row = cell list
type board = row list

val start : board
val SIZE : int

val moveDir : char -> (board -> board)

val isWin : board -> bool
val boardFull : (board -> bool)
val hasNextMove : board -> bool
val insertAtRandom : (int -> int) -> board -> board -> board option 

val showBoard : (board -> unit)

val game : System.Random -> board ->  unit
val gameOver : System.Random -> bool -> unit
val main : string[] -> int