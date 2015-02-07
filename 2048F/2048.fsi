module _2048

type cell = int option
type row = cell list
type board = row list

val start : board
val SIZE : int

val moveDir : char -> (board -> board)

val insertNewCell : int option -> (int * cell) -> (board -> row option list)

val isWin : board -> bool
val boardFull : (board -> bool)
val hasNextMove : board -> bool

val showBoard : (board -> unit)

val game : System.Random -> board -> unit
val gameOver : bool -> System.Random -> unit