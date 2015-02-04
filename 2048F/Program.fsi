module _2048

type cell = int option
type row = cell list
type board = row list
val start : board
val SIZE : int
val ap : ('a -> 'b) option -> 'a option -> 'b option
val pad : int -> 'a option list -> 'a option list
val shift : int -> (row -> row)
val merge : row -> row
val move : (row -> row)
val transpose : board -> board
val moveRight : (board -> board)
val moveLeft : (board -> board)
val moveUp : (board -> board)
val moveDown : (board -> board)
val moveDir : char -> (board -> board)
val cellFormat : cell -> string
val rowformat : (row -> string)
val rowEmpty : int -> row -> (int*cell) list
val boardEmpty : (board -> (int*cell) list)
val replace : int -> cell -> (row -> row)
val newCell : (int * cell) -> int -> (board -> board)
val isWin : board -> bool
val newCellCoord : int -> board -> int*cell
val game : board -> System.Random -> unit
val gameOver : bool -> unit
val main : string[] -> int