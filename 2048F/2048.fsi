module _2048

type cell = int option
type row = cell list
type board = row list

val start : board
val SIZE : int

val nthOrNone : 'a list -> int -> 'a option
val pure' : 'a -> 'a option
val ap : ('a -> 'b) option -> 'a option -> 'b option
val (<*>) : (('a -> 'b) option -> 'a option -> 'b option)

val pad : int -> 'a option list -> 'a option list
val shift : (row -> row)
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

val replace : 'a -> int -> ('a list -> 'a list)
val newCell : int option -> (int * cell) -> (board -> row option list)

val isWin : board -> bool

val boardFull : (board -> bool)
val rowHasMerges : row -> bool
val boardHasMerges : board -> bool
val hasNextMove : board -> bool
val showBoard : (board -> unit)

val game : board -> System.Random -> unit
val gameOver : bool -> System.Random -> unit
val main : string[] -> int