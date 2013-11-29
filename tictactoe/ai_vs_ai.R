# clear workspace to make sure the script doesn't depend on old stuff
rm(list = ls())

source('ttt.R')

# ask the user to make a move
humanMove <- function() {
  move = c()
  while (length(move) != 2 || any(move < 1) || any(move > 3)) {
    move = readline("What's your move? [row col] ")
    move = strsplit(move, ' ')[[1]]
    move = as.integer(move)
  }
  return(move)
}

winners = list()

for (n in 1:20) {
  # sink(paste0("game", n, ".txt"))
  board = matrix(ncol = 3, byrow = TRUE, data = c(
    0,  0,  0,
    0,  1,  0,
    0,  0,  0
  ))
  player = -1
  printBoard(board)
  
  while (winner(board) == 0 && !isFull(board)) {
    if (FALSE) {
      move = humanMove()
    } else {
      move = randomMove(bestMoves(board, player))
    }
    board = makeMove(board, player, move)
    printBoard(board)
    player = -player
  }
  winners[[n]] = winner(board)
  # sink()
}

print(winners)
