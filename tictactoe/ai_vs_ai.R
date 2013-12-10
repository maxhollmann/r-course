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

updateBoard <- function(board, player) {
  printBoard(board)
}

winners = list("-1" = 0, "0" = 0, "1" = 0)

for (n in 1:200) {
  #sink(paste0("game", n, ".txt"))
  continue = play(
    updateBoard.func = updateBoard,
    finish.func = function(winner) {
      sink("winners.txt", append = TRUE)
      cat(winner, "\n")
      sink()
    },
    min.delay = 0
  )
  #sink()
}

print(winners)
