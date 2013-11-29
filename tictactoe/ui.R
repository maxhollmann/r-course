source('ttt.R')

library(tcltk)
tt = tktoplevel()
tktitle(tt) = "Tic Tac Toe"

# overwrite progressbar functions to use TclTk
createProgressBar <- function(max) {
  tkProgressBar(
    title = "Progress",
    label = "Evaluating moves...",
    min   = 0,
    max   = max
  )
}
increaseProgressBar <- function(bar) {
  setTkProgressBar(bar, getTkProgressBar(bar) + 1)
}

# return the sign of the player, or a dot for no player
playerSign <- function(player) {
  if (player == -1) return('O')
  if (player ==  1) return('X')
  return('')
}

# callback for buttons
buttonPressed <- function(r, c) {
  if (attr(humanMove, "waiting") == TRUE) {
    attr(humanMove, "move") <<- c(r, c)
  }
}

# callback for moves in the game
humanMove <- function(board, player) {
  tktitle(tt) = paste0("Tic Tac Toe - ", playerSign(player), "'s turn")
  attr(humanMove, "waiting") <<- TRUE
  attr(humanMove, "move") <<- NULL
  while (is.null(attr(humanMove, "move"))) {
    Sys.sleep(0.1)
  }
  return(attr(humanMove, "move"))
}

updateBoard <- function(board) {
  sapply(1:3, function(row) { 
    sapply(1:3, function(col) { 
      btn <- tkbutton(
        tt,
        text = playerSign(board[row, col]),
        command = function() buttonPressed(row, col),
        width = 10,
        height = 6
      ) 
      tkgrid(btn, row = row, column = col) 
    })
  })
}

finished <- function(winner) {
  if (winner == 0) {
    winner = "Nobody"
  } else {
    winner = playerSign(winner)
  }
  tclvalue(tkmessageBox(
    title = "Game over",
    message = paste0(winner, " wins!\nPlay again?"),
    icon = "info",
    type = "yesno"
  )) == 'yes'
}

player1.human = tclvalue(tkmessageBox(
  title = "Who's O?",
  message = "Yes for human, no for computer",
  icon = "info",
  type = "yesno"
)) == 'yes'
player2.human = tclvalue(tkmessageBox(
  title = "Who's X?",
  message = "Yes for human, no for computer",
  icon = "info",
  type = "yesno"
)) == 'yes'

tkfocus(tt)
continue = TRUE
while (continue) {
  continue = play(
    if(player1.human) humanMove else NULL,
    if(player2.human) humanMove else NULL, 
    updateBoard.func = updateBoard,
    finish.func = finished
  )
}
tkdestroy(tt)
