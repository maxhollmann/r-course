source('ttt.R')

# Draws a single sign on the board (can be 'X' or 'O', otherwise nothing will be drawn)
drawSign <- function(x, y, sign) {
  size = 0.309 # golden ratio, yo!
  lwd  = 10
  if (sign == 'X') {
    lines(c(x - size, x + size), c(y - size, y + size), lwd = lwd, col = "darkred")
    lines(c(x + size, x - size), c(y - size, y + size), lwd = lwd, col = "darkred")
  } else if (sign == 'O') {
    symbols(x, y, circles = size, lwd = lwd, fg = "darkgreen", inches = FALSE, add = TRUE)
  }
}

# Draws the board in it's current state, and indicates who's turn it is in the title of the plot
drawBoard <- function(board, player = 0) {
  plot(NA, xlim=c(0, 3), ylim=c(3, 0), main=paste0('The Board - ', playerSign(player), "'s turn"), xlab='', ylab='', asp=1, axes=FALSE)
  
  # black out the illegal area
  rect(-9999, -9999, 9999, 9999, col = 'black')
  rect(0, 0, 3, 3, col = 'white')
  
  # draw grid
  abline(0, 0, h = 1:2, v = 1:2, lwd = 5)
  
  for (row in 1:3) {
    for (col in 1:3) {
      drawSign(col - .5, row - .5, playerSign(board[row, col]))
    }
  }
}

# return the sign of the player
playerSign <- function(player) {
  if (player == -1) return('O')
  if (player ==  1) return('X')
  return('')
}


# callback for moves in the game
humanMove <- function(board, player) {
  repeat {
    loc <- locator(1)
    
    # convert information in "loc" to the variables "row" and "column".
    # Hint: take a look at ?floor and ?ceiling and ?round - which is most helpful?
    row = ceiling(loc$y)
    column = ceiling(loc$x)
    move = c(row, column)
    
    # only return the move when it is valid
    if (row %in% 1:3 && column %in% 1:3 && isPossible(board, move)) return(move)
  }
}

updateBoard <- function(board, player) {
  drawBoard(board, player)
}

finished <- function(winner) {
  if (winner == 0) {
    winner = "Nobody"
  } else {
    winner = playerSign(winner)
  }
  
  # Ask user if he wants to play again
  plot(NA, xlim=c(-1, 1), ylim=c(-1, 1), main=paste0(winner, " wins! Play again?"), xlab='', ylab='', asp=1, axes=FALSE)
  
  # make a pretty frame
  rect(-9999, -9999, 9999, 9999, col = 'black')
  rect(-1, -1, 1, 1, col = 'white')
  
  text(0, .5, labels = "Yes")
  text(0, -.5, labels = "No")
  abline(0, 0, lwd = 5)
  
  y = locator(1)$y
  return(y > 0) # return TRUE if yes was clicked -> play returns TRUE -> while loop keeps running
}

# Returns 'human' if user selects human, 'ai' if user selects computer
playerSettingsMenu <- function(player) {
  plot(NA, xlim=c(-1, 1), ylim=c(-1, 1), main=paste0('Who is playing ', playerSign(player), "?"), xlab='', ylab='', asp=1, axes=FALSE)
  # make a pretty frame
  rect(-9999, -9999, 9999, 9999, col = 'black')
  rect(-1, -1, 1, 1, col = 'white')
  
  text(0, .5, labels = "Human")
  text(0, -.5, labels = "Computer")
  abline(0, 0, lwd = 5)
  
  y = locator(1)$y
  return(ifelse(y > 0, 'human', 'ai'))
}

player1.human = playerSettingsMenu(-1) == 'human'
player2.human = playerSettingsMenu(1)  == 'human'

while (play(
  player1.func     = if(player1.human) humanMove else NULL,
  player2.func     = if(player2.human) humanMove else NULL, 
  updateBoard.func = updateBoard,
  finish.func      = finished
)) {}

