source('ttt.R')

createBoard <- function(board, player = 0) {
  plot(NA, xlim=c(0, 3), ylim=c(3, 0), main=paste('The Board -', 'Player', playerSign(player)), xlab='', ylab='', asp=1, axes=TRUE)
  # Add the vertical and horizontal lines.
  # Hint: take a look at ?abline
  
  # black out the illegal area
  rect(-9999, -9999, 9999, 9999, col = 'black')
  rect(0, 0, 3, 3, col = 'white')
  
  # draw grid
  abline(0, 0, h = 1:2, v = 1:2)
  
  for (row in 1:3) {
    for (col in 1:3) {
      points(col - .5, row - .5, pch = playerSign(board[row, col]), cex = 5)
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
    if (row %in% 1:3 && column %in% 1:3 && isPossible(board, move)) break # only exit the loop when row and column are valid
  }
  
  # then 
  return(move)
}

updateBoard <- function(board, player) {
  createBoard(board, player)
}

finished <- function(winner) {
  if (winner == 0) {
    winner = "Nobody"
  } else {
    winner = playerSign(winner)
  }
  
  plot(NA, xlim=c(-1, 1), ylim=c(-1, 1), main=paste0(winner, " wins! Play again?"), xlab='', ylab='', asp=1, axes=TRUE)
  # make a pretty frame
  rect(-9999, -9999, 9999, 9999, col = 'black')
  rect(-1, -1, 1, 1, col = 'white')
  
  text(0, .5, labels = "Yes")
  text(0, -.5, labels = "No")
  abline(0, 0)
  
  y = locator(1)$y
  return(y > 0)
}

# Returns 'human' if user selects human, 'ai' if user selects computer
playerSettingsMenu <- function(player) {
  plot(NA, xlim=c(-1, 1), ylim=c(-1, 1), main=paste0('Who is playing ', playerSign(player), "?"), xlab='', ylab='', asp=1, axes=TRUE)
  # make a pretty frame
  rect(-9999, -9999, 9999, 9999, col = 'black')
  rect(-1, -1, 1, 1, col = 'white')
  
  text(0, .5, labels = "Human")
  text(0, -.5, labels = "Computer")
  abline(0, 0)
  
  y = locator(1)$y
  return(ifelse(y > 0, 'human', 'ai'))
}

player1.human = playerSettingsMenu(-1) == 'human'
player2.human = playerSettingsMenu(1) == 'human'

continue = TRUE
while (continue) {
  continue = play(
    if(player1.human) humanMove else NULL,
    if(player2.human) humanMove else NULL, 
    updateBoard.func = updateBoard,
    finish.func = finished,
    first = 1
  )
}

