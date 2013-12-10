###############################################################################
# NOTES
# 
# Moves are represented as vectors in the format c(row, column), where row and
# column are integers between 1 and 3.
# 
# Player 1 is -1 or O, player 2 is 1 or X.
###############################################################################


###############################################################################
# Functions to check who won
winnerSum <- function(sums) {
  if (any(sums == 3))  return(3)
  if (any(sums == -3)) return(-3)
  return(0)
}
winnerRow <- function(m) {
  s = rowSums(m)
  return(winnerSum(s))
}
winnerCol <- function(m) {
  s = colSums(m)
  return(winnerSum(s))
}
winnerDiag <- function(m) {
  s = c(
    sum(diag(m)),
    
    # rotate matrix by 90 degrees clockwise
    # code from http://stackoverflow.com/questions/16496210/rotate-a-matrix-in-r
    sum(diag(t(apply(m, 2, rev))))
  )
  return(winnerSum(s))
}
winner <- function(m) {
  w = c(winnerRow(m), winnerCol(m), winnerDiag(m))
  if (any(w == -3)) return(-1)
  if (any(w == 3))  return(1)
  return(0)
}
isFull <- function(m) {
  return(!any(m == 0))
}
###############################################################################

createProgressBar <- function(max) {
  txtProgressBar(
    min   = 0,
    max   = max,
    style = 3 # mark ends with '|', show percentage
  )
}
increaseProgressBar <- function(bar) {
  setTxtProgressBar(bar, getTxtProgressBar(bar) + 1)
}

# returns TRUE if the move is possible, FALSE otherwise
isPossible <- function(board, move) {
  return(board[move[1], move[2]] == 0)
}

# returns all possible moves
possibleMoves <- function(board) {
  moves = list()
  for (r in 1:3) {
    for (c in 1:3) {
      move = c(r, c)
      # add move if field is free
      if (isPossible(board, move)) {
        moves[[length(moves) + 1]] = move
      }
    }
  }
  return(moves)
}

# returns the board after the move
makeMove <- function(board, player, move) {
  r = move[1]
  c = move[2]
  if(!isPossible(board, move)) {
    stop(paste("Field", r, c, "is not free!"))
  }
  
  board[r, c] = player
  return(board)
}

# Returns TRUE if the player has a fork on the board
# A fork means that the player has at least two fields on which he would win with the next move,
# meaning that the opponent can't prevent it.
hasFork <- function(board, player) {
}

# Returns the value of the given move. Higher values indicate better moves.
moveScore <- function(board, player, move, depth = 0) {
  # Multiplicating the scores by 10^(9-depth) makes sure that good moves are executed as
  # soon as possible, since depth is increased by 1 in each layer of recursion.
  
  prog = attr(moveScore, 'progress')
  if (!is.null(prog)) {
    increaseProgressBar(prog)
  }
  
  # new board after the move is executed
  nb = makeMove(board, player, move)
  
  # new board after the opponent executes this move
  nb.opp = makeMove(board, -player, move)
  
  # if the board is full after this move, we don't have a choice anyways, so just return 0
  if (isFull(nb)) {
    return(0)
  }
  
  # if player would win with this move, that deserves the highest score
  if (winner(nb) == player) {
    return(9 * 10^(9-depth))
  }
  
  # if opponent could win on this field in the next move, block field
  # slightly lower score than above, because winning is more important than preventing loss
  if (winner(nb.opp) == -player) {
    return(8 * 10^(9-depth))
  }
  
  # TODO give 7 to creating a fork, 6 to preventing one
  
  # when the above doesn't result in a score, determine what the 
  best = -Inf
  for (move in possibleMoves(nb)) {
    # good move for other player is bad for this one, so we negate the score
    val = -moveScore(nb, -player, move, depth+1)
    best = max(val, best)
  }
  return(best)
}

# returns the best of all possible moves
bestMoves <- function(board, player) {
  best = -Inf
  moves = list()
  scores = matrix(0, 3, 3)
  
  if (length(possibleMoves(board)) == 9) {
    # speeds things up
    return(list(
      c(1, 2),
      c(2, 3),
      c(3, 2),
      c(2, 1)
    ))
  }
  
  # show a progress bar when we have to analyze more than 1000 possibilities
  possibilities = ceiling(factorial(length(possibleMoves(board))) * 2/3)
  if (possibilities > 1000) {
    attr(moveScore, 'progress') <<- createProgressBar(possibilities)
  }
  
  for (move in possibleMoves(board)) {
    score = moveScore(board, player, move)
    scores[move[1], move[2]] = score
    #cat(r, c, "->", score, "\n")
    if (score > best) {
      # replace previously best moves with just the current one, because it has a higher score
      best = score
      moves = list(move)
    } else if (score == best) {
      # add the current move to the best ones, because has the same score
      moves[[length(moves) + 1]] = move
    }
  }
  
  prog = attr(moveScore, 'progress')
  if (!is.null(prog)) {
    close(prog)
    attr(moveScore, 'progress') <<- NULL
  }
  # print(scores)
  return(moves)
}

# returns one of the moves at random
randomMove <- function(moves) {
  return(moves[[sample(1:length(moves), 1)]])
}

# return the sign of the player, or a dot for no player
playerSign <- function(player) {
  if (player == -1) return('o')
  if (player ==  1) return('x')
  return('.')
}

# print the board in a prettier way than just showing the matrix
printBoard <- function(board) {
  for (r in 1:3) {
    for (c in 1:3) {
      cat(playerSign(board[r, c]), " ")
    }
    cat("\n")
  }
  cat("\n")
}

# Plays a game of tic tac toe
# player1 is -1, player2 is 1
# When playerX.func is NULL, the AI plays for this player
# Returns the return value of finish.func if present, otherwise NULL
play <- function(player1.func = NULL, player2.func = NULL, updateBoard.func = NULL, finish.func = NULL, min.delay = 1) {
  board = matrix(ncol = 3, byrow = TRUE, data = c(
    0,  0,  0,
    0,  0,  0,
    0,  0,  0
  ))
  
  player = sample(c(-1, 1), 1)
  if(!is.null(updateBoard.func)) updateBoard.func(board, player)
  
  while (winner(board) == 0 && !isFull(board)) {
    if (player == -1 && !is.null(player1.func)) {
      move = player1.func(board, player)
    } else if (player == 1 && !is.null(player2.func)) {
      move = player2.func(board, player)
    } else {
      start.t = proc.time()['elapsed']
      move = randomMove(bestMoves(board, player))
      delta.t = proc.time()['elapsed'] - start.t
      if (delta.t < min.delay) {
        Sys.sleep(min.delay - delta.t) # make sure the AI takes a certain amount of time to move
      }
    }
    board = makeMove(board, player, move)
    player = -player
    if(!is.null(updateBoard.func)) updateBoard.func(board, player)
  }
  
  if(!is.null(finish.func)) return(finish.func(winner(board)))
  return(NULL)
}
