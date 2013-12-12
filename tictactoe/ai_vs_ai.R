# clear workspace to make sure the script doesn't depend on old stuff
rm(list = ls())

source('ttt.R')

for (n in 1:200) {
  continue = play(
    finish.func = function(winner) {
      sink("winners.txt", append = TRUE)
      cat(winner, "\n")
      sink()
    },
    min.delay = 0,
    first = 1
  )
}
