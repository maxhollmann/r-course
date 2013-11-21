rm(list = ls())

# just for convenience, adds 'add = TRUE' to the symbols call
sym <- function(...) {
  args <- list(...)
  args$add <- TRUE
  
  do.call('symbols', args)
}

# rgb with values from 0 to 255
rgb2 <- function(r, g, b, alpha) {
  rgb(r/255, g/255, b/255, alpha)
}

plot(NA, xlim = c(-500, 500), ylim = c(-500, 500), axes = FALSE, xlab = "", ylab = "")
# plot(1, 1)

# background
# sym(0, 0, rectangles = matrix(c(1000, 1000), ncol = 2), bg = 'black')

# circles
sym(0, 0, circles = 1, bg = rgb2(227, 219, 186), fg = NULL)
sym(-100, -50, circles = 50)
