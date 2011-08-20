`she.plot` <-
function (X, pch = 20, pcol = 'black', pcex = 1, pbg = 'black', lcol = 'black',
  lwd = 1, lty = 'dotted', ylab = expression('ln'~italic(E)), bty = 'l', ...) {
  
  if (is.vector(X$N) == TRUE)
    vari = expression('ln'~italic(M))
  else
    vari = expression('ln'~italic(L))
  
  plot(log(X[[4]]), log(X$E), type = 'n', bty = bty, xlab = vari,
    ylab = ylab, ...)
  
  lines(log(X[[4]]), log(X$E), lty = lty, lwd = lwd, col = lcol)
  
  points(log(X[[4]]), log(X$E), pch = pch, col = pcol, bg = pbg, cex = pcex)
}
