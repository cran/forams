`abc.plot` <-
function (X = NULL, xlim = c(0, ceiling(log(length(X$Accum.Abund)))),
  ylim = c(0, 100), yaxp = c(0, 100, 10), lty.bio = 'dotted',
  lty.abu = 'solid', lwd = 2, col.bio = 'black', col.abu = 'black',
  xlab = expression('Species Rank'~(Log[e]~Scale)),
  ylab = 'Cumulative Dominance %', leg = TRUE, W = TRUE, col.pol = '#f5f5f5',
  ...) {
  
  plot(x = log(1:length(X$Accum.Abund)), y = X$Accum.Abund, type = 'n',
    xlim = xlim, ylim = ylim, ylab = ylab, xlab = xlab, axes = FALSE, ...)
  
  polygon(x = c(log(1:length(X$Accum.Biomass)),
    sort(log(1:length(X$Accum.Abund)), decreasing=TRUE),
    log(1:length(X$Accum.Biomass))[1]),
    y = c(sort(X$Accum.Biomass, decreasing=FALSE),
    sort(X$Accum.Abund, decreasing=TRUE), sort(X$Accum.Biomass)[1]),
    col = col.pol, border = NA)
  
  lines(x = log(1:length(X$Accum.Biomass)), y = sort(X$Accum.Biomass),
    lty = lty.bio, lwd = lwd, col = col.bio)
  
  lines(x = log(1:length(X$Accum.Abund)), y = X$Accum.Abund, lty = lty.abu,
    lwd = lwd, col = col.abu)
  
  axis(side = 1, at = 0:ceiling(log(length(X$Accum.Abund))),
    labels = round(exp(0:ceiling(log(length(X$Accum.Abund))))))
  
  axis(side = 2, yaxp = yaxp)
  
  if (W == TRUE)
    legend(x = 'topleft', legend = paste('W =', X$W.Stat, sep=' '), bty = 'n')
  else (W == FALSE)
    legend(x = 'topleft', legend='', bty = 'n')
  
  if (leg == TRUE)
    legend(x = 'bottomright', legend = c('Biomass', 'Abundance'), lwd = lwd,
      lty = c(lty.bio, lty.abu), bty = 'n', col = c(col.bio, col.abu))
  else (leg == FALSE)
    legend(x = 'bottomright', legend = '', bty = 'n')
}
