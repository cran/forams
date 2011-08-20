`fi` <-
function (df, groups) {
  Ps <- NA
  Po <- NA
  Ph <- NA
  for(i in 1:ncol(df)) {
      Ps[i] <- as.matrix(sum(df[i][groups=='Ps']) / sum(df[i]))
  }                          
  for(i in 1:ncol(df)) {
      Po[i] <- as.matrix(sum(df[i][groups=='Po']) / sum(df[i]))
  }
  for(i in 1:ncol(df)) {
      Ph[i] <- as.matrix(sum(df[i][groups=='Ph']) / sum(df[i]))
  }
  FI <- as.matrix((10 * Ps) + Po + (2 * Ph))
  FI2 <- cbind(as.matrix(1:ncol(df)), FI)
  colnames(FI2) <- c('PlotOrder', 'FI')
  rownames(FI2) <- colnames(df)
  FI2 <- as.data.frame(FI2)
  class(FI2) <- "fi"
  return(FI2)
}

