`abc` <-
function (df) {
    Abundance <- df[order(df[, 1], decreasing=TRUE), ][1]
    Biomass <- df[order(df[, 2], decreasing=TRUE), ][2]
    PercAbun <- NA
    PercAbun[1] <- Abundance[1,] / sum(Abundance) * 100
    for (i in 2:nrow(Abundance)) {
      PercAbun[i] <- (Abundance[i,] / sum(Abundance)) * 100 + PercAbun[i-1]
    }
    PercAbun <- data.frame(PercAbun)
    colnames(PercAbun) <- 'Accum.Abund'
    rownames(PercAbun) <- rownames(Abundance)
    PercBio <- NA
    PercBio[1] <- Biomass[1,] / sum(Biomass) * 100
    for (i in 2:nrow(Biomass)) {
      PercBio[i] <- (Biomass[i,] / sum(Biomass)) * 100 + PercBio[i-1]
    }
    PercBio <- data.frame(PercBio)
    colnames(PercBio) <- 'Accum.Biomass'
    rownames(PercBio) <- rownames(Biomass)
    BiAi <- NA
    for(i in 2:nrow(df)) {
      BiAi[i] <- PercBio[i,] - PercAbun[i,]
    }
    abc <- cbind(PercAbun, PercBio[match(rownames(PercAbun), rownames(PercBio)),])
    abc[3] <- abc[2] - abc[1]
    colnames(abc) <- c('Accum.Abund', 'Accum.Biomass', 'BiAi')
    rownames(abc) <- rownames(PercAbun)
    W<- function (df) {
      return(round(sum(df$BiAi) / (50 * (nrow(df) - 1)), 4))
    }
    W2<- W(abc)
    class(abc) <- "abc"
    abc$W.Stat <- W2
    return(abc)
}
