# lestimate either 'Raw Thermodynamic score(1a)', 'Bezier approximated score(1b)' or 'Harmonic mean(2)'
BFmt <- function(mat, lestimate="Bezier"){
  if (lestimate== "Bezier"){
    lestimate <- "Bezier approximated score(1b)"
  }
  else if (lestimate== "Thermo"){
    lestimate <- "Raw Thermodynamic score(1a)"
  }
  else if (lestimate== "Hmean"){
    lestimate <- " Harmonic mean(2)"
  }
  y <- list()
  namc <- rownames(mat[[length(mat)]])
  xmat <- matrix(NA, nrow=nrow(mat[[length(mat)]]), ncol=length(mat))
  colnames(xmat) <- seq(1, length(mat))
  rownames(xmat) <- namc
  for (i in 1:length(mat)){
    minval <- min(mat[[i]][,lestimate])
    for (j in seq(1, nrow(mat[[i]]))){
      xmat[j,i] <- exp(minval-as.numeric(mat[[i]][[j,lestimate]]))
    }
    y <- xmat
  }
  y
}
