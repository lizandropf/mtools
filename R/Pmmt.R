# lestimate either 'Raw Thermodynamic score(1a)', 'Bezier approximated score(1b)', 'Harmonic mean(2)'
Pmmt <- function(mat, lestimate="Bezier"){
  if (lestimate== "Bezier"){
    lestimate <- "Bezier approximated score(1b)"
  }
  else if (lestimate== "Thermo"){
    lestimate <- "Raw Thermodynamic score(1a)"
  }
  else if (lestimate== "Hmean"){
    lestimate <- " Harmonic mean(2)"
  }
  xmat <- matrix(NA, nrow=nrow(mat[[length(mat)]]), ncol=length(mat))
  rownames(xmat) <- rownames(mat[[length(mat)]])
  colnames(xmat) <- colnames(xmat, do.NULL=FALSE, prefix='Locus ')
  for (i in seq(1, length(mat))){
    mxnum <- max(mat[[i]][,lestimate])
    for (j in seq(1, nrow(mat[[i]]))){
      xmat[[j,i]] <- exp(mat[[i]][[j,lestimate]]-mxnum)
    }
    xsum <- sum(xmat[,i])
    for (k in seq(1, nrow(mat[[i]]))){
      xmat[k,i] <- xmat[k,i]/xsum
    }
  }
  xmat
}
