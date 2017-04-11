# all txt files should be in a single folder, x = any file in that folder (function reads all of them in the folder)
# it also needs the number of loci
Lmmat <- function(x, loci, all=TRUE){
  x <- dirname(x)
  xz <- as.vector(list.files(x, pattern='txt'))
  x <- file.path(x,list.files(x, pattern='.txt'))
  zcc <- list()
  if (all==TRUE){
    loci2 <- loci + 1
  }
  else{
    loci2 <- loci
  }
  for (i in seq(1, loci2)){
    y <- readLines(x[[i]])
    lnum <- grep('Bezier', y)
    z <- unlist(strsplit(y[lnum], '[[:blank:]][[:blank:]]'))
    zz <- grep('[[:alnum:]]', z, value=TRUE)
    zc <- matrix(NA, nrow=length(xz), ncol=length(zz))
    rownames(zc) <- xz
    colnames(zc) <- zz
    if (i==loci+1){
      an <- 2
    }
    else{
      an <- 1
    }
    for (j in seq(1, length(x))){
      y <- readLines(x[[j]])
      lnum <- grep('Bezier', y)
      xa <- unlist(strsplit(y[lnum+an+i], '[[:blank:]]'))
      za <- grep('[[:alnum:]]', xa, value=TRUE)
      if (i==loci2){
        za[[1]] <- loci2
        zc[j,] <- as.numeric(za)
      }
      else{
        zc[j,] <- as.numeric(za)
      }
    }
    zcc[[i]] <- zc
  }
  zcc
}
