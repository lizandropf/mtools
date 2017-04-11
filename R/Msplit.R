#' Divide Bayes File All into separate txt files for input into Tracer or for further analysis

Msplit <- function(x, loci, rep, writef=FALSE, f){
  x <- readLines(x)
  fac <- loci*rep
  num <- grep('# @@@@@@@@', x)
  if(length(unlist(strsplit(x[num+1], '\t')))<length(unlist(strsplit(x[num+2], '\t')))){
    hedr <- unlist(strsplit(x[num+1], '\t'))
    xtable <- read.table(text=x, header=FALSE, sep='\t', skip=num+1)
    xtable <- xtable[,1:length(hedr)]
    colnames(xtable) <- hedr
  }
  else{
    xtable <- read.table(text=x, header=TRUE, sep='\t', skip=num)
  }
  xtable <- as.matrix(xtable)
  lnum <- length(xtable[,1])
  divnum <- lnum/fac
  divnum2 <- divnum
  xtable <- xtable[ order (xtable[,2], xtable[, 3]),]
  xlist <- list()
  k <- 0
  for (i in seq(1, lnum, by=divnum)){
    k <- k+1
    xlist[[k]] <- xtable[i:divnum2,]
    rownames(xlist[[k]]) <- c(1:length(xlist[[k]][,1]))
    divnum2 <- divnum2+divnum
  }
  if (!writef==FALSE){
    for (i in 1:length(xlist)){
      write.table(xlist[[i]],file=paste(f, paste(colnames(xlist[[i]])[[2]], xlist[[i]][1,2], colnames(xlist[[i]])[[3]], xlist[[i]][1,3], sep="_"), '.txt', sep=''), row.names=rownames(xlist[[i]]), col.names=colnames(xlist[[i]]), quote=FALSE, sep='\t', eol = "\r\n", fileEncoding = "utf8")
    }
    flist <- xlist
  }
  else {
    flist <- xlist
    nflist <- list()
    for (i in 1:length(flist)){
      nflist[[i]] <- paste(colnames(flist[[i]])[[2]], flist[[i]][1,2], colnames(flist[[i]])[[3]], flist[[i]][1,3], sep="_")
    }
    names(flist) <- unlist(nflist)
  }
  flist
}
