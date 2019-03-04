library(colorspace)

testcol <- vector()


for(i in 0:199)
{
  testcol <- c(testcol, hex(mixcolor(i/199,polarLUV(70,50,30),polarLUV(70,50,120))))
  
}


testcol2 <- vector()


for(i in 0:199)
{
  testcol2 <- c(testcol2, hex(mixcolor(i/199,polarLUV(70,50,300),polarLUV(70,50,210))))
  
}


testcols <- vector()

for(i in 0:199)
{
  testcols <- rbind(testcols, hex(mixcolor(i/199,hex2RGB(testcol),hex2RGB(testcol2))))
  
}

testcols[rev(1:ncol(testcols)),]


plotcols <- function ( x) {
  require(colorspace)
  allcol <- diverge_hcl(100)
  x <- x + abs(min(x))
  stan_x <- floor((x/max(x)) * 99) + 1
  plotcol <-  allcol[stan_x]
  names(plotcol) <- names(x)
  return(plotcol)
}


plotcols2D <- function ( x) {
  require(colorspace)
  x[,1] <- x[,1] + abs(min(x[,1]))
  x[,2] <- x[,2] + abs(min(x[,2]))
  x <- x/max(x)
  
  x[,1] <- x[,1] - mean(range(x[,1])) + 0.5
  
  x[,2] <- x[,2] - mean(range(x[,2])) + 0.5
  
  
  plotcols <- vector()
  for(i in rownames(x))
  {
    plotcols[i] <- testcols[ceiling(x[i,1]*199)+1,ceiling(x[i,2]*199)+1]  
  }
  
  
  names(plotcols) <- rownames(x)
  return(plotcols)
}

plotcols2D_clust <- function ( x, clusts) {
  require(colorspace)
  x[,1] <- x[,1] + abs(min(x[,1]))
  x[,2] <- x[,2] + abs(min(x[,2]))
  x <- x/max(x)
  
  x[,1] <- x[,1] - mean(range(x[,1])) + 0.5
  
  x[,2] <- x[,2] - mean(range(x[,2])) + 0.5
  
  x <- as.data.frame(x)
  temp <- vector()
  for(i in unique(clusts)){
    i_sites <- names(clusts)[clusts == i]
    i_x <- x[i_sites,]
    i_mean <- colMeans(i_x)
    temp <- rbind(temp, i_mean)
  }
  
  rownames(temp) <- unique(clusts)
  
  plotcols <- vector()
  for(i in rownames(temp))
  {
    plotcols[i] <- testcols[ceiling(temp[i,1]*199)+1,ceiling(temp[i,2]*199)+1]  
  }
  names(plotcols) <- rownames(temp)
  
  
  
  allplotcols <- rep(0, nrow(x))
  names(allplotcols) <- rownames(x)
  
  for(i in names(allplotcols)){
    allplotcols[i] <- plotcols[clusts[i]]
    
  }
  
  allplotcols
  return(allplotcols)
}
