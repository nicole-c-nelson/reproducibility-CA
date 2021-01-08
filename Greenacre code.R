library(ca)

data(author)
author.ca <- ca(author)
nsim      <- 100       # number of simulations

# compute row sums
author.rowsum <- apply(author, 1, sum)

# compute nsim simulations of first book
author.sim <- rmultinom(nsim, author.rowsum[1], prob = author[1,])

# compute nsim simulations of other books and column-bind
for (i in 2:12) {
  author.sim <- cbind(author.sim, rmultinom(nsim, author.rowsum[i], 
                                            prob = author[i,]))
}

# transpose to have same format as original matrix
author.sim  <- t(author.sim)
author.sim2 <- matrix(rep(0,12*nsim*26), nrow = 12*nsim)

# reorganize rows so that matrices are together
for (k in 1:nsim) { 
  for (i in 1:12) {
    author.sim2[(k-1)*12+i,] <- author.sim[k+(i-1)*nsim,]
  }
}

# get standard coordinates for rows
author.rowsc <- author.ca$rowcoord[,1:2]

# calculate principal coordinates of all replicates using transition formula
author.colsim <- t(t(author.rowsc)%*%author.sim2[1:12,])/
  apply(author.sim2[1:12,], 2, sum)
for (k in 2:nsim) author.colsim <- rbind(author.colsim,
                                         t(t(author.rowsc)%*%author.sim2[((k-1)*12+1):(k*12),])/
                                           apply(author.sim2[((k-1)*12+1):(k*12),], 2, sum))

# reorganize rows of coordinates so that letters are together
author.colsim2 <- matrix(rep(0,26*nsim*2), nrow = 26*nsim)
for (j in 1:26) { 
  for (k in 1:nsim) {
    author.colsim2[(j-1)*nsim+k,] <- author.colsim[j+(k-1)*26,]
  }
}

# plot all points (use first format of coords for labelling...)
plot(author.colsim[,1], -author.colsim[,2], xlab = "dim1", ylab = "dim2", 
     type = "n")
letters <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o",
             "p","q","r","s","t","u","v","w","x","y","z")
text(author.colsim[,1], -author.colsim[,2], letters, cex = 0.8, col = "gray")

# plot convex hulls for each letter
# first calculate principal coordinates of letters for original matrix
author.col <- t(t(author.rowsc)%*%author)/apply(author, 2, sum)
for (j in 1:26) {
  points <- author.colsim2[(nsim*(j-1)+1):(nsim*j),]
  # note we are reversing second coordinate in all these plots
  points[,2] <- -points[,2]
  hpts       <- chull(points)
  hpts       <- c(hpts,hpts[1])
  lines(points[hpts,], lty = 3, lwd = 2)
  text(author.col[j,1], -author.col[j,2], letters[j], font = 2, cex = 1.5)
}

# peeling a new convex hull (here we peel until just under 5% are removed)
plot(author.colsim2[,1], -author.colsim2[,2], xlab = "dim1", ylab = "dim2", 
     type = "n")
for (j in 1:26) {
  points <- author.colsim2[(nsim*(j-1)+1):(nsim*j),]
  # note we are reversing second coordinate in all these plots
  points[,2] <- -points[,2]
  repeat {
    hpts <- chull(points)
    npts <- nrow(points[-hpts,])   #next number of points in peeled set
    if(npts/nsim < 0.95) break
    points <- points[-hpts,]
  }
  hpts <- c(hpts,hpts[1])
  lines(points[hpts,], lty = 3)
  text(author.col[j,1], -author.col[j,2], letters[j], font = 2)
}


# I tried using this code on our data, but after looking at it closely I realized it's not going to work
# It's built on the idea that you have many samples of the same 12 texts
# Which you can see in the "get standard coordinates for rows" line of code
# Here is the code below with our data subbed in, just to archive it
# But if you run it, it produces a total mess!

df_coverage_bootstrap <- df_coverage_2 %>%
  column_to_rownames(var = "Name")
coverage <- as.matrix(df_coverage_bootstrap)
boot.ca <- ca(coverage)
nsim <- 100

#compute rowsums
boot.rowsum <- apply(coverage, 1, sum)

#compute bootstrap samples
boot.sim <- df_coverage_2 %>%
  rep_sample_n(size=353, replace=TRUE, reps = nsim) %>%
  ungroup() %>%
  select(-c("replicate", "Name"))

boot.sim2 <- as.matrix(boot.sim)

# get standard coordinates for rows
boot.rowsc <- boot.ca$rowcoord[,1:2]

# calculate principal coordinates of all replicates using transition formula
boot.colsim <- t(t(boot.rowsc)%*%boot.sim2[1:353,])/
  apply(boot.sim2[1:353,], 2, sum)
for (k in 2:nsim) boot.colsim <- rbind(boot.colsim,
                                       t(t(boot.rowsc)%*%boot.sim2[((k-1)*353+1):(k*353),])/
                                         apply(boot.sim2[((k-1)*353+1):(k*353),], 2, sum))

# reorganize rows of coordinates so that letters are together
boot.colsim2 <- matrix(rep(0,29*nsim*2), nrow = 29*nsim)
for (j in 1:29) { 
  for (k in 1:nsim) {
    boot.colsim2[(j-1)*nsim+k,] <- boot.colsim[j+(k-1)*29,]
  }
}

# plot all points (use first format of coords for labelling...)
plot(boot.colsim[,1], -boot.colsim[,2], xlab = "dim1", ylab = "dim2", 
     type = "n")
node.numb <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15",
               "16","17","18","19","20","21","22","23","24","25","26","27","28","29")
text(boot.colsim[,1], -boot.colsim[,2], node.numb, cex = 0.8, col = "gray")

# plot convex hulls for each letter
# first calculate principal coordinates of letters for original matrix
boot.col <- t(t(boot.rowsc)%*%coverage)/apply(coverage, 2, sum)
for (j in 1:29) {
  points <- boot.colsim2[(nsim*(j-1)+1):(nsim*j),]
  # note we are reversing second coordinate in all these plots
  points[,2] <- -points[,2]
  hpts       <- chull(points)
  hpts       <- c(hpts,hpts[1])
  lines(points[hpts,], lty = 3, lwd = 2)
  text(boot.col[j,1], -boot.col[j,2], node.numb[j], font = 2, cex = 1.5)
}

# peeling a new convex hull (here we peel until just under 5% are removed)
plot(boot.colsim2[,1], -boot.colsim2[,2], xlab = "dim1", ylab = "dim2", 
     type = "n")
for (j in 1:29) {
  points <- boot.colsim2[(nsim*(j-1)+1):(nsim*j),]
  # note we are reversing second coordinate in all these plots
  points[,2] <- -points[,2]
  repeat {
    hpts <- chull(points)
    npts <- nrow(points[-hpts,])   #next number of points in peeled set
    if(npts/nsim < 0.95) break
    points <- points[-hpts,]
  }
  hpts <- c(hpts,hpts[1])
  lines(points[hpts,], lty = 3)
  text(boot.col[j,1], -boot.col[j,2], node.numb[j], font = 2)
}
