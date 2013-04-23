require(RODBC)

mcmc <- function(size, mat, stpos) {
#size = number of runs, mat = trasition matrix 
  mcmci <- function(size, stpos, mat, rar) {
    loop <- function(r, stpos, k) {
      if (r <= mat[stpos, k]) {
	    return(k)
	    }
	  else loop(r, stpos, k + 1)
	  }
    if (size <= 0) return(rar)
    else {
    rnd <- runif(1)
    f <- loop(rnd, stpos, 1)
    trar <- matrix(replicate(nrow(mat)^2, 0), nrow(mat))  
    trar[stpos, f] <- 1
    rar <- rar + trar
    mcmci(size-1, f, mat, rar)
    }
  }
  rar <- matrix(replicate(nrow(mat)^2, 0), nrow(mat))
  a <- mcmci(size, stpos, mat, rar)
  return(a)
}


genchain <- function(udf) {
  mdf <- data.frame(t(rep(NA, ncol(udf))))
  names(mdf) <- names(udf)
  mdf <- mdf[-1, ]
  i <- 1
  rows <- nrow(udf)
  
  while( i <= rows) {
  
    # The first entry of the user and the next user interaction is not inactivity
	if(((udf[i, 1] != udf[i-1, 1]) || i == 1) && (udf[i, 3] < 6)) { 
      k <- rbind(data.frame(iUserId = udf[i, 1], RNo = 0, DDiff = 0, vDesc = "Start"), udf[i, ])
      mdf <- rbind(mdf, k)
	  }
	  
	# If the user goes from first entry directly to inactivity
    else if(((udf[i, 1] != udf[i-1, 1]) || i == 1) && (udf[i, 3] >= 6)) {
      if(udf[i, 1] == udf[i+1, 1]) {
	    meanRno <- ((udf[i, 2] + udf[i+1, 2])/2 )
		k <- rbind(data.frame(iUserId = udf[i, 1], RNo = 0, DDiff = 0, vDesc = "Start"), udf[i, ])
	    k <- rbind(k, data.frame(iUserId = udf[i, 1], RNo = meanRno, DDiff = 0, vDesc = "Inactive"))
		}
	  else {
	    meanRno <- (udf[i, 2]+1)
		k <- rbind(data.frame(iUserId = udf[i, 1], RNo = 0, DDiff = 0, vDesc = "Start"), udf[i, ])
	    k <- rbind(k, data.frame(iUserId = udf[i, 1], RNo = meanRno, DDiff = 0, vDesc = "Inactive"))
		k <- rbind(k, data.frame(iUserId = udf[i, 1], RNo = meanRno, DDiff = 0, vDesc = "Inactive"))
	    }
      mdf <- rbind(mdf, k)
	  }
	  
	# If the user goes to inactivity
    else if(udf[i, 3] >= 6) {
      if(udf[i, 1] == udf[i+1, 1]) {
	    meanRno <- ((udf[i, 2] + udf[i+1, 2])/2 )
		k <- rbind(udf[i, ], data.frame(iUserId = udf[i, 1], RNo = meanRno, DDiff = 0, vDesc = "Inactive"))
		}
	  else {
	    meanRno <- (udf[i, 2]+1)
		k <- rbind(udf[i, ], data.frame(iUserId = udf[i, 1], RNo = meanRno, DDiff = 0, vDesc = "Inactive"))
		k <- rbind(k, data.frame(iUserId = udf[i, 1], RNo = meanRno, DDiff = 0, vDesc = "Inactive"))
	    }
      mdf <- rbind(mdf, k)
	  }
	
	# If the user neither starts nor goes to inactivity
    else {
      mdf <- rbind(mdf, udf[i, ])
	  }
	i <- i + 1
  }
  return(mdf)
}


u <- #sequence data

fullchain <- genchain(u)

bump <- moveup(fullchain)
bump <- bump[complete.cases(bump), ]
trmat <- table(bump[, 4], bump[, 5])
trmat <- cbind(Start = 0, trmat)
trmat <- trmat[order(rownames(trmat)), order(colnames(trmat))]
coltrdf <- data.frame(rID = as.vector(row(trmat)), rname = colnames(trmat))
rowtrdf <- data.frame(rID = as.vector(row(trmat)), rname = colnames(trmat))
entry <- coltrdf[coltrdf$rname == "Start", 1][1]
comrow <- rowtrdf[trdf$rname == "Inactive", 1][1]
comcol <- coltrdf[trdf$rname == "Inactive", 1][1]
comval <- 1e99

tempmat <- trmat
tempmat[1, i] <- tempmat[1, i] + sum(tempmat[1,])/10
tempmat <- tempmat/rowSums(tempmat)
l <- t(apply(tempmat, 1, cumsum))
retm <- mcmc(20, l, entry)
reps <- 10000-1
while(reps > 0) {
  retm <- retm + mcmc(20, l, entry)
  reps <- reps - 1
  }
ncom <- retm[comcol, comrow]
bestrow <- c(ncom, "No alteration")

for (i in 1:(nrow(trmat))) {
  tempmat <- trmat
  tempmat[11, i] <- tempmat[11, i] + sum(tempmat[11,])/10
  print(tempmat[11, i])
  tempmat <- tempmat/rowSums(tempmat)
  l <- t(apply(tempmat, 1, cumsum))
  retm <- mcmc(20, l, entry)
  reps <- 10000-1
  while(reps > 0) {
    retm <- retm + mcmc(20, l, entry)
    reps <- reps - 1
    }
  
  ncom <- retm[comcol, comrow]
  # if (ncom < comval) {
    # comval <- ncom
	# bestrow <- as.vector(coltrdf[i, 2])
	# }
  bestrow<- rbind(bestrow, c(ncom, as.vector(coltrdf[i, 2])))
}
  