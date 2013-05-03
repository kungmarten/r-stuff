# Prep data for the transition matrix, i.e. add a start and finish for each session.

genchain <- function(udf) {
  mdf <- data.frame(t(rep(NA, ncol(udf))))
  names(mdf) <- names(udf)
  mdf <- mdf[-1, ]
  i <- 2
  rows <- nrow(udf)
  k <- rbind(data.frame(iUserId = udf[i, 1], RNo = 0, vDesc = "Start"), udf[i, ])
  mdf <- k
    
  while( i < rows) {
    #if the next user differs from the current, i.e. inactivity.
    if (udf[i, 1] != udf[(i+1), 1]) {
	  k <- rbind(udf[i, ], data.frame(iUserId = udf[i, 1], RNo = udf[i, 2] + 1, vDesc = "Inactive"),  data.frame(iUserId = udf[i, 1], RNo = udf[i, 2] + 2, vDesc = "Inactive"))
	  k <- rbind(k, data.frame(iUserId = udf[(i+1), 1], RNo = 0, vDesc = "Start"))
	  mdf <- rbind(mdf, k)
	}
	# If the user neither starts nor goes to inactivity
    else {
      mdf <- rbind(mdf, udf[i, ])
	  }
	i <- i + 1
  }
  k <- rbind(udf[i, ], data.frame(iUserId = udf[i, 1], RNo = udf[i, 2] + 1, vDesc = "Inactive"),  data.frame(iUserId = udf[i, 1], RNo = udf[i, 2] + 2, vDesc = "Inactive"))
  mdf <- rbind(mdf, k)
  return(mdf)
}