# Create example data
# j <- matrix(runif(16), 4)
# k <- j/rowSums(j)
# l <- t(apply(k, 1, cumsum))

# This function will take a transition matrix and simulate the "walk" through it.
# The return matrix is a count on how many steps were taken in each position. 
mcmc <- function(size, mat) {
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
  a <- mcmci(size, 1, mat, rar)
  return(a)
}

  
