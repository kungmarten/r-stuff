# Create example data
# j <- matrix(runif(16), 4)
# k <- j/rowSums(j)
# l <- t(apply(k, 1, cumsum))

# This function will take a transition matrix and simulate the "walk" through it.
# The transtion matrix should not be the nominal probability but the cumulative.
# Please refer to the creation of matrix l in the example above.
# The return matrix is a count on how many steps were taken in each position. 
  
mcmc <- function(size, mat) {
#size = number of runs, mat = trasition matrix 
  loop <- function(r, stpos, k) {
    if (r <= mat[stpos, k]) {
	  return(k)
	  }
	else loop(r, stpos, k + 1)
	}
  rar <- matrix(replicate(nrow(mat)^2, 0), nrow(mat))
  i <- 1
  stpos <- 1
  
  while (i <= size) {
    rnd <- runif(1)
    f <- loop(rnd, stpos, 1)
    trar <- matrix(replicate(nrow(mat)^2, 0), nrow(mat))  
    trar[stpos, f] <- 1
    rar <- rar + trar
	stpos <- f
    i <- i + 1
    }
  return(rar)
}

  
