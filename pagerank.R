# Parameter M adjacency matrix where M_i,j represents the link from 'j' to 'i', such that for all 'j' sum(i, M_i,j) = 1
# Parameter d damping factor
# Parameter v_quadratic_error quadratic error for v
# Return v, a vector of ranks such that v_i is the i-th rank from [0, 1]

pagerank <- function(M, d, v_quadratic_error) {
	N <- ncol(M)
	v <- as.matrix(runif(N))
	v <- v / base:::norm(v, type = "O")
	last_v <- as.matrix(rep(1, N)) * 1000
	M_hat <- (d * M) + (((1 - d) / N) * matrix(replicate(N^2, 1), N))
	
	while (base:::norm(v - last_v, type = "2") > v_quadratic_error) {
		last_v = v
		v = M_hat %*% v
	}
	return(v)
}
