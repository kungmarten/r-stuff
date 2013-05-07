# Number of runs (one for each cell)
cc = length(trmat)

# let's measure the inactivity rank
rankpoint = 11

# Create empty matrix
inacmat <- zeros(size(trmat, 1), size(trmat, 2))

while (cc > 0) {
   nmat = trmat
   nmat[cc] = (trmat[cc] * 1.1)
   nmat = nmat/rowSums(nmat)
   nmat = t(nmat)
   prv = pagerank (nmat, 0.95, 0.0001);
   inacmat[cc] = prv[rankpoint];
   cc = cc - 1;
}