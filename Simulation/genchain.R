genchain <- function(udf, i, rows, mdf) {
  if (i >= rows) {
    return(mdf)
	}
  else {
    if(i == 1) {
      mdf <- rbind(data.frame(id = udf[i, 1], row_number = 0, days_until = 0, vDesc = "Start"), udf[i, ])
  	}
    else if((udf[i, 1] != udf[i-1, 1]) && (udf[i, 3] < 6)) { 
      k <- rbind(data.frame(id = udf[i, 1], row_number = 0, days_until = 0, vDesc = "Start"), udf[i, ])
      mdf <- rbind(mdf, k)
	  }
    else if((udf[i, 1] != udf[i-1, 1]) && (udf[i, 3] >= 6)) {
      if(udf[i, 1] == udf[i+1, 1]) {
	    meanrow_number <- ((udf[i, 2] + udf[i+1, 2])/2 )
		}
	  else {
	    meanrow_number <- (udf[i, 2]+1)
	    }
      k <- rbind(data.frame(id = udf[i, 1], row_number = 0, days_until = 0, vDesc = "Start"), udf[i, ])
	  k <- rbind(k, data.frame(id = udf[i, 1], row_number = meanrow_number, days_until = 0, vDesc = "Inactive"))
      mdf <- rbind(mdf, k)
	  }
    else if(udf[i, 3] >= 6) {
      if(udf[i, 1] == udf[i+1, 1]) {
	    meanrow_number <- ((udf[i, 2] + udf[i+1, 2])/2 )
		}
	  else {
	    meanrow_number <- (udf[i, 2]+1)
	    }
      k <- rbind(udf[i, ], data.frame(id = udf[i, 1], row_number = meanrow_number, days_until = 0, vDesc = "Inactive"))
      mdf <- rbind(mdf, k)
	  }	  
    else {
      mdf <- rbind(mdf, udf[i, ])
	  }
    genchain(udf, i+1, rows, mdf)  
  }
}