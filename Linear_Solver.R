linear_solver = function(m){
  sm=m
  for(i in 1:nrow(m)){
    sm[i,] = sm[i,]/sm[i,i]
    for(j in (1:nrow(m))[-i]){
      sm[j,] = sm[j,]-sm[i,]*sm[j,i]
    }
  }
  sm
}