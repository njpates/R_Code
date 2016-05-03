iter = function(max) {
  x = c()
  for (i in 1:max) {
    for (j in 1:max) {
      if (i <= j) {
        x = c(x,paste(i,'_',j,sep = ''))
      }
      #      if(i>j){
      #        x = c(x,paste(j,'_',i,sep=''))
      #      }
    }
  }
  matrix(x,ncol = 1)
}

iter(9)
rm(x)
