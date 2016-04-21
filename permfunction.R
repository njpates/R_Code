permfunc = function(x){
  perm = c()
  permtemp = perm
  for(i in 1:x){
    permtemp = paste(i,seq(i,x,by=1),sep='')
    perm = c(perm,permtemp)
  }
  
  matrix(perm,ncol=1)
}