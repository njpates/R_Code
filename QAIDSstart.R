rest= function(totalvar){matrix = array()
mastermatrix = array()
for (i in 1:totalvar) {
  for (j in 1:totalvar) {
    if (i != j) {
      matrix = array(0,dim = c(totalvar,totalvar))
      matrix[i,j] = 1
      matrix[j,i] = 1
      matrix = t(c(matrix))
      mastermatrix = rbind(mastermatrix,matrix)
    }
  }
}
mastermatrix = mastermatrix[-1,]
mastermatrix
}

#################################################

QAIDSeq = function(eqno,totalvar){
  a = paste('a',eqno,'+',sep='')
  index = 1:totalvar
  perm1 = paste(index,'_',sep='')
  perm = as.character()
  for(i in 1:totalvar){
    perm = c(perm,paste(perm1,i,sep=''))
  }
  a1 = paste('g',eqno,'_',index,'*',paste('lnp',index,sep=''),'+',sep='')
  ######
  for(i in 1:totalvar){
    a = paste(a,a1[i],sep='')
  }
  ######  
  b = paste('b',eqno,'*(lnx-',sep='')
  alp = paste('a',index,sep ='')
  alp1 = paste('*lnp',index,sep='')
  alp = paste(alp,alp1,'+',sep='')
  alp1 = as.character()
  for(i in 1:totalvar){
    alp1 = paste(alp1,alp[i],sep='')
  }
  
  lna0 = paste('a0+',alp1,sep='')
  
  
  #la02 = paste('(1/2)*(')
  pperm = paste('*lnp',index,sep='')
  pperm2= as.character()
  for( i in 1:totalvar){
    pperm2 = c(pperm2,paste(pperm,pperm[i],sep=''))
  }
  pperm2 = paste(perm,pperm2,sep='')
  pperm2 = paste('g',pperm2,sep='')
  pperm3 = as.character()
  for(i in 1:length(pperm2)){
    pperm3=paste(pperm3,pperm2[i],'+',sep='')
  }
  pperm3 = substr(pperm3,1,(nchar(pperm3)-1))
  lna0 = paste('(',lna0,'(1/2)*(',pperm3,')',')',sep='')
  
  ###
  lnap = paste('(lnX-',lna0,')',sep='')
  ###
  
  bp = paste('p',index,'^',sep='')
  bp1 = paste('(t',index,')',sep='')
  bp = paste(bp,bp1,sep='')
  bp2 = as.character()
  for(i in 1:length(bp)){
    bp2 = paste(bp2,bp[i],'*',sep='')
  }
  
  bp = substr(bp2,1,(nchar(bp2)-1))
  ###
  bp = paste('(',bp,')',sep='')
  ###
  paste('eq',eqno,'=',' w',eqno,'~',a,'b',eqno,'*',lnap,'+t',eqno,'*(1/',bp,')*2*',lnap,sep='')
}

QAIDSeq(1,10)
QAIDSeq(2,10)
QAIDSeq(3,10)
QAIDSeq(4,10)
QAIDSeq(5,10)
QAIDSeq(6,10)
QAIDSeq(7,10)
QAIDSeq(8,10)
QAIDSeq(9,10)
QAIDSeq(10,10)

