library(lpSolve)
library(dplyr)
library(magrittr)
library(data.table)
CCR = function(Inputs,Outputs,i){
  cons1 = c(rep(0,times=ncol(Outputs)),Inputs[i,])
  cons2 = cbind(Outputs,(-Inputs))
  cons3 = diag((ncol(Inputs)+ncol(Outputs)))
  cons = rbind(cons1,cons2,cons3)
  RHS = rep(0,times=nrow(cons)); RHS[1] = 1
  obj = rep(0,times=(ncol(Inputs)+ncol(Outputs)))
  obj[1:ncol(Outputs)] = Outputs[i,]
  dir = rep('<=',times=nrow(cons)); dir[1] = '=='; dir[(2+nrow(cons2)):length(dir)] ='>'
  lp('max',obj,cons,dir,RHS)$objval
}
Inputs = as.matrix(m[,c(2,3)])
Outputs = as.matrix(m[,c(4,5,6)])
Labels = as.matrix(m[,c(1)])
Obj_val = rep(0,times=length(Labels))

for(i in 1:length(Obj_val)){ 
  Obj_val[i] = CCR(Inputs,Outputs,i)
}
m1 = (cbind(m[1],Obj_val) %>% data.table)[order(Obj_val,decreasing=TRUE)]