library(data.table)
library(magrittr)
library(dplyr)
library(lpSolve)
setwd('~/Dropbox/DEA Folder')
m=read.csv('checkdata.csv')
Inputs = as.matrix(m[,c(2,3)])
Outputs = as.matrix(m[,c(4,5,6)])
Labels = as.matrix(m[,c(1)])

i = 1

CCR_Dual = function(Inputs,Outputs,i){
  
  cons = cbind(rbind(
    
    cbind(-(Inputs[i,] %>% c), Inputs %>% t)
    
    ,cbind(rep(0,times=Outputs %>% ncol),(-Outputs) %>% t)
  )
  
  , diag((Outputs %>% ncol)+(Inputs %>% ncol))
  )
  
  RHS = c(
    0 %>% rep(times=(Inputs %>% ncol))
    , -Outputs[i,] %>% c
  )
  
  rel = rep('<=',times= RHS %>% length)
  obj = c(-1, (0 %>% rep(times = Inputs %>% nrow)), .000 %>% rep(times= (Inputs %>% ncol) + (Outputs %>% ncol)))
  
  model =lp('max',obj,cons,rel,RHS,compute.sens=TRUE) 
  
  duals = (model$solution[-1]) %>% rev()
  duals = duals[-c(1:((Outputs%>% ncol)+(Inputs %>% ncol)))] %>% rev
  objval = -model$objval
  
  list(duals = duals,objval=objval,model=model)
}

CCR_Dual(Inputs,Outputs,i)$duals

