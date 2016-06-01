library(data.table)
library(magrittr)
library(dplyr)
library(lpSolve)
setwd('~/Dropbox/DEA Folder')
m=read.csv('checkdata.csv')
Inputs = as.matrix(m[,c(2,3)])
Outputs = as.matrix(m[,c(4,5,6)])
Labels = as.matrix(m[,c(1)])

i=1

BCC_Dual = function(Inputs,Outputs,i){
  cons = cbind(rbind(
    cbind(-(Inputs[i,] %>% c), Inputs %>% t)
    ,cbind(rep(0,times=Outputs %>% ncol),(-Outputs) %>% t))
    , diag((Outputs %>% ncol)+(Inputs %>% ncol)))
  RHS = c(0 %>% rep(times=(Inputs %>% ncol)), -Outputs[i,] %>% c)
  RHS = RHS %>% c(1)
  rel = rep('<=',times= RHS %>% length) %>% rev; rel[1] = c('=='); rel = rel %>% rev
  obj = c(-1, (0 %>% rep(times = Inputs %>% nrow)), .000 %>% rep(times= (Inputs %>% ncol) + (Outputs %>% ncol)))
  cons = rbind(cons,c(0 , rep(1,times= Inputs %>% nrow), rep(0,times=(Inputs %>% ncol)+(Outputs %>% ncol))) %>% t)
  model =lp('max',obj,cons,rel,RHS,compute.sens=TRUE) 
  duals = (model$solution[-1]) %>% rev()
  duals = duals[-c(1:((Outputs%>% ncol)+(Inputs %>% ncol)))] %>% rev
  objval = -model$objval
  list(duals = duals,objval=objval,model=model)
}

fillmat=array(0,dim=c(nrow(Inputs),nrow(Inputs)))
eff = c(1:fillmat %>% nrow)
for(i in 1:nrow(Inputs)){
  fillmat[,i]=BCC_Dual(Inputs,Outputs,i)$duals
  eff[i] = BCC_Dual(Inputs,Outputs,i)$objval
}
#round(fillmat,1)
d = cbind(eff,fillmat %>% round(digits=2))
rownames(d)=c(1:nrow(d))
colnames(d) = c('Efficiency',c(1:nrow(d))%>% as.character)