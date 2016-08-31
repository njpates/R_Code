CCR2 = function(cons_Input,tech_Input,cons_Output,tech_Output,i){
library(lpSolve)
library(magrittr)
library(dplyr)

N = tech_Input %>% nrow()
M = tech_Input %>% ncol()
S = tech_Output %>% ncol()

obj = c(1,0 %>% rep(times=N))
i_const = c(-cons_Input[i,]) %>% cbind(tech_Input %>% t)
o_const = c(0 %>% rep(times=S)) %>% cbind(tech_Output %>% t)
nn_const = c(0 %>% rep(times=N)) %>% cbind(diag(N))
const = rbind(i_const,o_const,nn_const)
rel = c(rep("<=",times = M),rep(">=",times = S),rep(">=",times = N))
rhs = c(rep(0,times = M),cons_Output[i,] %>% c,rep(0,times = N))
obj_val = lp(direction = "min",obj,const,rel,rhs)$objval
obj_val	
}