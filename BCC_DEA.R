
library(lpSolve )
data <- read.csv("DEAsample3.csv")

labels <- data[,1]
In <- data[,2:3]
Out <- data[,4:5]

BCC_DEA <- function(In,Out,labels){
  theta <- rep(0,times=nrow(data))
  for(i in 1:nrow(data)){
    
    
    inputs <- as.matrix(In)
    
    inputs1 <- t(inputs)
    
    Inputs <- cbind(inputs1[,i],-inputs1)
    Inputs
    
    outputs <- as.matrix(Out)
    outputs1 <- t(outputs)
    Outputs <- cbind(rep(0,times=nrow(outputs1)),outputs1)
    
    Const <- rbind(c(0,rep(1,times=nrow(inputs))),Outputs,Inputs,cbind(rep(0,times=nrow(inputs1)),diag(ncol(inputs1)))
    )
    
    rel <- c(1,outputs[i,],rep(0,times=nrow(Const)-1-ncol(outputs)))
    dir=rep(">=",times=nrow(Const))
    
    
    
    obj <- c(1,rep(0,times=ncol(Const)-1))
    
    theta[i] <- lp("min",obj,Const,dir,rel)$solution[1]
    
  }
  names(theta) <- labels
  theta
}

BCC_DEA(In,Out,labels)
cbind(optx_ex,BCC_DEA(In,Out,labels))
Const


