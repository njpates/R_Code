Targeted Announced Adoption with Lumpsum payments with Budget ConstraintsE
# Let '#@!@#' be the symbol for potential
# changes to the type of analysis
# Let '#!!!#' be the symbol for key looping
# areas.

set.seed(4364)
##### Loading Needed Packages #####

library(igraph)
library(data.table)
library(magrittr)
library(dplyr)

##### Explanatory Text #####

# We will examine the adoption trends of farmers
# that have the potential of a subsidy in the future
# this simulation starts from time zero to some time
# horizon TH. At the start of the time period, farmers
# KNOW that the policy will begin at time A. That is
# the government doesn't "suprise" farmers with a
# policy just as it is beginning. We can alter this
# assumption by taking the market expectations adoption
# with the expectation neutral adoption. We assume that
# when the government offers the subsidy, they will selec
# a portion of the farmers that haven't yet adopted at
# random. That is the government could waste its "offers"
# on farmers that would adopt if they didn't recieve
# the subsidy at a particular time, conceding that they
# didn't get the one that they wanted and ones where the
# technology isn't cheap enough even with the subsidy. See the
# section by the Contemp_Adopt

##### Introducing Parameters for the theta Distribution F(theta) #####

# Standard Tech Parameters
a = 50000; b = 450; c = 5

# Green Tech Parameters
d = 55000; e = 250; f = 2.5

# Cost Dynamic Parameters
g = 90000; h = 2200; j = 10

##### Time Horizon and Time Periods #####

TH <- 50

#time <- 1:TH

##### Number of firms in the market #####

N <- 500

##### Distributional Parameters #####
r = 0.1;beta = 5;mu = 0

##### Period Specific Discount Rates #####

discount <- function(t) {
  1 / ((1 + r) ^ t)
}

##### Firm Population Characteristics #####

theta <- rlogis(N,location = mu,scale = beta)

theta <- theta %>% sort

##### The Government's Policy Parameters #####

# Proportion of time that passes until the policy is active

pa <- .5

# Actual Period when the policy becomes active

A <- TH * pa %>% round(digits=1)

# Period when the policy is announced

announce <- A - 5

##### Cost Function #####

funccost <- function(t) {
  g - h * t + j * t ^ 2
}

##### Profit Functions #####

# Standard Tech Profits

pi1 <- function(theta) {
  a + b * theta - c * theta ^ 2
}

# Green Tech Profits

pi2 <- function(theta) {
  d + e * theta - f * theta ^ 2
}

##### Description of the graph ######
# Before we write the graph let's take a look at the structure of the graph
# where we have 5 periods. These periods correspond to the period where the
# farmer HAS A CHOICE TO MAKE.

#T 1       2       3       4       5

#       1      3       6        9      12
#  1 ----- 2 ----- 4 ----- 6 ----- 8 -------    <--------------  ORIGINAL TECHNOLOGY RUNG
#    \       \       \       \       \ 13   |
#     \       \       \       \       \     |
#      \2      \4      \7     \10     10 --|   <--------------  TERMINAL NODE
#       \       \       \       \     /
#         \       \       \       \ /  14
#          3 ----- 5 ----- 7 ----- 9            <--------------  GREEN TECHNOLOGY RUNG
#               5       8       11

### Some Facts ###
# Farmers DO NOT have green tech in period 1.
# We will always have n nodes on the bottom rung.
# We will always have n+1 nodes on the top rung (including the terminal node).
# We will always have n-1 arcs on the bottom rung.
# We will always have 2*(n) arcs on the top rung.
# The (n+1)th node will always be the first node in the original tech rung.
# The terminal node will always be the (2*n+1)th node.

# To pick out a specific time period (t)'s arc value to
# change in the adoption arcs in the example (6,8,10,12,14)
# it will be found taking (n-1) + 2*(t). For instance,
# if we wanted to change the contribution from the government
# in period 3 we would reference (5-1)+2*(3)=10.

##### Establishing functions through time #####

# Time Periods

time <- c(1:TH)

# Discount factor through time

df <- time %>% discount

# Non-Dicounted Market costs of adopting through time

cost <- time %>% funccost 

# Dicounted Market costs of adopting through time

cost1 <- cost * df

##### Adoption Plan In Absence of Government Help ######


EM_Path_matrix <- array(0,dim = c(N,TH+1))

k <- 1

while (k<N+1){
  
  theta1 <- theta[k]
  
  underprof = ((theta1 %>% pi2) *df[-1]) %>% round(digits=3)
  overprof = cbind((theta1 %>% pi1) * df[-1],(theta1 %>% pi2 - time[-1] %>% funccost)*df[-1])
  prof = overprof %>% cbind(underprof) %>% t %>% c
  prof = c(theta1 %>% pi1 *df[1],(theta1 %>% pi2 -time[1] %>% funccost)*df[1]) %>% c(prof)
  prof = -prof
  prof = prof - min(prof) + 1 %>% round(digits = 3)
  
  graph_create = function(TH){
    require(igraph)
    edge_label1 <- paste("P",rep(0,length = TH),',',seq(1:TH),')',sep='') %>%cbind(paste("P(",rep(1,length=TH),",",seq(1:TH),") - C",seq(1:TH),sep=""))
    edge_label = paste("P(",rep(1,length=TH-1),",",seq(2,TH),")",sep="") %>% cbind(edge_label1[-1,] )
    edge_label = c(edge_label1[1,]) %>% c(edge_label %>% t %>% c)
    
    labelmat = 0 %>% rep(times=(TH-1)) %>% cbind(1 %>% rep(times=(TH-1)),cbind(1 %>% rep(times=(TH-1))))
    
    lm2 = c(2:TH) %>% cbind(c(2:TH) %>% cbind(c(2:TH)))
    labelmat = paste(labelmat,lm2,sep=',')
    labelmat = paste('P(',labelmat,')',sep='')
    #    lablemat = lablemat %>% paste('C(',2:TH,')',sep='') %>% paste()
    #    %>% array(dim=c((TH-1),2))
    #  
    #    c(paste('',sep='') %>% rep(times=TH-1) %>% c(paste('-C',2:TH,'+S',2:TH,sep='')) %>% c(paste('',sep='')%>% rep(times=(TH-1))))
    
    edge_label = labelmat %>% paste(c(paste('',sep='') %>% rep(times=TH-1) %>% c(paste('-C',2:TH,'+S',2:TH,sep='')) %>% c(paste('',sep='')%>% rep(times=(TH-1)))),sep='') %>% array(dim=c(TH-1,3)) %>% t %>% c
    
    edge_label = c('P(0,1)','P(1,1)-C1') %>% c(edge_label)
    
    
    ######## Creating Adjacency List ########
    
    adj <- list()
    adj[[1]] =c(2,3)
    for(z in 1:(TH-1)){
      adj[[2*z]] = c(2*(z+1),2*z+3)
      adj[[2*z+1]] = c(2*z+3)
    }
    adj[[2*TH-1]]= c(2*TH)
    adj[[2*(TH-1)]] = c(2*TH,2*TH)
    
    ########################################
    
    xlayout = c(1) %>% c(cbind(c(2:TH),c(2:TH))%>% t) %>% c(TH+1)
    ylayout = c(2) %>% c(rep(c(2,0),times=(TH-1))) %>% c(1)
    layout = array(c(xlayout,ylayout),dim=c(2*TH,2))
    adjg <- graph.adjlist(adj)
    list(graph = adjg,layout=layout,label = edge_label)
  }
  #plot((TH %>% graph_create)$graph,layout=(TH %>% graph_create)$layout,edge.label=(TH %>% graph_create)$label)
  #plot((TH %>% graph_create)$graph,layout=(TH %>% graph_create)$layout,edge.label=)
  
  
  #plot((TH %>% graph_create)$graph,layout=layout,edge.label=(TH %>% graph_create)$label)
  
  #plot((TH %>% graph_create)$graph,layout=layout,edge.label=prof)
  
  EM_Path_matrix[k,] <- get.shortest.paths((TH %>% graph_create)$graph,from=1,to=2*TH,weights=prof)$vpath[[1]]
  
  k <- k+1  
  print(k)
}
EM_Path_matrix %>% data.table

Adopt_mat = array(0,dim=c(N,TH))
for(i in 1:(ncol(EM_Path_matrix)-1)){
  Adopt_mat[,i] = ((EM_Path_matrix[,i+1] - EM_Path_matrix[,i] - 2)%>% abs)
}
Adopt_mat[,1] = ((EM_Path_matrix[,1+1] - EM_Path_matrix[,1] - 1)%>% abs)
Adopt_mat[,TH] = ((EM_Path_matrix[,TH+1] - EM_Path_matrix[,TH] - 1)%>% abs)

#write.csv(Adopt_mat,'check.csv')
Adopt_prop=((array(1,dim=c(TH,N))%*%Adopt_mat)[1,] %>% cumsum / N )
Adopt_prop %>% plot(xlab ='Time (in Periods)',
                    ylab ='Cumulative Adoption',type='l')

######## Choice Data Table ##########
# This gives the optimal choices for everyone that adopted without the government's help.

Choice= (Adopt_mat %*% c(1:TH)) %>% data.table
colnames(Choice) = 'Free_Market'
Choice[,Index:=c(1:nrow(Choice))]
Choice[,Theta:=theta]
Past_Periods = Choice[,Free_Market>announce]['Free_Market']

# Currently active farmers in the system
Active_Farmers=Choice[,Index][Choice[,Free_Market]>=announce]
Inactive_Farmers=Choice[,Index][Choice[,Free_Market]<announce]

# Characteristics of active farmers
Active_Theta=Choice[,Theta][Choice[,Free_Market]>=announce]

# Verified Test of Adding columns to Choice Table
#Choice[,paste('Test',1):=Index]

# Government Parameters

Budget <- 1000000

subsidy <- 4000

factor_num <- Budget/subsidy

N0 <- Active_Farmers %>% length

prob0 <- factor_num/N0


##### Adoption Plan with Neutral Expectations [announce:A] ######
# Idea:
# We will remove all famers that have adopted before the announcment period 'announce'. These are farmers that have already made their decisions before they realized the policy was in place. Next we calculate the probability that the farmer believes will be the chances of getting the subsidy. Prob before the policy is active is of course zero. After the subsidy is active probability is based upon the current farmers in the pool at the time the policy is announced and the number of open slots the government can fill.
prob <- c()

prob[1:A-1] <- 0

i <- 1
while(i+A-1<TH+1){
  prob[i+A-1] <- factor_num/(N0-factor_num*(i-1))
  
  if(prob[i+A-1]>1 | prob[i+A-1]<0){
    prob[i+A-1] <- 1
  }
  
  i <- i+1
}

k <- 1

EN_Path_matrix <- array(0,dim=c(N0,TH+1))

while(k < N0+1){
  
  theta1 <- Active_Theta[k]
  
  underprof = ((theta1 %>% pi2) *df[-1]) %>% round(digits=3)
  overprof = cbind((theta1 %>% pi1) * df[-1],(theta1 %>% pi2 - time[-1] %>% funccost)*df[-1])
  overprof[c((A-1):(TH-1)),2]= overprof[c((A-1):(TH-1)),2]+subsidy*prob[A:TH]*df[A:TH]
  
  prof =  cbind(overprof,underprof) %>% t %>% c
  
  prof = c(theta1 %>% pi1 *df[1],(theta1 %>% pi2 -time[1] %>% funccost)*df[1],prof)
  prof = -prof
  prof = prof - min(prof) + 1 %>% round(digits = 3)
  
  adjg = graph_create(TH)$graph
  
  #plot(adjg,layout=layout,edge.label=graph_create(TH)$label)
  #plot(adjg,layout=layout,edge.label=prof)
  EN_Path_matrix[k,]<- get.shortest.paths(adjg,from=1,to=(2*TH),weights=prof)$vpath[[1]]
  
  
  #EN_Path_matrix[k,] <- get.shortest.paths(adjg,from=1,to=(2*(announce-1)),weights=prof)$vpath[[1]][-announce]  %>% c(get.shortest.paths(adjg,from=2*(announce-1),to=2*TH,weights=prof)$vpath[[1]])
  k <- k+1
  print(theta1)
}


####
EN_Adopt_mat = array(0,dim=c(N0,TH))
for(i in 1:(ncol(EN_Path_matrix)-1)){
  EN_Adopt_mat[,i] = ((EN_Path_matrix[,i+1] - EN_Path_matrix[,i] - 2)%>% abs)
}
EN_Adopt_mat[,1] = ((EN_Path_matrix[,1+1] - EN_Path_matrix[,1] - 1)%>% abs)
EN_Adopt_mat[,TH] = ((EN_Path_matrix[,TH+1] - EN_Path_matrix[,TH] - 1)%>% abs)


Inter_Choice = rep(0,times=nrow(Choice))
Inter_Choice[Active_Farmers] = EN_Adopt_mat %*% c(1:TH) %>% c
Inter_Choice[Inactive_Farmers] = Choice[Inactive_Farmers,Free_Market]

Choice[,paste("Period",A-1):=Inter_Choice,with=FALSE]
#################################################################

######## Unique Vector and Counts in the Adoption Vector ########
UV_per = Choice[,paste('Period',A-1),with=FALSE] %>% unique 
UV = 0 %>% rep(times=UV_per %>% nrow)
sampvec = Choice[,paste0('Period ',A-1),with=FALSE] %>% c
UV_uper = rep(0,times=UV_per %>% nrow)
for(i in 1:(UV %>% length)){
  UV_uper[i] = (sampvec[[1]] == UV_per[[1]][i]) %>% sum
}
UV_Adopt_prop = rep(0,times=TH)
UV_Adopt_prop[UV_per[[1]]] = UV_uper
UV_Adopt_prop = (UV_Adopt_prop %>% cumsum)/N
UV_Adopt_prop %>% lines(lty=3)
EN_Adopt_prop = UV_Adopt_prop

# Currently active farmers in the system

Active_Farmers=Choice[,Index][Choice[,paste0('Period',A-1)>=A-1]]
Inactive_Farmers=Choice[,Index][Choice[,paste0('Period',A-1)<A-1]]
Active_Theta=Choice[,Theta][Choice[,paste0('Period',A-1)>=A-1]]

#################################################################


######################## Final Iteration ########################

Choice[,paste0('Period ',A):=Choice[,paste0('Period ',A-1),with=FALSE],with=FALSE]

######## Updating New and Old Farmers w/ Characteristics ########
Active_Farmers=Choice[,Index][Choice[,Intermediate]>A]
Inactive_Farmers=Choice[,Index][Choice[,Intermediate<=A]]
Active_Theta=Choice[,Theta][Choice[,Intermediate>A]]
N0 = Active_Farmers %>% length

Choice[,paste0('Period ',A):=Choice[,paste0('Period ',A-1),with=FALSE],with=FALSE]

q <- A



while(q < TH+1){
  
  # Who is Still in the Active Farmers?
  Active_Farmers=Choice[,Index][Choice[,paste('Period',q-1),with=FALSE]>q-1]
  Inactive_Farmers=Choice[,Index][Choice[,paste('Period',q-1),with=FALSE]<=q-1]
  Active_Theta=Choice[,Theta][Choice[,paste('Period',q-1),with=FALSE]>q-1]
  N0  = Active_Theta %>% length
  
  # What does our New Group look like?
  Choice[,paste('Period',q):=Choice[,paste('Period',q-1),with=FALSE],with=FALSE]
  
  ######################## Positive Afirmation ###################
  EP_Path_matrix = array(0,dim=c(N0,TH+1))
  EP_Adopt_mat = array(0,dim=c(N0,TH))
  k=1
  while(k<N0+1)  {
    
    theta1 = Active_Theta[k]
    adjg = graph_create(TH)$graph
    
    underprof = ((theta1 %>% pi2) *df[-1]) %>% round(digits=3)
    overprof = cbind((theta1 %>% pi1) * df[-1],(theta1 %>% pi2 - time[-1] %>% funccost)*df[-1]) %>% round(digits=3)
    prob1 = prob
    prob1[q] = 1
    overprof[c((A-1):(TH-1)),2]= overprof[c((A-1):(TH-1)),2]+subsidy*prob1[A:TH]*df[A:TH]
    prof =  cbind(overprof,underprof) %>% t %>% c
    prof = c(theta1 %>% pi1 *df[1],(theta1 %>% pi2 -time[1] %>% funccost)*df[1],prof)
    prof = -prof
    prof = prof - min(prof) + 1 %>% round(digits = 3)
    
    EP_Path_matrix[k,]<- get.shortest.paths(adjg,from=1,to=(2*TH),weights=prof)$vpath[[1]]
    
    #  EP_Path_matrix[k,] <- get.shortest.paths(adjg,from=1,to=(2*(q-1)),weights=prof)$vpath[[1]][-q]  %>% c(get.shortest.paths(adjg,from=2*(q-1),to=2*TH,weights=prof)$vpath[[1]]) 
    
    k=k+1
    print(theta1)
  }
  
  
  for(i in 1:(ncol(EP_Path_matrix)-1)){
    EP_Adopt_mat[,i] = ((EP_Path_matrix[,i+1] - EP_Path_matrix[,i] - 2)%>% abs)
  }
  EP_Adopt_mat[,1] = ((EP_Path_matrix[,1+1] - EP_Path_matrix[,1] - 1)%>% abs)
  EP_Adopt_mat[,TH] = ((EP_Path_matrix[,TH+1] - EP_Path_matrix[,TH] - 1)%>% abs)
  EP_Choice = rep(0,times=Choice %>% nrow)
  EP_Choice[Inactive_Farmers] = (Choice[Inactive_Farmers,paste('Period',q-1),with=FALSE])[[1]]
  EP_Choice[Active_Farmers] = EP_Adopt_mat %*% c(1:TH) %>% c
  Choice[,paste('EP_Choice',q):=EP_Choice,with=FALSE]
  
  Applicants =Choice[,Index][Choice[,paste('EP_Choice',q),with=FALSE]==q]
  if((Applicants %>% length)>Budget/subsidy %>% round(digits=0) ){
    Accepted = sample(Applicants,round(Budget/subsidy,digits=0),0)}else{
      Accepted = Applicants
    }
  
  Choice[Accepted,paste('Period',q):=q]
  
  Active_Farmers=Choice[,Index][Choice[,paste('Period',q),with=FALSE]>q]
  Inactive_Farmers=Choice[,Index][Choice[,paste('Period',q),with=FALSE]<=q]
  Active_Theta=Choice[,Theta][Choice[,paste('Period',q),with=FALSE]>q]
  N0  = Active_Theta %>% length
  
  ######################## Negative Afirmation ###################
  EG_Path_matrix = array(0,dim=c(N0,TH+1))
  EG_Adopt_mat = array(0,dim=c(N0,TH))
  k=1
  while(k<N0+1)  {
    
    theta1 = Active_Theta[k]
    adjg = graph_create(TH)$graph
    
    underprof = ((theta1 %>% pi2) *df[-1]) %>% round(digits=3)
    overprof = cbind((theta1 %>% pi1) * df[-1],(theta1 %>% pi2 - time[-1] %>% funccost)*df[-1]) %>% round(digits=3)
    prob1 = prob
    prob1[q] = 0
    overprof[c((A-1):(TH-1)),2]= overprof[c((A-1):(TH-1)),2]+subsidy*prob1[A:TH]*df[A:TH]
    prof =  cbind(overprof,underprof) %>% t %>% c
    prof = c(theta1 %>% pi1 *df[1],(theta1 %>% pi2 -time[1] %>% funccost)*df[1],prof)
    prof = -prof
    prof = prof - min(prof) + 1 %>% round(digits = 3)
    
    EG_Path_matrix[k,]<- get.shortest.paths(adjg,from=1,to=(2*TH),weights=prof)$vpath[[1]]
    
    #  EG_Path_matrix[k,] <- get.shortest.paths(adjg,from=1,to=(2*(q-1)),weights=prof)$vpath[[1]][-q]  %>% c(get.shortest.paths(adjg,from=2*(q-1),to=2*TH,weights=prof)$vpath[[1]]) 
    
    k=k+1
    print(theta1)
  }
  
  
  for(i in 1:(ncol(EG_Path_matrix)-1)){
    EG_Adopt_mat[,i] = ((EG_Path_matrix[,i+1] - EG_Path_matrix[,i] - 2)%>% abs)
  }
  EG_Adopt_mat[,1] = ((EG_Path_matrix[,1+1] - EG_Path_matrix[,1] - 1)%>% abs)
  EG_Adopt_mat[,TH] = ((EG_Path_matrix[,TH+1] - EG_Path_matrix[,TH] - 1)%>% abs)
  EG_Choice = rep(0,times=Choice %>% nrow)
  EG_Choice[Inactive_Farmers] = (Choice[Inactive_Farmers,paste('Period',q-1),with=FALSE])[[1]]
  EG_Choice[Active_Farmers] = EG_Adopt_mat %*% c(1:TH) %>% c
  Choice[,paste('EG_Choice',q):=EG_Choice,with=FALSE]
  
  print(q)
  q=q+1
}
periodvec =paste("Period",c((TH):TH))
Choice[,periodvec,with=FALSE] %>% as.data.frame

SA_per = Choice[,paste('Period',TH),with=FALSE] %>% unique 
SA = 0 %>% rep(times=SA_per %>% nrow)
sampvec = Choice[,paste0('Period ',TH),with=FALSE] %>% c
SA_uper = rep(0,times=SA_per %>% nrow)
for(i in 1:(SA %>% length)){
  SA_uper[i] = (sampvec[[1]] == SA_per[[1]][i]) %>% sum
}
SA_Adopt_prop = rep(0,times=TH)
SA_Adopt_prop[SA_per[[1]]] = SA_uper
SA_Adopt_prop = (SA_Adopt_prop %>% cumsum)/N
SA_Adopt_prop %>% lines(lty=2,lwd=2)

(Choice[,Free_Market] - Choice[,"Period 50",with=FALSE])[[1]] %>% hist

