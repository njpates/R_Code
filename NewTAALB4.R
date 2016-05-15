# Targeted Announced Adoption with Lumpsum payments with Budget Constraints

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
# policy just as it is beginning. We can alter thisS
# assumption by taking the market expectations adoption
# with the expectation neutral adoption. We assume that
# when the government offers the subsidy, they will select
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

N <- 1000

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

A <- TH * pa %>% round(digits=0)

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
  
  #!##!#!#!#!#!#!#!#!#!#!#!#!    
  graph_create = function(TH){
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
    layout = array(c(xlayout,ylayout),dim=c(8,2))
    adjg <- graph.adjlist(adj)
    list(graph = adjg,layout=layout,label = edge_label)
  }
  #plot((TH %>% graph_create)$graph,layout=(TH %>% graph_create)$layout,edge.label=(TH %>% graph_create)$label)
  #plot((TH %>% graph_create)$graph,layout=(TH %>% graph_create)$layout,edge.label=prof)
  
  #!##!#!#!#!#!#!#!#!#!#!#!#!
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



