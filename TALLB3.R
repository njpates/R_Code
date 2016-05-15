# Targeted Announced Adoption with Lumpsum payments with Budget Constraints

# Let '#@!@#' be the symbol for potential
# changes to the type of analysis
# Let '#!!!#' be the symbol for key looping
# areas.

set.seed(4364)
##### Loading Needed Packages #####

library(igraph)

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

theta <- sort(theta)

##### The Government's Policy Parameters #####

# Proportion of time that passes until the policy is active

pa <- .5

# Actual Period when the policy becomes active

A <- round(TH * pa,0)

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

#       5      7       9       11      13
#  6 ----- 7 ----- 8 ----- 9 ----- 10-------    <--------------  ORIGINAL TECHNOLOGY RUNG
#    \       \       \       \       \ 14   |
#     \       \       \       \       \     |
#      \6      \8      \10     \12     11 --|   <--------------  TERMINAL NODE
#       \       \       \       \     /
#         \       \       \       \ /  4
#  1       2 ----- 3 ----- 4 ----- 5            <--------------  GREEN TECHNOLOGY RUNG
#               1       2       3

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

df <- discount(time)

# Non-Dicounted Market costs of adopting through time

cost <- funccost(time)

# Dicounted Market costs of adopting through time

cost1 <- cost * df

##### Adoption Plan In Absence of Government Help ######


EM_Path_matrix <- array(0,dim = c(N,TH))

k <- 1

while (k < N + 1) {
  theta1 <- theta[k]
  
  underprof <- pi2(theta1) * df
  underprof <- round(underprof,digits = 3)
  underprof <- underprof[-1]
  
  overprof <- array(0,dim = c(TH,2))
  overprof[,1] <- pi1(theta1) * df
  overprof[,2] <- pi2(theta1) * df - funccost(time) * df
  overprof <- round(overprof,digits = 3)
  
  prof <- c(underprof,c(t(overprof)))
  
  upper0_label <-
    paste("P(",rep(0,length = TH),",",seq(1:TH),")",sep = "")
  upper1_label <-
    paste("P(",rep(1,length = TH),",",seq(1:TH),") - C",seq(1:TH),sep = "")
  upper_label <- array(0,dim = c(length(upper0_label),2))
  upper_label[,1] <- upper0_label; upper_label[,2] <- upper1_label
  upper_label <- c(t(upper_label))
  upper_count <- 1:length(upper_label)
  lower_label <-
    paste("P(",rep(1,length = TH - 1),",",seq(2,TH),")",sep = "")
  edge_label <- c(lower_label,upper_label)
  
  adj <- list()
  
  # Upper adjacency list
  for (i in 2:(TH - 1)) {
    adj[[i]] <- c(upper_count[i + 1])
  }
  
  adj[TH] <- 2 * TH + 1
  
  for (i in 1:TH) {
    adj[[TH + i]] <- c(upper_count[i + TH + 1],upper_count[i + 1])
  }
  
  adj[[2 * TH]] <- c(2 * TH + 1,2 * TH + 1)
  names(adj) <- c(upper_label)
  
  xlayout <- c(1:TH,1:TH,TH + 1) * 2
  ylayout <- c(rep(1,length = TH),rep(3,length = TH),2)
  layout <- cbind(xlayout,ylayout)
  
  adjg <- graph.adjlist(adj)
  #plot(adjg,layout=layout,edge.label=prof)
  prof <- -prof
  prof <- prof - min(prof) + 1
  #plot(adjg,layout=layout,edge.label=prof)
  
  EM_Path_matrix[k,] <-
    get.shortest.paths(adjg,from = TH + 1,to = 2 * TH + 1,weights = prof)$vpath[[1]]
  
  rm(adj)
  rm (adjg)
  
  k <- k + 1
}

Adoption_matrix <- array(0,dim = c(N,TH))
for (k in 1:N) {
  for (i in 1:TH) {
    if (EM_Path_matrix[k,i] != EM_Path_matrix[k,i + 1] - 1) {
      Adoption_matrix[k,i] = 1
    }
  }
}

for (k in 1:N) {
  if (EM_Path_matrix[k,TH] == TH) {
    Adoption_matrix[k,TH]  <- 0
  }
}

Adopt_prop <- c()

for (i in 1:TH) {
  Adopt_prop[i] <- sum(Adoption_matrix[,i])
  
}

Adopt_prop <- cumsum(Adopt_prop) / N


##########
#plot(Adopt_prop,type='l',ylim=c(0,1))
plot(
  Adopt_prop,type = 'l',ylim = c(0,1),xlab = 'Time (in Periods)',ylab = 'Cumulative Adoption (Percentage)',lwd =
    1.5
)

##### Setting up the Theta_df object #####

Theta_df <- data.frame(theta)

eligible_theta_Index <- 1:length(theta)
eligible_theta <- theta


for (i in 2:announce) {
  eligible_theta_Index[!Adoption_matrix[,i - 1] %in% 0] <- 0
  eligible_theta[eligible_theta_Index %in% 0] <- 10000
  Theta_df[[i]] <- eligible_theta
  colnames(Theta_df)[i] <- paste("Period",i - 1,sep = " ")
}


##### Adoption Plan with Neutral Expectations [announce:A] ######

# Currently active farmers in the system

eligible_theta_iteration <-
  eligible_theta[!eligible_theta_Index %in% 0]

# Government Parameters

Budget <- 2000000

subsidy <- 40000

factor_num <- Budget / subsidy

N0 <- length(eligible_theta_iteration)

prob0 <- factor_num / N0

est_N <- c()
prob <- c()

prob[1:A - 1] <- 0

i <- 1
while (i + A - 1 < TH + 1) {
  prob[i + A - 1] <- factor_num / (N0 - factor_num * (i - 1))
  
  if (prob[i + A - 1] > 1 | prob[i + A - 1] < 0) {
    prob[i + A - 1] <- 1
  }
  
  i <- i + 1
}

Probability <- prob

k <- 1

EN_Path_matrix <- array(0,dim = c(N0,TH + 1))

while (k < N0 + 1) {
  theta1 <- eligible_theta_iteration[k]
  
  underprof <- pi2(theta1) * df
  underprof <- round(underprof,digits = 3)
  underprof <- underprof[-1]
  
  overprof <- array(0,dim = c(TH,2))
  overprof[,1] <- pi1(theta1) * df
  overprof[,2] <-
    pi2(theta1) * df - (funccost(time) - subsidy * prob) *
    df
  overprof <- round(overprof,digits = 3)
  
  prof <- c(underprof,c(t(overprof)))
  
  upper0_label <-
    paste("P(",rep(0,length = TH),",",seq(1:TH),")",sep = "")
  upper1_label <-
    paste("P(",rep(1,length = TH),",",seq(1:TH),") - C",seq(1:TH),sep = "")
  upper_label <- array(0,dim = c(length(upper0_label),2))
  upper_label[,1] <- upper0_label; upper_label[,2] <- upper1_label
  upper_label <- c(t(upper_label))
  upper_count <- 1:length(upper_label)
  lower_label <-
    paste("P(",rep(1,length = TH - 1),",",seq(2,TH),")",sep = "")
  edge_label <- c(lower_label,upper_label)
  
  adj <- list()
  
  # Upper adjacency list
  for (i in 2:(TH - 1)) {
    adj[[i]] <- c(upper_count[i + 1])
  }
  
  adj[TH] <- 2 * TH + 1
  
  for (i in 1:TH) {
    adj[[TH + i]] <- c(upper_count[i + TH + 1],upper_count[i + 1])
  }
  
  adj[[2 * TH]] <- c(2 * TH + 1,2 * TH + 1)
  names(adj) <- c(upper_label)
  
  xlayout <- c(1:TH,1:TH,TH + 1) * 2
  ylayout <- c(rep(1,length = TH),rep(3,length = TH),2)
  layout <- cbind(xlayout,ylayout)
  
  adjg <- graph.adjlist(adj)
  #plot(adjg,layout=layout,edge.label=prof)
  prof <- -prof
  prof <- prof - min(prof) + 1
  #plot(adjg,layout=layout,edge.label=prof)
  
  EN_Path_matrix[k,] <-
    get.shortest.paths(adjg,from = TH + 1,to = 2 * TH + 1,weights = prof)$vpath[[1]]
  
  rm(adj)
  rm (adjg)
  
  k <- k + 1
}

EN_Adoption_matrix <- array(0,dim = c(N0,TH))
for (k in 1:N0) {
  for (i in 1:TH) {
    if (EN_Path_matrix[k,i] != EN_Path_matrix[k,i + 1] - 1) {
      EN_Adoption_matrix[k,i] = 1
    }
  }
}

for (k in 1:N0) {
  if (EN_Path_matrix[k,TH] == TH) {
    EN_Adoption_matrix[k,TH]  <- 0
  }
}

EN_Adopt_prop <- c()

for (i in 1:TH) {
  EN_Adopt_prop[i] <- sum(EN_Adoption_matrix[,i])
}

EN_Adopt_prop <- cumsum(EN_Adopt_prop)

INDEX <- 1:N0
eligible_Index_Iteration <-
  eligible_theta_Index[!eligible_theta_Index %in% 0]

for (i in announce:(A - 1)) {
  eligible_theta[1:eligible_Index_Iteration[EN_Adopt_prop[i]]] <-
    10000
  Theta_df[[ncol(Theta_df) + 1]] <- eligible_theta
  colnames(Theta_df)[i + 1] <- paste("Period",i,sep = " ")
  
}


EN_Adopt_prop <- EN_Adopt_prop + Adopt_prop[announce - 1] * N
EN_Adopt_prop <- EN_Adopt_prop / N
points(EN_Adopt_prop)

eligible_theta_Index <- 1:nrow(Theta_df)

for (i in 1:nrow(Theta_df)) {
  if (eligible_theta[i] == 10000) {
    eligible_theta_Index[i] <- 0
  }
}


########## Final Iteration ##############

q <- A

while (q < TH + 1) {
  eligible_theta_iteration <-
    eligible_theta[!eligible_theta_Index %in% 0]
  eligible_Index_iteration <-
    eligible_theta_Index[!eligible_theta_Index %in% 0]
  N0 <- length(eligible_theta_iteration)
  INDEX <- c(1:N0)
  
  prob <- rep(0,times = q - 1)
  
  i <- q
  while (length(prob) < TH) {
    prob[i] <- (Budget / subsidy) / (N0 - (Budget / subsidy) * (i - q))
    
    if (prob[i] > 1 | prob[i] < 0) {
      prob[i] <- 1
    }
    
    i <- i + 1
  }
  
  EP_Path_matrix <- array(0,dim = c(N0,TH + 1))
  
  
  
  k <- 1
  
  while (k < N0 + 1) {
    theta1 <- eligible_theta_iteration[k]
    
    
    underprof <- pi2(theta1) * df
    underprof <- round(underprof,digits = 3)
    underprof <- underprof[-1]
    
    overprof <- array(0,dim = c(TH,2))
    overprof[,1] <- pi1(theta1) * df
    overprof[,2] <-
      pi2(theta1) * df - (funccost(time) - subsidy * prob) *
      df
    overprof[q,2] <-
      pi2(theta1) * df[q] - (funccost(time[q]) - subsidy) * df[q]
    overprof[1:(q - 1),2] <-
      pi2(theta1) * df[1:(q - 1)] - (funccost(time[1:(q - 1)])) * df[1:(q - 1)]
    overprof <- round(overprof,digits = 3)
    
    prof <- c(underprof,c(t(overprof)))
    
    upper0_label <-
      paste("P(",rep(0,length = TH),",",seq(1:TH),")",sep = "")
    upper1_label <-
      paste("P(",rep(1,length = TH),",",seq(1:TH),") - C",seq(1:TH),sep = "")
    upper_label <- array(0,dim = c(length(upper0_label),2))
    upper_label[,1] <- upper0_label; upper_label[,2] <- upper1_label
    upper_label <- c(t(upper_label))
    upper_count <- 1:length(upper_label)
    lower_label <-
      paste("P(",rep(1,length = TH - 1),",",seq(2,TH),")",sep = "")
    edge_label <- c(lower_label,upper_label)
    
    adj <- list()
    
    # Upper adjacency list
    for (i in 2:(TH - 1)) {
      adj[[i]] <- c(upper_count[i + 1])
    }
    
    adj[TH] <- 2 * TH + 1
    
    for (i in 1:TH) {
      adj[[TH + i]] <- c(upper_count[i + TH + 1],upper_count[i + 1])
    }
    
    adj[[2 * TH]] <- c(2 * TH + 1,2 * TH + 1)
    names(adj) <- c(upper_label)
    
    xlayout <- c(1:TH,1:TH,TH + 1) * 2
    ylayout <- c(rep(1,length = TH),rep(3,length = TH),2)
    layout <- cbind(xlayout,ylayout)
    
    adjg <- graph.adjlist(adj)
    #plot(adjg,layout=layout,edge.label=prof)
    prof <- -prof
    prof <- prof - min(prof) + 1
    #plot(adjg,layout=layout,edge.label=prof)
    
    EP_Path_matrix[k,] <-
      get.shortest.paths(adjg,from = TH + 1,to = 2 * TH + 1,weights = prof)$vpath[[1]]
    
    rm(adj)
    rm (adjg)
    
    
    k <- k + 1
  }
  
  # Positive Adoption Matrix
  
  EP_Adoption_matrix <- array(0,dim = c(N0,TH))
  for (k in 1:N0) {
    for (i in 1:TH) {
      if (EP_Path_matrix[k,i] != EP_Path_matrix[k,i + 1] - 1) {
        EP_Adoption_matrix[k,i] = 1
      }
    }
  }
  
  for (k in 1:N0) {
    if (EP_Path_matrix[k,TH] == TH) {
      EP_Adoption_matrix[k,TH]  <- 0
    }
  }
  
  EP_Adopt_prop <- c()
  
  for (i in 1:TH) {
    EP_Adopt_prop[i] <- sum(EP_Adoption_matrix[,i])
  }
  
  EP_Adopt_prop <- cumsum(EP_Adopt_prop)
  
  Pos_Index <- EP_Adopt_prop[q]
  
  # Negative Path Matrix
  if (N0 != 0) {
    EG_Path_matrix <- array(0,dim = c(N0,TH + 1))
    
    k <- 1
    
    while (k < N0 + 1) {
      theta1 <- eligible_theta_iteration[k]
      
      
      underprof <- pi2(theta1) * df
      underprof <- round(underprof,digits = 3)
      underprof <- underprof[-1]
      
      overprof <- array(0,dim = c(TH,2))
      overprof[,1] <- pi1(theta1) * df
      overprof[,2] <-
        pi2(theta1) * df - (funccost(time) - subsidy * prob) *
        df
      overprof[1:q,2] <-
        pi2(theta1) * df[1:q] - (funccost(time[1:q])) * df[1:q]
      overprof <- round(overprof,digits = 3)
      
      prof <- c(underprof,c(t(overprof)))
      
      upper0_label <-
        paste("P(",rep(0,length = TH),",",seq(1:TH),")",sep = "")
      upper1_label <-
        paste("P(",rep(1,length = TH),",",seq(1:TH),") - C",seq(1:TH),sep = "")
      upper_label <- array(0,dim = c(length(upper0_label),2))
      upper_label[,1] <-
        upper0_label; upper_label[,2] <- upper1_label
      upper_label <- c(t(upper_label))
      upper_count <- 1:length(upper_label)
      lower_label <-
        paste("P(",rep(1,length = TH - 1),",",seq(2,TH),")",sep = "")
      edge_label <- c(lower_label,upper_label)
      
      adj <- list()
      
      # Upper adjacency list
      for (i in 2:(TH - 1)) {
        adj[[i]] <- c(upper_count[i + 1])
      }
      
      adj[TH] <- 2 * TH + 1
      
      for (i in 1:TH) {
        adj[[TH + i]] <- c(upper_count[i + TH + 1],upper_count[i + 1])
      }
      
      adj[[2 * TH]] <- c(2 * TH + 1,2 * TH + 1)
      names(adj) <- c(upper_label)
      
      xlayout <- c(1:TH,1:TH,TH + 1) * 2
      ylayout <- c(rep(1,length = TH),rep(3,length = TH),2)
      layout <- cbind(xlayout,ylayout)
      
      adjg <- graph.adjlist(adj)
      #plot(adjg,layout=layout,edge.label=prof)
      prof <- -prof
      prof <- prof - min(prof) + 1
      #plot(adjg,layout=layout,edge.label=prof)
      
      EG_Path_matrix[k,] <-
        get.shortest.paths(adjg,from = TH + 1,to = 2 * TH + 1,weights = prof)$vpath[[1]]
      
      rm(adj)
      rm (adjg)
      
      
      k <- k + 1
    }
    
    # Positive Adoption Matrix
    
    EG_Adoption_matrix <- array(0,dim = c(N0,TH))
    for (k in 1:N0) {
      for (i in 1:TH) {
        if (EG_Path_matrix[k,i] != EG_Path_matrix[k,i + 1] - 1) {
          EG_Adoption_matrix[k,i] = 1
        }
      }
    }
    
    for (k in 1:N0) {
      if (EG_Path_matrix[k,TH] == TH) {
        EG_Adoption_matrix[k,TH]  <- 0
      }
    }
    
    EG_Adopt_prop <- c()
    
    for (i in 1:TH) {
      EG_Adopt_prop[i] <- sum(EG_Adoption_matrix[,i])
    }
    
    EG_Adopt_prop <- cumsum(EG_Adopt_prop)
    
    Neg_Index <- EG_Adopt_prop[q]
    
    if (round(Budget / subsidy,digits = 0) < Pos_Index) {
      Index_Offer <-
        sample(1:Pos_Index,round(Budget / subsidy,digits = 0),0)
      Index_Offer <- sort(Index_Offer)
      for (i in 1:length(Index_Offer)) {
        eligible_Index_Iteration[Index_Offer[i]] <- 0
      }
    }
    
    if (round(Budget / subsidy,digits = 0) > Pos_Index) {
      eligible_Index_Iteration[c(1:Pos_Index)] <- 0
    }
    
    if (round(Budget / subsidy,digits = 0) == Pos_Index) {
      eligible_Index_Iteration[c(1:Pos_Index)] <- 0
    }
    
    
    if (Neg_Index > 0) {
      eligible_Index_Iteration[c(1:Neg_Index)] <- 0
    }
    
    ######@PROBLEM AREA@#########
    eligible_Index_Iteration <-
      eligible_Index_Iteration[!eligible_Index_Iteration %in% 0]
    eligible_theta_Index <- rep(0,times = N)
    for (i in 1:length(eligible_Index_Iteration)) {
      eligible_theta_Index[eligible_Index_Iteration[i]] <-
        eligible_Index_Iteration[i]
      
    }
  }
  eligible_theta[eligible_theta_Index %in% 0] <- 10000
  
  Theta_df[[ncol(Theta_df) + 1]] <- eligible_theta
  colnames(Theta_df)[ncol(Theta_df)] <- paste("Period",q,sep = " ")
  ######@@#########
  
  
  q <- q + 1
}

test <- c()

for (i in 1:ncol(Theta_df)) {
  test[i] <- length(Theta_df[[i]][Theta_df[[i]] %in% 10000]) / N
  
}
test <- test[-1]
points(test,pch = 3,col = "blue")
lines(rep(announce,times = 10),seq(0,1,length = 10))
lines(rep(A,times = 10),seq(0,1,length = 10))
eligible_theta_Index

Budget / subsidy
test
Index_Offer

eligible_theta_Index
eligible_Index_Iteration
eligible_Index_Iteration[!eligible_Index_Iteration %in% 0]
Pos_Index
prob
plot(
  Adopt_prop,type = 'l',main = paste(
    "Budget = ",Budget," Subsidy = ",round(subsidy,digits = 2),"\n\ Active Period = ", A, " Announcement Period = ",announce
  ),ylim = c(0,1)
  ,xlab = 'Year',ylab = 'Percentage Adopted Green Technology'
)
points(c(announce:50),EN_Adopt_prop[announce:50])
points(test,pch = 3,col = "blue")
lines(rep(A,times = 10),seq(0,1,length = 10),col = 'grey',lty = 2)
lines(rep(announce,times = 10),seq(0,1,length = 10),col = 'grey',lty = 2)
legend(
  3,.9,pch = c(1,3),col = c("black","blue"),legend = c("Adoption Under Neutral Expectations","Simulated Adoption")
)

plot(theta,pi1(theta),type = 'l',ylim = c(0,max(pi2(theta))))
lines(theta,pi2(theta),lty = 2)
for (i in 1:length(time - 1)) {
  lines(theta,(pi2(theta) - funccost(time[i])))
}

plot(time * pi1(theta[600]),xlim = c(5,50))
lines(time * pi2(theta[600]) - funccost(time))
plot(time * pi1(theta[600]) - (time * pi2(theta[600]) - funccost(time)),type =
       'l')
time * pi2(theta[600] - funccost(time))
change <- c()
for (i in 1:length(time) - 1) {
  change[i] <-
    time[i + 1] * pi2(theta[600] - funccost(time[i + 1])) - time[i] * pi2(theta[600] +
                                                                            funccost(time[i]))
}

change
plot(change)
