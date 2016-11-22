###Will get error if you input k = 0 - easy fix if you so desire

###Gets the floor each passenger gets off on
getFloor <- function(){
  floor <- sample(2:11,1)
}

###Counts the total number of stops before Mark gets off
totalStops <- function(k, steve){
  stops <- steve
  count <- 0
  i <- 1
  j <- 1
  while(i<k){
    ###if two or more get off at floor 9, then we need to change the value 
    ###because it doesn't count as an extra stop for Steve
    floor <- getFloor()
    if(floor == 9){
      stops <- c(stops, 11)
    }
    else{
      stops <- c(stops, floor)
    }
    i <- i + 1
  }
  ###if two or more get off at floor 9, then we need to change the value 
  ###to not overvalue the sample size
  
  for(j in 1:k){
    if(stops[j]<= 9){
      count <- count + 1
    }
  }
  
  count
}

###Simulation to average the results over a trial
###k = total number of passengers including mark
###ntrials = number of trials
simulation <- function(k,ntrials){
  m <- 1
  totalSim <- c()
  steve <- c(9)
  for(m in 1:ntrials){
    totalSim <- c(totalSim, totalStops(k, steve))
  }
  print((sum(totalSim))/ntrials)
}
