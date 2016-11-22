###
numTrials <- 100000
numSteps <- c()
i <- 1
while(i <= numTrials){
  position <- 0
  breakLoop <- FALSE
  locVisited <- c(0)
  while(breakLoop == FALSE){
    ##goes right if less than .5, goes left if greater than .5
    if(runif(1)<.5){
      position <- position + sample(1:2,1)
      if(any((locVisited==position))){
        breakLoop <- TRUE
      }
      else{  
        locVisited <- c(locVisited, position)
        }
      }
    else{
      position <- position - sample(1:2,1)
      if(any((locVisited==position))){
        breakLoop <- TRUE
      }
      else{
        locVisited <- c(locVisited, position)
      }
    }
    numSteps <- c(numSteps, length(locVisited))
    i <- i + 1
  }
}

###this will print out the percentage of each step from 1 to 7
print(length(which(numSteps == 1))/numTrials)
print(length(which(numSteps == 2))/numTrials)
print(length(which(numSteps == 3))/numTrials)
print(length(which(numSteps == 4))/numTrials)
print(length(which(numSteps == 5))/numTrials)
print(length(which(numSteps == 6))/numTrials)
print(length(which(numSteps == 7))/numTrials)

###print a distribution of the files -- possible Pareto
library(fitdistrplus)
plotdist(numSteps)
