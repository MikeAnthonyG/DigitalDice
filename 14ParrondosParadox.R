

###hundred rounds
gameA <- function(e,m){
  d <- data.frame(0)
  i<-1
  while(i <= 100){
    if(runif(1)< (.5-e)){
      m <- m + 1
    }
    else{
      m <- m - 1
    }
    d[i,] <- m
    i <- i + 1
  } 
  d
}
gameB <- function(e,m){
  d <- data.frame(0)
  i <- 1
  while(i <= 100){
    if(m %% 3 == 0){
      if(runif(1)< (.1 - e)){
        m <- m + 1
      }
      else{
        m <- m -1
      }
    }
    else{
      if(runif(1)<.75 - e){
        m <- m + 1
      }
      else{
        m <- m - 1
      }
    }
    d[i,] <- m
    i <- i + 1
  }
  d
}

### runs game A 10,000 times - M is the capital00000000
trialA <- function(e,m){
  dA <- gameA(e,m)
  for(i in 2:10000){
    dA <- cbind(dA, gameA(e,m)) 
  }
  dA
}

trialB <- function(e,m){
  dA <- gameB(e,m)
  for(i in 2:10000){
    dA <- cbind(dA, gameA(e,m)) 
  }
  dA
}


#Switches randomly between games
trialAB <- function(e){
  d <- data.frame()
  for(i in 1:10000){
    m <- 0
    for(j in 1:100){
      if(runif(1)<.5){
        m <- roundA(e,m)
        d[j,i] <- m 
      }
      else{
        m <- roundB(e,m)
        d[j,i] <- m
      }
    }
  }
  d
  
}

roundA <- function(e,m){
  if(runif(1)< (.5-e)){
    m <- m + 1
  }
  else{
    m <- m - 1
  }
  m
}
roundB <- function(e,m){
    if(m %% 3 == 0){
      if(runif(1)< (.1 - e)){
        m <- m + 1
      }
      else{
        m <- m -1
      }
    }
    else{
      if(runif(1)<.75 - e){
        m <- m + 1
      }
      else{
        m <- m - 1
      }
    }
  m
}

###get data and get the means of each row and plot
triA <- trialA(.005)
meansA <- data.frame(ID=c(1:100), Means=rowMeans(triA[,-1]))
plot(meansA)

###gets the sample functions of game B and plots the ensemble average
triB <- trialB(.005)
meansB <- data.frame(ID=c(1:100), Means = rowMeans(triB[,-1]))
plot(meansB)

#gets the sample functions of switching randombly between game A and B and 
#plots the ensemble average
triAB <- trialAB(.005)
meansAB <- data.frame(ID=c(1:100), Means = rowMeans(triAB[,-1]))
plot(meansAB)
