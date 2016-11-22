
i <- 4
totalNearestNeighbors <- c()
probValue <- c()
for(i in 4:50){
  car <- sort(runif(i))
  nearNeighbor <- c()
  nearNeighbor[1] <- car[2]-car[1]
  total <- 0
  ##new for-loop
  j <- 2
  for(j in 2:(length(car)-1)){
    if((car[j]-car[j-1])<(car[j+1]-car[j])){
      nearNeighbor[j] <- car[j]-car[j-1]
    }
    
    else{
      nearNeighbor[j] <- car[j+1] - car[j]
    }
  }
  nearNeighbor[j+1] <- car[j+1]-car[j]
  
  ###For-loop to look for near neighbors
  k<- 1
  for(k in 1:(i-1)){
    if(nearNeighbor[k]==nearNeighbor[k+1]){
      total <- total + 1
    }
  }
  totalNearestNeighbors <- c(totalNearestNeighbors, total)
  probValue <- c(probValue, (total/i))
  
  ###the probValue finds total number cars with mutual neighbors in a given set
  ###to find individual possibilities use 1-probValue for the cars. 
}


