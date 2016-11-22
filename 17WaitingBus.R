

busStop <- function(n){
  wait <- c()
  for(j in 1:10000){
    if(n==0){
      time<-c(0)
    }
    else{
      time <- c(0)
      for(i in 1:n){
        time<- c(time,runif(1))
      }
    }
    time <- sort(time)
    person <- runif(1)
    if(time[length(time)]<person){
      wait <- c(wait, 1-person)
    }
    else{
      time2<- which(time>person)
      wait <- c(wait, time[time2[1]]-person)
    }
  }
  print(mean(wait))
}

###1 bus
busStop(0)
####2 buses
busStop(1)
busStop(2)
busStop(3)
busStop(4)#n =4 will have 5 buses