library(dplyr)
#Questions to answer:
#1: Average total waiting time (waiting + serving)
#2: Max time spent waiting (unluckiest customer)
#3: Average length of waiting queue
#4: Max length of queue
#5: The difference a second clerk makes
#6: Fraction of the work day clerks sit idle

oneServer <- function(u,lambda){
  df <- data.frame(time=sort(sample(1:36000, lambda*10)))
  df[1,2] <- 0
  qTime <- 0
  for(i in 2:(lambda*10)){
    if(qTime > 0 & df[i,1]-df[i-1,1]<(3600/u)){
      qTime <- qTime + df[i,1]-df[i-1,1]
      df[i,2] <- qTime
    } 
    else if(qTime == 0 & df[i,1]-df[i-1,1]<(3600/u)){
      qTime <- qTime + df[i,1]-df[i-1,1]
      df[i,2] <- qTime
    }
    else if(df[i,1]-df[i-1,1]>qTime){
      qTime <- 0
      df[i,2] <- 0
    }
    else{
      qTime <- qTime - (df[i,1]-df[i-1,1])
      df[i,2] <- qTime
    }
  }
  df <- mutate(df,totalTime = df$V2 + 90)
  ###finds queue times
  line <- 0
  count <- 0
  lineQueues <- c()
  for(j in 1:300){
    if(df[j,2]==0){
      if(count != 0){
        lineQueues <- c(lineQueues, count)
      }
      if(count > line){
        line <- count
      }
      count <- 0
    }
    else{
      count <- count + 1
    }
  }
  dFin <- c(mean(df$totalTime),max(df$V2),max(lineQueues),mean(lineQueues))
  dFin
}

twoServer <- function(u,lambda){
  df <- data.frame(time=sort(sample(1:36000, lambda*10)))
  df[1,2] <- 0
  qTime <- 0
  qTime2 <- 0
  for(i in 2:(lambda*10)){
    if((qTime == 0 & qTime2 == 0)|(qTime==0 & qTime2 >0)){
      if(qTime > 0 & df[i,1]-df[i-1,1]<(3600/u)){
        qTime <- qTime + df[i,1]-df[i-1,1]
        df[i,2] <- qTime
      } 
      else if(qTime == 0 & df[i,1]-df[i-1,1]<(3600/u)){
        qTime <- qTime + df[i,1]-df[i-1,1]
        df[i,2] <- qTime
      }
      else if(df[i,1]-df[i-1,1]>qTime){
        qTime <- 0
        df[i,2] <- 0
      }
      else{
        qTime <- qTime - (df[i,1]-df[i-1,1])
        df[i,2] <- qTime
      }
    }
  else if((qTime2 == 0 & qTime == 0)|(qTime2==0 & qTime >0)){
    if(qTime2 > 0 & df[i,1]-df[i-1,1]<(3600/u)){
      qTime2 <- qTime2 + df[i,1]-df[i-1,1]
      df[i,2] <- qTime2
    } 
    else if(qTime2 == 0 & df[i,1]-df[i-1,1]<(3600/u)){
      qTime2 <- qTime2 + df[i,1]-df[i-1,1]
      df[i,2] <- qTime2
    }
    else if(df[i,1]-df[i-1,1]>qTime2){
      qTime2 <- 0
      df[i,2] <- 0
    }
    else{
      qTime2 <- qTime2 - (df[i,1]-df[i-1,1])
      df[i,2] <- qTime2
    }
  }
  ###when both qTime and qTime 2 are greater than zero  
  else{
    if(qTime < qTime2){
      if(qTime > 0 & df[i,1]-df[i-1,1]<(3600/u)){
        qTime <- qTime + df[i,1]-df[i-1,1]
        df[i,2] <- qTime
      } 
      else if(qTime == 0 & df[i,1]-df[i-1,1]<(3600/u)){
        qTime <- qTime + df[i,1]-df[i-1,1]
        df[i,2] <- qTime
      }
      else if(df[i,1]-df[i-1,1]>qTime){
        qTime <- 0
        df[i,2] <- 0
      }
      else{
        qTime <- qTime - (df[i,1]-df[i-1,1])
        df[i,2] <- qTime
      }
    }
    else{
      if(qTime2 > 0 & df[i,1]-df[i-1,1]<(3600/u)){
        qTime2 <- qTime2 + df[i,1]-df[i-1,1]
        df[i,2] <- qTime2
      } 
      else if(qTime2 == 0 & df[i,1]-df[i-1,1]<(3600/u)){
        qTime2 <- qTime2 + df[i,1]-df[i-1,1]
        df[i,2] <- qTime2
      }
      else if(df[i,1]-df[i-1,1]>qTime2){
        qTime2 <- 0
        df[i,2] <- 0
      }
      else{
        qTime2 <- qTime2 - (df[i,1]-df[i-1,1])
        df[i,2] <- qTime2
      }
    }
  }
    
}
  df <- mutate(df,totalTime = df$V2 + 90)
  ###finds queue times
  line <- 0
  count <- 0
  lineQueues <- c()
  for(j in 1:300){
    if(df[j,2]==0){
      if(count != 0){
        lineQueues <- c(lineQueues, count)
      }
      if(count > line){
        line <- count
      }
      count <- 0
    }
    else{
      count <- count + 1
    }
  }
  dFin <- c(mean(df$totalTime),max(df$V2),max(lineQueues),mean(lineQueues))
  dFin
}


###Run Tests
one <- c()
for(e in 1:5){
  one <- rbind(one,oneServer(25,40))
}
two <- c()
for(e in 1:5){
  two <- rbind(two,twoServer(25,40))
}
