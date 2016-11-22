

getSons <- function(){
  a <- sample(1:8,1)
  if(a == 1){
    0
  }
  else if(a == 2){
    1
  }
  else if(a == 3){
    2
  }
  else if(a == 4){
    3
  }
  else if(a == 5){
    4
  }
  else if(a == 6){
    5
  }
  else if(a == 7){
    6
  }
  else{
    7
  }
}

seventhGeneration <- function(trials){
  t <- data.frame()
  for(i in 1:trials){
    a <- c()
    iniSons <- getSons()
    a <- c(a, iniSons)
    for(j in 1:3){
      num <- 0
      for(n in 1:iniSons){
        num <- num + getSons()
      }
      a <- c(a, num)
      iniSons <- num
    }
    if(a[1] == 0){
      t <- rbind(t, c(0,0,0,0))
    }
    else if(a[2] ==0){
      t <- rbind(t, c(a[1],0,0,0))
    }
    else if(a[3]==0){
      t <- rbind(t, c(a[1],a[2],0,0))
    }
    else{
      t <- rbind(t, a)
    }
  }
  t
}
###Remember first row is second generation, second row is third generation
t <- seventhGeneration(10000)
#colMeans(t)  #looks at average number of sons
#to look at data to get percentage of haveing 4 sons left in the third generation
f <- table(as.factor(t$X13))
  