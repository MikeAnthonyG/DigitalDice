#Problem 4: A Curious Coin-flipping Game

##l,m,n equals the amount of coins each respective participant has
##prob is the weight given to the probability of the coin landing heads
coinSimulation <- function(l, m, n, prob){
  count <- 0
  while(l != 0 & m!=0 & n != 0){
    ml <- toss(prob)
    mm <- toss(prob)
    mn <- toss(prob)
    if(ml != mm & ml != mn){
      l <- l + 2
      m <- m - 1
      n <- n - 1
      count <- count + 1
      }
    if(mm != ml & mm != mn){
      l <- l - 1
      m <- m + 2
      n <- n - 1
      count <- count + 1
      }
    if(mn != mm & mn != ml){
      l <- l - 1
      m <- m - 1
      n <- n + 2
      count <- count + 1
    }
 
  }
  count
  
}


##toss function to get heads or tails
toss <- function(p){
  if(runif(1)<p) "H" else "T"
}


###this function will find the average amount of turns it takes for one 
###player to be "ruined"
###runs the simulation 10,000 times
findAverage <- function(l,m,n,prob){
  count2 <- c()
  i <- 1
  while(i <= 10000){
    count2 <- c(count2, coinSimulation(l,m,n,prob))
    i <- i + 1
  }
  print(mean(count2))
}
