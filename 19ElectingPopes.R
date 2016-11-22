
election <- function(N,M,n){
  candidates <- sample(1:N, n)
  result <- c()
  for(i in 1:N){
    if(length(candidates)==1){
      result <- c(result,candidates[1])
    }
    else{
      result <- c(result, sample(candidates,1))
    }
  }
  res <- as.factor(result)
  df <- data.frame(table(res))
  success <- 0
  for(i in 1:length(df$Freq)){
    if(df[i,2]>=M){
      success <- 1
    }
  }
  if(success == 1){
    1
  }
  else{
    0
  }
}

###Candidates cannot vote for themselves
election2 <- function(N,M,n){
  candidates <- c(1:n)
  result <- c()
  for(i in 1:length(candidates)){
    vote <- sample(1:N,1)
    tf <- FALSE
    while(tf == FALSE){
      if(vote == candidates[i]){
        vote <- sample(1:N,1)
      }else{
        tf <- TRUE
      }
    }
    result <- c(result, vote)
  }
  for(i in 1:(N-n)){
    result <- c(result, sample(candidates,1))
  }
  
  res <- as.factor(result)
  df <- data.frame(table(res))
  success <- 0
  for(i in 1:length(df$Freq)){
    if(df[i,2]>=M){
      success <- 1
    }
  }
  if(success == 1){
    1
  }
  else{
    0
  }
}
testCC2 <- c()
testCC3 <- c()
testCC4 <- c()
test2CC2 <- c()
test2CC3 <- c()
test2CC4 <- c()
l <- 100000
#Imperial and CC test with you can vote for yourself
for(j in 1:l){

  testCC2 <- c(testCC2, election(25,17,2))
  testCC3 <- c(testCC3, election(25,17,3))
  testCC4 <- c(testCC4, election(25,17,4))
  test2CC2 <- c(test2CC2, election2(25,17,2))
  test2CC3 <- c(test2CC3, election2(25,17,3))
  test2CC4 <- c(test2CC4, election2(25,17,4))
}
sum(testCC2)/l
sum(testCC3)/l
sum(testCC4)/l
sum(test2CC2)/l
sum(test2CC3)/l
sum(test2CC4)/l
