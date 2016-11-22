#a = senators against bill, m <- senators absent 
senate <- function(a, m, nTrials){
  i<- 1
  pass <- c()
  while(i <= nTrials){
    sFor <- 100 - a
    sAgst <- a
    for(j in 1:m){
      if(runif(1) < .5){
        sFor <- sFor -1
      }
      else{
        sAgst <- sAgst -1
      }
    }
    if(sFor>sAgst){pass<-c(pass,1)}else{pass<-c(pass,0)}
    i <- i + 1
  }
  ###Probability of defeat is 1-p from the way I wrote the code
  print("Probability of the bill being defeated: ")
  print(1 - mean(pass))
}