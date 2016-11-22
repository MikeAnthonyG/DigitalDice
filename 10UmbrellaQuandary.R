

###Function takes in probability of rian, number of umbrellas at his home and at his 
###office and the number of trials
umbrella <- function(prob, xUmb, yUmb, nTrials){
  i<-1
  numSuccess <- c()
  while(i <= nTrials){
    count <- 0
    loc <- -1
    x <- xUmb
    y <- yUmb
    wet <- FALSE
    while(wet == FALSE){
      rainProb <- runif(1)
      if(rainProb > prob){
        count <- count + 1
        loc <- loc * -1
      }
      else{
        if(loc == -1){
          if(x == 0){
            numSuccess <- c(numSuccess, count)
            wet <- TRUE
          }
          else{
            x <- x -1
            count <- count + 1
            y <- y + 1
            loc <- loc * -1
          }
        }
        else{
          if(y ==0){
            numSuccess <- c(numSuccess, count)
            wet <- TRUE
          }
          else{
            y <- y -1
            x <- x + 1
            count <- count + 1
            loc <- loc * -1
          }
        }
      } 
    }
    
    i <- i + 1
  }
  avg <- mean(numSuccess)
  avg
}


###this will loop through the probabilities of rain by increments of .01
###and plot the data - manipulate this data for ntrials, number of umbrellas etc
j<-.01
dataSteps <- c()
dataProb <- c()
while(j<=1){
  dataProb <- c(dataProb, j)
  dataSteps <- c(dataSteps, umbrella(j,1,1,1000))
  j <- j + .01
}
df <- cbind(dataProb)
df <- cbind(df, dataSteps)
plot(df)
