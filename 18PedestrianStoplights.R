df <- data.frame()
getStops <- function(m){
  total <- 0  
  for(i in 1:1000) {
    count <- 0
    j <- m + 1
    k <- m + 1
    dir <- 0
    while (j > 1 & k > 1){
      if(runif(1) < .5){
        j <- j - 1
      }
      else{
        k <- k - 1
      }
    }
    if(j==1){
      x <- k
    }
    else{
      x <- j
    }
    while(x > 1){
      if(runif(1)<.5){
        count <- count + 1
      }
      else{
        x <- x - 1
      }
    }
    total <- total + count
  }
  total/1000
}

###depending on your computer, this could take awhile.
df <- data.frame()
for(j in 1:1000){
  df[j,1] <- getStops(j)
}

plot(x=1:1000,y=df$V1)