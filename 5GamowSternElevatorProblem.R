

##Warning, only works on 2 elevators

###Gets elevator direction 1 equates to up and 2 equates to down
floorDirection <- function(){
  if(runif(1) < .5) 1 else 2
}

i <- 1
success <- 0
trials <- 100000

for(i in 1:trials){
  elePosition1 <- c(sample(1:7,1),floorDirection())
  elePosition2 <- c(sample(1:7,1),floorDirection())
  if(elePosition1[1] == 1){
    success <- success + 1
  }
  if(elePosition2[1] == 1){
    success <- success + 1
  }
 
}

print(1-(success/trials))