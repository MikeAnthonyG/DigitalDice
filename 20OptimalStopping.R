
###pop = number of population
###desLev = desperation level = the ranking in which you will settle for marriage
###ss = sample space, how many samples to set your base date rate
successfulMate <- function(pop, desLev, ss){
  dates <- sample(1:pop,pop)
  sampleDates <- dates[1:ss]
  dates <- dates[!dates %in% sampleDates]
  ###which is the lowest rank in the sample dates
  highSampleDate <- min(sampleDates)
  found <- FALSE
  i <- 1
  chosenMate <- 100
  while(found == FALSE & i <= length(dates)){
    if(dates[i]<= highSampleDate){
      chosenMate <- dates[i]
      found <- TRUE
    }else{
      i <- i + 1
    }
  }
  if(chosenMate <= desLev){
    1
  }
  else{
    0
  }
}

###answers first question
pop <-11
desperation <- c(2:5)
x <- c()
y <- c()
t <- data.frame()
trials <- 10000
for(i in 1:pop){
  for(j in 1:length(desperation)){
    for(n in 1:trials){
      x <- c(x, successfulMate(pop,desperation[j],i))
    }
    y <- c(y, mean(x))
    x <- c()
  }
  t <- rbind(t,y)
  y <- c()
}