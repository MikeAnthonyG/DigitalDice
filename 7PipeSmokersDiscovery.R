ntrials <- 10000
i <- 1
total <- c()
while(i<=ntrials){
  booklet1 <- 40
  booklet2 <- 40
  count <- 0
  while(booklet1 != 0 & booklet2 != 0){
    if(runif(1)<.5){
      booklet1 <- booklet1 - 1
      count <- count + 1
    }
    else{
      booklet2 <- booklet2 - 1
      count <- count + 1
    }
  }
  total <- c(total, count)
  i<-i + 1
}
print(sum(total)/ntrials)