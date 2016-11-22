#Malt Shop

i <- 1
success<- 0
nrep <- 1000000

for(i in 1:nrep){
  bill <- sample(1:30, 1)
  lil <- sample(0:30,1)
  if(lil < bill){
    if(bill - lil < 5){
      success <- success + 1
    }
  }
  if(bill < lil){
    if(lil-bill < 7){
      success <- success + 1
    }
  }
  if(bill==lil){success <- success + 1}
}

print(success/nrep)