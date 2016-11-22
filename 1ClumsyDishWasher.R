##The Clumsy Dish Washer

clumsy <- 0
i <- 1
for(i in 1:1000000){
  brokendishes <- 0
  j <- 1
  for(j in 1:5){
    r <- runif(1)
    if(r < 0.2){
      brokendishes <- brokendishes + 1
    }  
  }
  if(brokendishes > 3){
    clumsy <- clumsy + 1
  }
}
print(clumsy/1000000)
