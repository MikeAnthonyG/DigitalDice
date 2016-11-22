

appealsCourt <- function(a,b,c,d,e){
  judges <- c(getJud(a),getJud(b),getJud(c),getJud(d),getJud(e))
  if(sum(judges)>=3){
    1
  }else{
    0
  }
}
appealsCourtE <- function(a,b,c,d){
  judgeAE <- getJud(a)
  judges <- c(judgeAE,getJud(b),getJud(c),getJud(d),judgeAE)
  if(sum(judges)>=3){
    1
  }else{
    0
  }
}
getJud <- function(p){
  if(runif(1)<p){
    1
  }
  else{
    0
  }
}

normalCase <- c()
for(i in 1:100000){
  normalCase<- c(normalCase,appealsCourt(.95,.95,.9,.9,.8))
}
print(1-sum(normalCase)/length(normalCase))

judgeE <- c()
for(j in 1:100000){
  judgeE <- c(judgeE, appealsCourtE(.95,.95,.9,.9))
}
print(1-sum(judgeE)/length(judgeE))
