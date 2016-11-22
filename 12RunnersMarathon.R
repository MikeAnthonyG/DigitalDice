library(fitdistrplus)
library(ggplot2)
library(reshape2)
sampleSize <- c(.02,.05,.1,.2)
df <- data.frame()
for(j in 1:4){
  percentError <- c()
  for(i in 1:10000){
    sizeN <- sample(100:1000, 1)
    sample <- sample(1:sizeN, sampleSize[j]*sizeN,replace=FALSE)
    estimateN <- (max(sample)*((length(sample)+1)/length(sample))) - 1
    df[i,j] <- ((estimateN - sizeN)/sizeN) * 100
    
  }
  
}

###Graph the data
col <-  c("twoPercent","fivePercent","tenPercent","twentyPercent")
colnames(df) <- col

melt_df <- melt(df)
head(melt_df)


##two different graphs - one overlayed, the other seperated into four different plots
g <- ggplot(melt_df) + geom_histogram(aes(x=value,y=(..count..)/sum(..count..), fill=variable),
                                 alpha=0.3, binwidth=2, position="identity")

e <- ggplot(melt_df) + geom_histogram(aes(x=value,y=(..count..)/sum(..count..)),
                                      alpha=0.3, binwidth=2, position="identity")
e <- e + facet_wrap(~variable)

###different graphs with cdfs
plotdist(df$twoPercent)
plotdist(df$fivePercent)
plotdist(df$tenPercent)
plotdist(df$twentyPercent)
g
e
