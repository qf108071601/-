library(tidyverse)
library(magrittr)
library(psych)
library(nFactors)
library(corrplot)


data = read.csv("responses.csv")

for (i in 1:150) {
  data[is.na(data[,i]),i] <- median(data[,i],na.rm = T)
}

for (i in 1:150) {
  data[,i] = as.numeric(data[,i])
}

colnames(data)

data.try = data[,c(1:19)]

ID <- seq.int(nrow(data.try))
data.try <- cbind(ID,data.try)


corr <- cor(data.try[,-1])
corrplot(corr, method="circle", tl.cex = 0.7)

#KMO TEST
KMO(data.try[,-1])


# 決定放幾個Factor
ev <- eigen(cor(data.try[,-1]))
ap <- parallel(subject=nrow(data.try[,-1]),var=ncol(data.try[,-1]),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


#跑Factor Analysis
EFA = fa(data.try[,-1],nfactors = 3,
         rotate = "varimax",
         fm = "pa",
         scores=T)
print(EFA)

factor.plot(EFA)	

EFA$loadings


# View the results
fa.diagram(EFA)


plot(density(EFA$scores, na.rm = TRUE), 
     main = "Factor Scores")


error.dots(data.try[,-1])
error.bars(data.try[,-1],eyes = T,col = "gray")

