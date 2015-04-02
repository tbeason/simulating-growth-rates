# import data
industry <- read.csv("~/GitHub/simulating-growth-rates/industry.csv", stringsAsFactors=FALSE)
CATprice <- read.csv("~/GitHub/simulating-growth-rates/CATprice.csv", stringsAsFactors=FALSE)
tbill <- read.csv("~/GitHub/simulating-growth-rates/tbill.csv", stringsAsFactors=FALSE)

# convert to xts (time series)
industry[,1] <- as.Date(industry[,1], format="%m-%d-%Y")
ind.xts <- as.xts(industry[,-1], order.by=industry[,1])

CATprice[,1] <- as.Date(CATprice[,1], format="%m-%d-%Y")
CAT.xts <- as.xts(CATprice[,-1], order.by=CATprice[,1])
names(CAT.xts) <- "CAT"

for(i in 1:ncol(ind.xts)){
  if(i == 1){
    ind.ret <- monthlyReturn(ind.xts[,i])    
  }
  else
  {
    ind.ret <- cbind(ind.ret,monthlyReturn(ind.xts[,i]))  
  }
}
names(ind.ret) <- names(ind.xts)

CAT.ret <- monthlyReturn(CAT.xts)
names(CAT.ret) <- names(CAT.xts)

noBM <- ind.ret[,-ncol(ind.ret)]

ret1 <- as.numeric(noBM[1,])
dist1 <- dnorm(ret1,mean=mean(ret1),sd=sd(ret1))
r1c <- as.numeric(CAT.ret[1])
prior1 <- 0.5*dist1/sum(dist1)

samples <- pnorm(sample(prior1,10000,replace=TRUE),mean=mean(ret1),sd=sd(ret1))
guess <- mean(samples)
