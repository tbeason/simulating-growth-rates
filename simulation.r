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

tbill[,1] <- as.Date(tbill[,1], format="%m-%d-%Y")
tbill.xts <- as.xts(tbill[,-1], order.by=tbill[,1])

#calc monthly returns
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

#noBM will not contain the benchmark time series
noBM <- ind.ret[,-ncol(ind.ret)]

rC <- as.numeric(CAT.ret)

expected <- numeric(length(rC)-1)
stdev <- numeric(length(rC)-1)

retI <- as.numeric(noBM[1,])
retS <- sort(retI)
priorC <- pnorm(retS,mean=mean(retS),sd=sd(retS))
priorD <- c(priorC[1],diff(priorC))
joint <- 0.5*priorD
posterior1 <- joint/sum(joint)
weighted <- posterior1*retS
expected[1] <- sum(weighted)  
stdev[1] <- sqrt(sum((priorD-expected[1])^2*posterior1))

samples <- rnorm(1000,mean=expected[1],sd=stdev[1])
guess <- mean(samples)


for(i in 2:length(expected))
{
  retI <- as.numeric(noBM[i,])
  retS <- sort(retI)
  priorC <- pnorm(retS,mean=mean(retS),sd=sd(retS))
  priorD <- c(priorC[1],diff(priorC))
  
  likelihood <- pnorm(rnorm(length(priorD),mean=mean(rC[1:i]),sd=sd(rC[1:i])),mean=mean(rC[1:i]),sd=sd(rC[1:i]))
  joint <- priorD*likelihood
  
  posterior <- joint/sum(joint)
  weighted <- posterior*retS
  expected[i] <- sum(weighted) 
  stdev[i] <- sqrt(sum((priorD-expected[i])^2*posterior))
}
matplot(cbind(rC[2:42],expected),type="l",main="Strictly Distribution Based")
samples1 <- data.frame(ret=rnorm(1000,mean=expected[length(expected)],sd=stdev[length(expected)]))


#### projection 2
#calc monthly returns
for(i in 1:ncol(ind.xts)){
  if(i == 1){
    ind.daily <- dailyReturn(ind.xts[,i])    
  }
  else
  {
    ind.daily <- cbind(ind.daily,dailyReturn(ind.xts[,i]))  
  }
}
names(ind.daily) <- names(ind.xts)

CAT.daily <- dailyReturn(CAT.xts)
names(CAT.daily) <- names(CAT.xts)
benchmark <- ind.daily$Benchmark
benchMonthly <- ind.ret$Benchmark

comps <- ind.daily[,-ncol(ind.daily)]
betas <- apply.monthly(comps,FUN=CAPM.beta,benchmark)
CAT.beta <- apply.monthly(CAT.daily,FUN=CAPM.beta,benchmark)

expCAPM <- numeric(length(rC)-1)
sdCAPM <- numeric(length(rC)-1)

retI <- as.numeric(tbill.xts[1])+as.numeric(betas[1,])*(as.numeric(benchMonthly[1])-as.numeric(tbill.xts[1]))
retS <- sort(retI)
priorC <- pnorm(retS,mean=mean(retS),sd=sd(retS))
priorD <- c(priorC[1],diff(priorC))

joint <- 0.5*priorD
posterior1 <- joint/sum(joint)
weighted <- posterior1*retS
expCAPM[1] <- sum(weighted)  
sdCAPM[1] <- sqrt(sum((priorD-expCAPM[1])^2*posterior1))

for(i in 2:length(expCAPM))
{
  retI <- as.numeric(tbill.xts[i])+as.numeric(betas[i,])*(as.numeric(benchMonthly[i])-as.numeric(tbill.xts[i]))  
  retS <- sort(retI)
  priorC <- pnorm(retS,mean=mean(retS),sd=sd(retS))
  priorD <- c(priorC[1],diff(priorC))
  
  likelihood <- pnorm(rnorm(length(priorD),mean=mean(rC[1:i]),sd=sd(rC[1:i])),mean=mean(rC[1:i]),sd=sd(rC[1:i]))
  joint <- priorD*likelihood
  
  posterior <- joint/sum(joint)
  weighted <- posterior*retS
  expCAPM[i] <- sum(weighted)  
  sdCAPM[i] <- sqrt(sum((priorD-expCAPM[i])^2*posterior))
}
matplot(cbind(rC[2:42],expCAPM),type="l",main="Using CAPM")
samples2 <- data.frame(ret=rnorm(1000,mean=expCAPM[length(expCAPM)],sd=sdCAPM[length(expCAPM)]))

mseBayes <- sqrt((expected-rC[2:42])^2)
mseCAPM <- sqrt((expCAPM-rC[2:42])^2)
summary(mseBayes)
summary(mseCAPM)

ret <- merge(CAT.ret[2:42],expected,expCAPM)
names(ret) <- c("CAT","Plain","CAPM")
chart.TimeSeries(ret,legend.loc="topright",main="CAT Returns - Estimated vs Actual",ylab="Monthly Return",cex.legend=0.6)


samples1$type <- 'plain'
samples2$type <- 'CAPM'
samp <- rbind(samples1,samples2)
ggplot(samp, aes(ret, fill = type)) + 
  geom_histogram(alpha = 0.5, position = 'identity') +
  xlab("Return") +
  ggtitle("Histogram of Final Period Returns")
