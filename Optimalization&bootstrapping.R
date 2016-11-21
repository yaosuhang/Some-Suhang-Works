################Question1:Demand Model########################
##a)Loading the data into R
library(boot)
hw1data=read.csv('Homework 1 Data.csv')
##b)Calculate the Price per unit in each week
hw1data$pricePerUnit=with(hw1data,dollars/units)
##c)Three attributes to predict log(units)
lm1=lm(log(units)~pricePerUnit,data=hw1data)
lm2=lm(log(units)~pricePerUnit+weekNum,data=hw1data)
lm3=lm(log(units)~pricePerUnit+weekOfYear,data=hw1data)
lm4=lm(log(units)~pricePerUnit+weekNum+weekOfYear,data=hw1data)
lm5=lm(log(units)~weekNum,data=hw1data)
lm6=lm(log(units)~weekNum+weekOfYear,data=hw1data)
lm7=lm(log(units)~weekOfYear,data=hw1data)
summary(lm4)##model 4 will be present in pdf
##d)lm(log(units)~price))
lm1=lm(log(units)~pricePerUnit,data=hw1data)
lm1
###########Question2 Profit Maximization############
##a) Creat a data frame
PriceData=data.frame(pricePerUnit=seq(0,2,by=0.01))
##b)demand of each price
PriceData$estimateDemand=predict(lm1,newdata=PriceData)
##c)Calculate expected Profit
PriceData$Units=exp(PriceData$estimateDemand)
PriceData$estimateProfit=(PriceData$pricePerUnit-0.6)*PriceData$Units
##d)Find Optimal Price
PriceData[which(PriceData$estimateProfit==max(PriceData$estimateProfit)),]
#e) create a function
optimalPrice = function(model){
  PriceData=data.frame(pricePerUnit=seq(0,2,by=0.01))
  PriceData$estimateDemand=predict(model,newdata=PriceData)
  PriceData$Units=exp(PriceData$estimateDemand)
  PriceData$estimateProfit=(PriceData$pricePerUnit-0.6)*PriceData$Units
  maxPrice=PriceData$pricePerUnit[PriceData$estimateProfit==max(PriceData$estimateProfit)]
  return(maxPrice)
}
optimalPrice(lm1)
 
##########################QUESTION 3  BOOTSTRAPPING###############################
##a) compute the bootstrap distribution fo optimal pric##SE=0.03671155
##b)Calculate Standard Error
##c)SE for nBootstraps=2000
##d)Plot histogram of the distribution of optimal Price
##When nBootstraps=1000
nBootstraps = 1000
nSamples = 360
bsPrice = rep(NA, nBootstraps)

for (i in 1:nBootstraps){
  bsSample = sample(nSamples,replace = TRUE)
  bsData = hw1data[bsSample,c('units','pricePerUnit')]
  bsDemandModel1 = lm(log(units) ~ pricePerUnit, data = bsData)
  bsPrice[i] = optimalPrice(bsDemandModel1)
}
mean(bsPrice)###0.83415

sd(bsPrice)###0.03570648

hist(bsPrice,main="Distribution of optimal price,1000samples") 

##When nBootstraps=2000
nBootstraps = 2000
nSamples = 360
bsPrice = rep(NA, nBootstraps)

for (i in 1:nBootstraps){
  bsSample = sample(nSamples,replace = TRUE)
  bsData = hw1data[bsSample,c('units','pricePerUnit')]
  bsDemandModel1 = lm(log(units) ~ pricePerUnit, data = bsData)
  bsPrice[i] = optimalPrice(bsDemandModel1)
}
mean(bsPrice)##0.834035

sd(bsPrice)###0.03457841
hist(bsPrice,main="Distribution of optimal price,2000samples")

############Question4####################
###After mutiple tries, when samples=4500 se=0.009617132
nBootstraps = 2000
nSamples = 360
bsPrice = rep(NA, nBootstraps)

for (i in 1:nBootstraps){
  bsSample = sample(1:nSamples,4500,replace = TRUE)
  bsData = hw1data[bsSample,c('units','pricePerUnit')]
  bsDemandModel1 = lm(log(units) ~ pricePerUnit, data = bsData)
  bsPrice[i] = optimalPrice(bsDemandModel1)
}
mean(bsPrice)###0.830455
sd(bsPrice)###0.009617132

######################Question5################
####A)B)C)
optimalProfit = function(model){
  PriceData=data.frame(pricePerUnit=seq(0,2,by=0.01))
  PriceData$estimateDemand=predict(model,newdata=PriceData)
  PriceData$Units=exp(PriceData$estimateDemand)
  PriceData$estimateProfit=(PriceData$pricePerUnit-0.6)*PriceData$Units
  maxProfit=PriceData$estimateProfit[PriceData$estimateProfit==max(PriceData$estimateProfit)]
  return(maxProfit)
}
optimalProfit(lm1)

nBootstraps = 1000
nSamples = 360
bsProfit = rep(NA, nBootstraps)

for (i in 1:nBootstraps){
  bsSample = sample(nSamples,replace = TRUE)
  bsData = hw1data[bsSample,c('units','pricePerUnit')]
  bsDemandModel1 = lm(log(units) ~ pricePerUnit, data = bsData)
  bsProfit[i] = optimalProfit(bsDemandModel1)
}
mean(bsProfit)
WekProfitLoss=mean(bsProfit)-optimalProfit(lm1)
##Profit loss in each week
WekProfitLoss##4.29931
###D)sample size=1000
nBootstraps = 1000
nSamples = 360
bsProfit = rep(NA, nBootstraps)

for (i in 1:nBootstraps){
  bsSample = sample(1:nSamples,1000,replace = TRUE)
  bsData = hw1data[bsSample,c('units','pricePerUnit')]
  bsDemandModel1 = lm(log(units) ~ pricePerUnit, data = bsData)
  bsProfit[i] = optimalProfit(bsDemandModel1)
}
mean(bsProfit)
WekProfitLoss=mean(bsProfit)-optimalProfit(lm1)
WekProfitLoss##1.070724
