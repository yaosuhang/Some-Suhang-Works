########################PART A#####################
library(reshape2)
library(plm)
#############Q1 cleaning data########
##loading the dataframe
HW2Prices=read.csv('HW2Prices.csv',header = T)
HW2Units=read.csv('HW2Units.csv',header = T)
HW2Prices$X=NULL
HW2Units$X=NULL
##create empty dataframe
q1DB <- matrix(NA,nrow=35900,ncol=6)
colnames(q1DB) <- c("panelID","weekNum","consumerID","pricePerUnit","units","isPurchase")
q1DB <- as.data.frame(q1DB)
###Put data in each column
q1DB$panelID=seq(1,35900,by=1)
q1DB$weekNum=rep(c(1:359),each=100)
q1DB$consumerID=rep(c(1:100),359)
price=melt(HW2Prices)
q1DB$pricePerUnit=price$value
Units=melt(HW2Units)
q1DB$units=Units$value
q1DB$isPurchase=ifelse(q1DB$units>0,1,0)
save.image("suhang.YaoHW2.Rdata")
###########Q3############
## linear probability model
model1 <- lm(isPurchase ~ pricePerUnit, data=q1DB)
summary(model1)

## logit 
logitmodel <- glm(isPurchase ~ pricePerUnit, family=binomial(link=logit), data=q1DB)
summary(logitmodel)
## probit
probitmodel <- glm(isPurchase ~ pricePerUnit, family=binomial(link=probit), data=q1DB)
summary(probitmodel)
##a) Creat a data frame
isPurchaseData=data.frame(pricePerUnit=seq(-5,5.5,by=0.1))
##b)demand of each price
isPurchaseData$linear=predict(model1,newdata=isPurchaseData)
isPurchaseData$Logit=predict(logitmodel,type="response",newdata=isPurchaseData)
isPurchaseData$Probit=predict(probitmodel,type="response",newdata=isPurchaseData)

##draw the Plot
plot(y=isPurchaseData$linear, x=isPurchaseData$pricePerUnit,
     main = "Probability of Purchase at each Price level", xlab = "Price", ylab = "Probability",
     col="green", pch=16)
points(y=isPurchaseData$Logit, x=isPurchaseData$pricePerUnit, col="blue", pch=16)
points(y=isPurchaseData$Probit, x=isPurchaseData$pricePerUnit, col="red", pch=16)
abline(v=2.1)
abline(v=3.7)
legend.text<-c("linear","Logit","Probit")
legend(0,0.8,legend=legend.text, col=c("green", "blue", "red"), pch=16)

###########QA6,200consumer dataset 
##loading the dataframe
HW2QA6Prices=read.csv('HW2QA6Prices.csv',header = T)
HW2QA6Units=read.csv('HW2QA6Units.csv',header = T)

###remove the first column
HW2QA6Prices$X=NULL
HW2QA6Units$X=NULL
###build an empty dataframe
q6DB <- matrix(NA,nrow=71800,ncol=6)
colnames(q6DB) <- c("panelID","weekNum","consumerID","pricePerUnit","units","isPurchase")
q6DB <- as.data.frame(q6DB)
###Put data in each column
q6DB$panelID=seq(1,71800,by=1)
q6DB$weekNum=rep(c(1:718),each=100)
q6DB$consumerID=rep(c(1:100),718)
price6=melt(HW2QA6Prices)
q6DB$pricePerUnit=price6$value
Units6=melt(HW2QA6Units)
q6DB$units=Units6$value
q6DB$isPurchase=ifelse(q6DB$units>0,1,0)
## linear probability model

QA6model1 <- lm(isPurchase ~ pricePerUnit, data=q6DB)
summary(QA6model1)
## logit 

QA6logitmodel <- glm(isPurchase ~ pricePerUnit, family=binomial(link=logit), data=q6DB)
summary(QA6logitmodel)
## probit

QA6probitmodel <- glm(isPurchase ~ pricePerUnit, family=binomial(link=probit), data=q6DB)
summary(QA6probitmodel)
##a) Creat a data frame
isPurchaseDataQA6=data.frame(pricePerUnit=seq(-5,5.5,by=0.1))
##b)demand of each price
isPurchaseDataQA6$linear=predict(QA6model1,newdata=isPurchaseDataQA6)
isPurchaseDataQA6$Logit=predict(QA6logitmodel,type="response",newdata=isPurchaseDataQA6)
isPurchaseDataQA6$Probit=predict(QA6probitmodel,type="response",newdata=isPurchaseDataQA6)
##############PART B
##Q1
install.packages("plm")
library(plm)
require("plm")
newq1DB=plm.data(q1DB,c('consumerID','weekNum'))
poolingModel = plm(units~pricePerUnit,data=q1DB,model = "pooling")
randomModel = plm(units~pricePerUnit,data=newq1DB,model = "random")
differentModel = plm(units~pricePerUnit,data=q1DB,model = "fd")
withinModel = plm(units~pricePerUnit,data=newq1DB ,model = "within")
summary(poolingModel)
summary(randomModel)
summary(differentModel)
summary(withinModel)

#########Q2 lm model
altpoolingmodel=lm(units~pricePerUnit,data=q1DB)
altwithinModel=lm(units~pricePerUnit+factor(consumerID),data=q1DB)
summary(altpoolingmodel)
summary(altwithinModel)

#######Q3
AIC(altpoolingmodel)
AIC(altwithinModel)
######Q4
purchaseconsumerdata=subset(q1DB,q1DB$isPurchase>0)
newwithinModelpurchase=plm.data(purchaseconsumerdata,c('consumerID','weekNum'))
withinModelpurchase = plm(units~pricePerUnit,data=newwithinModelpurchase ,model = "within")
summary(withinModelpurchase)
#######################PartC
###############Q1
q1DB.byconsumer=aggregate(q1DB[,"units"],list(consumer=q1DB$consumerID), FUN=sum)
summary(q1DB.byconsumer)
q1DB$fifty=ifelse(q1DB.byconsumer$x>=83,1,0)
typle1consumer=subset(q1DB,q1DB$fifty==0)
typle2consumer=subset(q1DB,q1DB$fifty==1)

typle1logitmodel <- glm(isPurchase ~ pricePerUnit, family=binomial(link=logit), data=typle1consumer)
summary(typle1logitmodel)

typle2logitmodel <- glm(isPurchase ~ pricePerUnit, family=binomial(link=logit), data=typle2consumer)
summary(typle2logitmodel)
#######################Q3
##two segment 
q6DB.byconsumer=aggregate(q6DB[,"units"],list(consumer=q6DB$consumerID), FUN=sum)
summary(q6DB.byconsumer)
q6DB$fifty=ifelse(q6DB.byconsumer$x>0,1,0)
typle1consumer=subset(q6DB,q1DB$fifty==0)
typle2consumer=subset(q6DB,q1DB$fifty==1)

