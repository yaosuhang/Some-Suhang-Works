#################################PART ONE###########################################
#######packages needed############
install.packages("rpart")
install.packages("earth")
install.packages("ipred")
install.packages("randomForest")
install.packages("e1071")
install.packages("gbm")
install.packages("nnet")
install.packages("FNN")
install.packages('partykit')
###############loading data###################
testingData=read.csv('testingdata.csv',header = T)
trainingData=read.csv('trainingdata.csv',header = T)
###############Data preparation##############
#string handling, change review.test into review length
trainingData$reviewLength=nchar(paste(trainingData$review.text))
testingData$reviewLength=nchar(paste(testingData$review.text))
#Clear cases where number.of.votes = 0 
DB = subset(trainingData,trainingData$number.of.votes >0)
DB$percentHelpful = DB$number.of.helpfulness/DB$number.of.votes
#divided data within sample
set.seed(123)
train = sample(1:nrow(DB), 0.8*nrow(DB))
reviewDB = DB[train,]
TestDB = DB[-train,]
Testactual=TestDB$percentHelpful
Trainactual=reviewDB$percentHelpful
#######a) CART treesusing,Store the predictions as 'cartVec'.
library(rpart)
library('partykit')
cartFIT = rpart(percentHelpful~reviewLength+star.rating,data=reviewDB, method="anova")
print(cartFIT)
rPartModParty = as.party(cartFIT)
plot(rPartModParty)
cartVecIn=predict(cartFIT,reviewDB,method="anova")
cartVecOut=predict(cartFIT,TestDB,method="anova")
mean((Testactual- cartVecOut )^2)
mean((Trainactual- cartVecIn )^2)
cartVec=predict(cartFIT,data=testingData,method="anova")
system.time(rpart(percentHelpful~reviewLength+star.rating,data=reviewDB, method="anova"))

#######b) MARS, using the "earth" package. Store the best as 'marsVec'.
library(earth)
MARFIT=earth(percentHelpful~reviewLength+star.rating,data=reviewDB)
summary(MARFIT)
marsVecIn=predict(MARFIT,reviewDB)
marsVecOut=predict(MARFIT,TestDB)
mean((Testactual- marsVecOut )^2)
mean((Trainactual- marsVecIn )^2)
marsVec=predict(MARFIT,data=testingData)
system.time(earth(percentHelpful~reviewLength+star.rating,data=reviewDB))
#######c) Bagged Trees, using the "ipred" package. Store the best as 'baggedVec'.
library(ipred)
baggedFIT=bagging(percentHelpful~reviewLength+star.rating,data=reviewDB)
baggedVecIn=predict(baggedFIT,reviewDB)
baggedVecOut=predict(baggedFIT,TestDB)
mean((Testactual- baggedVecOut )^2)
mean((Trainactual- baggedVecIn )^2)
baggedVec=predict(baggedFIT,data=testingData)
system.time(bagging(percentHelpful~reviewLength+star.rating,data=reviewDB))
#######d) Random Forests, using the "randomForest" package. Store the best as 'forestVec'.
library(randomForest)
randForestFIT <- randomForest(percentHelpful~reviewLength+star.rating,data=reviewDB) 
print(randForestFIT) # view results 
importance(randForestFit)
randForestVecIn=predict(randForestFIT,reviewDB)
randForestVecOut=predict(randForestFIT,TestDB)
mean((Testactual- randForestVecOut )^2)
mean((Trainactual- randForestVecIn )^2)
randForestVec=predict(randForestFIT, data=testingData)
system.time(randomForest(percentHelpful~reviewLength+star.rating,data=reviewDB))
#######e) Support Vector Machine, using the "e1071". Store the best as 'svmVec'.
library(e1071)
svmFIT = svm(percentHelpful~reviewLength+star.rating, data=reviewDB)
svmVecIn=predict(svmFIT,reviewDB)
svmVecOut=predict(svmFIT,TestDB)
mean((Testactual- svmVecOut )^2)
mean((Trainactual- svmVecIn )^2)
svmVec=predict(svmFIT, data=testingData)
system.time(svm(percentHelpful~reviewLength+star.rating, data=reviewDB))

#######f) Boosting, using Adaboost in the "gbm" package. Store the best as 'boostVec'.
library(gbm)
boostingFIT = gbm(formula = percentHelpful~reviewLength+star.rating,data=reviewDB, distribution = "gaussian")
plot(boostingModel)
summary(boostingModel)
boostingVecIn=predict(boostingFIT,reviewDB,distribution = "gaussian",n.trees = 100)
boostingVecOut=predict(boostingFIT,TestDB,distribution = "gaussian",n.trees = 100)
mean((Testactual- boostingVecOut )^2)
mean((Trainactual- boostingVecIn )^2)
svmVec=predict(svmFIT, data=testingData,distribution = "gaussian",n.trees = 100)
system.time(gbm(formula = percentHelpful~reviewLength+star.rating,data=reviewDB, distribution = "gaussian"))

#######g) Neural networks, using the "nnet" package. Store the best as 'nnetVec'.
library(nnet)
nnetFIT = nnet(formula = percentHelpful~reviewLength+star.rating,data=reviewDB, linout=1,size = 2)
nnetVecIn=predict(nnetFIT,reviewDB,linout=1,size = 2)
nnetVecOut=predict(nnetFIT,TestDB,linout=1,size = 2)
mean((Testactual- nnetVecOut )^2)
mean((Trainactual- nnetVecIn )^2)
nnetVec=predict(nnetFIT, data=testingData)
system.time(nnet(formula = percentHelpful~reviewLength+star.rating,data=reviewDB, linout=1,size = 2))

#######h) Knn-regression, using the "FNN" package. Store the best as 'knnVec'.
library(FNN)
knnFit = knn.reg(train = cbind(reviewDB$reviewLength,reviewDB$star.rating), y = reviewDB$percentHelpful,
                 test = cbind(TestDB$reviewLength,TestDB$star.rating),k=5)
mean((Testactual- knnFit$pred)^2)
knnFit$pred
system.time(knn.reg(train = cbind(reviewDB$reviewLength,reviewDB$star.rating), y = reviewDB$percentHelpful,
                    test = cbind(TestDB$reviewLength,TestDB$star.rating),k=5))