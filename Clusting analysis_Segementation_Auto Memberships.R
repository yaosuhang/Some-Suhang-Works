#Auto Membership Organization
#Marketing Projects
#####Main methology: Clusting analysis
#####Open automembship.Rdata

CustomerData <- Customer.Database...Auto.Membership_CEXAppend
GeneralPopData <- Random.Database.Sample...Auto.Membership_CEXAppend

str(CustomerData)
head(CustomerData)
summary(CustomerData$income)

#assess missingness first
##CustomerData$income['num.na'] <- sapply(CustomerData, function(x) sum(is.na(x)))

#gender
CustomerData$gender <- as.numeric(CustomerData$gender)
CustomerData$gender

#zip code
summary(CustomerData$zip_code) #no NAs, too specific

#number of children
summary(CustomerData$num_of_children) #no NA's

#lets use state to assess location
summary(CustomerData$state) #no NA's

#children 13-18, change NAs to 0
summary(CustomerData$child13_18) #17,838
CustomerData$child13_18[is.na(CustomerData$child13_18)]<-0

#income
##summary(CustomerData$income[!A:O]) #there are 39 that are not assigned A-O, replace with median income
summary(CustomerData$median_income)
#Replace blank 39 with the median income measures

CustomerData$income <- as.numeric(CustomerData$income)
CustomerData <- CustomerData[!(CustomerData$income=="1"),]

#age, remove the 0's since there are no NAs
summary(CustomerData$age==0) #494 zeroes
CustomerData$age[]

AgeAndIncome <- which(with(CustomerData, age==0, is.na(income)))
CustomerData <- CustomerData[-AgeAndIncome,]

#truck owner, change NA's to 0
summary(CustomerData$truck_owner) #19,928 NAs
CustomerData$truck_owner[is.na(CustomerData$truck_owner)]<-0

#motor cycle owner, change NA's to 0
summary(CustomerData$motor_cycle) #19,369 NAs
CustomerData$motor_cycle[is.na(CustomerData$motor_cycle)]<-0

#interest in travel, people with big families will be driving for vacation
#change NA's to 0
summary(CustomerData$travel) #1,928
CustomerData$travel[is.na(CustomerData$travel)]<-0

#interest in cars
#change NA's to 0
summary(CustomerData$cars) #14,845
CustomerData$cars[is.na(CustomerData$cars)]<-0

#Travel, change NA's to 0
summary(CustomerData$travel_business)#15,346 NA's Taking planes more often
CustomerData$travel_business[is.na(CustomerData$travel_business)]<-0

summary(CustomerData$travel_personal) #15,713 NA's Taking planes more often
CustomerData$travel_personal[is.na(CustomerData$travel_personal)]<-0

summary(CustomerData$travel_vacation) #3,414 NA's FOCUS ON THIS SEGMENT (least NA's)
CustomerData$travel_vacation[is.na(CustomerData$travel_vacation)]<-0

#Personal Insurance and Pensions
summary(CustomerData$AINSPENSN) #95 NA's
CustomerData$AINSPENSN[is.na(CustomerData$AINSPENSN)]<-0

#For Transportation
summary(CustomerData$ATRANS) #95 NA's
CustomerData$ATRANS[is.na(CustomerData$ATRANS)]<-0

#######Regressions and Prediction########

#eduaction code
summary(CustomerData$education_code) #5,860 NA's

EducationLM <- lm(education_code~income, data = CustomerData)
EducationLM #4 are negative

EducationPred <- predict(EducationLM, data=GeneralPopData)
summary(EducationPred)

#median income and occupation to predict education
OccupationLM <- lm(occupation~median_income, data = CustomerData)
summary(OccupationLM)

OccupationPred <- predict(OccupationLM, data = GeneralPopData)
summary(OccupationPred)
####################################################################

#Cluster Analysis
install.packages("cluster")
install.packages("fpc")
library(cluster) 
library(fpc)

#Clusting code
set.seed(123456)   # set random number seed before doing cluster analysis
toclust <- CustomerData[,c(8,9,15,20,32,33,47,50,52,64,66)]    # select the relevant data for clustering
wss <- (nrow(toclust)-1)*sum(apply(toclust,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(toclust, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
colMeans(toclust)

#Use pamk() to determine the optimal number of clusters
pm <- pamk(toclust,scaling=TRUE)
pm$nc #6

#Create tow new kmeans clusters
km <- kmeans(toclust,6,iter.max = 20, nstart=2)
km$centers

#Function turns min into 0, max into 1 and scales rest linearly
rscale <- function(x){(x-min(x))/(max(x)-min(x));}

km1m <- aggregate(toclust,by=list(km$cluster),FUN=mean) #calculate profile means
km1ms <- apply(km1m[,2:ncol(km1m)],2,rscale) #rescale profile means for easy presentation
par(mar=c(8.1,4.1,4.1,2.1)) #setting margins to give room for x axis labels
matplot(t(km1ms),col=c(1,4,2),ylab="Mean Value (Range Normalized)",xaxt="n")
axis(side=1,at=c(8,9,15,20,32,33,47,50,52,64,66),labels=names(toclust),las=2) 
#putting x axis labels on plot
##plots the three clusters with 1 max and 0 min for each variable

percsize <- paste(1:8," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
pie(km$size,labels=percsize)
##plots the relative size of each group

clusplot(toclust, km$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0) #plot clusters against principal components
##idea of this plot is identify the clusters 
##the axes are the principal components, which are sort of a mixture 
##  of all of the variables used in the clustering

# Centroid Plot against 1st 2 discriminant functions
plotcluster(toclust, km$cluster) #plot against discriminant functions ()
##idea of this plot is that the axes are the two best 
##  functions to distinguish the clusters

# calculate range for each segment in excel
# project segments to whole population to see how many people can fall into each segment
# clean general population data

# after choosing nearby states, import 'State' csv
#income
summary(GeneralPopData$income)

GeneralPopData$income <- as.numeric(GeneralPopData$income)
GeneralPopData <- GeneralPopData[!(GeneralPopData$income=="1"),]

#age, remove the 0's since there are no NAs
summary(GeneralPopData$age==0) 

AgeAndIncome <- which(with(GeneralPopData, age==0, is.na(income)))
GeneralPopData <- GeneralPopData[-AgeAndIncome,]

#Personal Insurance and Pensions
summary(GeneralPopData$AINSPENSN)
GeneralPopData$AINSPENSN[is.na(GeneralPopData$AINSPENSN)]<-0

#For Transportation
summary(GeneralPopData$ATRANS) 
GeneralPopData$ATRANS[is.na(GeneralPopData$ATRANS)]<-0

# 8325 rows left after cleaning

# segment 1
index1 <- (GeneralPopData$age > 55.74146 & GeneralPopData$age < 57.15788) 
index2 <- (GeneralPopData$income > 10.3983 & GeneralPopData$income < 11.6366495)
index3 <- (GeneralPopData$AINSPENSN > 6526.3255 & GeneralPopData$AINSPENSN < 7844.751)
index4 <- (GeneralPopData$ATRANS > 12513.04 & GeneralPopData$ATRANS < 13729.04)

index <- index1*index2
table(index)
# 20 people fall into segment 1

# segment 2
index1 <- (GeneralPopData$age > 58.090225 & GeneralPopData$age < 58.467335) 
index2 <- (GeneralPopData$income > 9.6571995 & GeneralPopData$income < 10.3983)
index3 <- (GeneralPopData$AINSPENSN < 2182.058)
index4 <- (GeneralPopData$ATRANS < 5074.285)

index <- index1*index2
table(index)
# No person falls into segment 2

# segment 3
index1 <- (GeneralPopData$age > 54.857805 & GeneralPopData$age < 55.74146) 
index2 <- (GeneralPopData$income > 11.6366495 & GeneralPopData$income < 12.8972945)
index3 <- (GeneralPopData$AINSPENSN > 7844.751 & GeneralPopData$AINSPENSN < 9177.354)
index4 <- (GeneralPopData$ATRANS > 13729.04 & GeneralPopData$ATRANS < 15085.915)

index <- index1*index2
table(index)
# 25 people fall into segment 3

# segment 4
index1 <- (GeneralPopData$age > 57.15788 & GeneralPopData$age < 58.090225) 
index2 <- (GeneralPopData$income > 8.5269345 & GeneralPopData$income < 9.6571995)
index3 <- (GeneralPopData$AINSPENSN > 5132.2235 & GeneralPopData$AINSPENSN < 6526.3255)
index4 <- (GeneralPopData$ATRANS > 11014.97 & GeneralPopData$ATRANS < 12513.04)

index <- index1*index2
table(index)
# 20 people fall into segment 4

# segment 5
index1 <- (GeneralPopData$age > 58.467335) 
index2 <- (GeneralPopData$income < 8.5269345)
index3 <- (GeneralPopData$AINSPENSN > 2182.058 & GeneralPopData$AINSPENSN < 5132.2235)
index4 <- (GeneralPopData$ATRANS > 5074.285 & GeneralPopData$ATRANS < 11014.97)

index <- index1*index2
table(index)
# 1671 people fall into segment 5

# segment 6
index1 <- (GeneralPopData$age < 54.857805) 
index2 <- (GeneralPopData$income > 12.8972945)
index3 <- (GeneralPopData$AINSPENSN > 9177.354)
index4 <- (GeneralPopData$ATRANS > 15085.915)

index <- index1*index2
table(index)
# 649 people fall into segment 6