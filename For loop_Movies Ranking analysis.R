#######Case about using for loop####
#######################PART ONE Run a linear regression model#####################
lm1data=subset(homework3DB,homework3DB$subsequentEarnings>0)
lm1=lm(formula=log(lm1data$subsequentEarnings)~lm1data$releaseRank)
summary(lm1)
############################# PART 2 DATA CLEANING##################
#a) create an empty data frame
finalDB <- matrix(NA,nrow=length(unique(homework3DB$releaseDate)),ncol=3)
colnames(finalDB) <- c("Date","firstWeekBoxDiff","laterLogBoxDiff")
finalDB <- as.data.frame(finalDB)

#b) populate this data as follows
#populate date directly by getting unique weeks in dataset
finalDB$Date <- unique(homework3DB$releaseDate)

#populate firstWeekfinalDB
#populate laterLogfinalDB
for(i in finalDB$Date){
  subsetBoxOffice <- subset(homework3DB,homework3DB$releaseDate==i)
  finalDB[finalDB$Date == i, "firstWeekBoxDiff"] <- 
    log(subsetBoxOffice[subsetBoxOffice$releaseRank == 1, "releaseWeekEarnings"])-log(subsetBoxOffice[subsetBoxOffice$releaseRank == 2, "releaseWeekEarnings"])
  finalDB[finalDB$Date == i, "laterLogBoxDiff"] <-
    log(subsetBoxOffice[subsetBoxOffice$releaseRank == 1, "subsequentEarnings"])-log(subsetBoxOffice[subsetBoxOffice$releaseRank == 2, "subsequentEarnings"])
}      

finalDB[finalDB == -Inf|finalDB == Inf] <- NA
finalDB <- finalDB[complete.cases(finalDB), ]

#store the R workplace and submit it
save.image(file = "hw3_yaosuhang.Rdata")

#############PART3#############################
#b)Subset close box office (first Week Box Diff<.1)
closeDiff=subset(finalDB,finalDB$firstWeekBoxDiff<.1)
#d)T test
#Use close threshold to pickout rank1 and rank2 movies in original dataframe
subsetclosedata=unique(closeDiff$Date)
treatmentSubsequentEarnings=rep(0,length(subsetclosedata))
for(a in 1:length(subsetclosedata)){
treatmentSubsequentEarnings[a]=homework3DB$subsequentEarnings[homework3DB$releaseDate==closeDiff$Date[a]&homework3DB$releaseRank==1]
}
ControlSubsequentEarnings=rep(0,length(subsetclosedata))
for(b in 1:length(subsetclosedata)){
ControlSubsequentEarnings[b]=homework3DB$subsequentEarnings[homework3DB$releaseDate==closeDiff$Date[b]&homework3DB$releaseRank==2]
}
#Calculate T test between treatment group and control croup
t.test(treatmentSubsequentEarnings,ControlSubsequentEarnings,paired = T)
#doing new lm with close threshold
DB=subset(homework3DB,homework3DB$releaseRank==1|homework3DB$releaseRank==2)
TDB=DB[DB$releaseDate%in%subsetclosedata,]
lm2=lm(formula=log(TDB$subsequentEarnings)~TDB$releaseRank)
summary(lm2)

