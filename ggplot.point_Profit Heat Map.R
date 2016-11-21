library(ggplot2)
#Loading Data#
cc= `Coffee.Chain.data(1)`
#save the result of regression in another dataframe Profitsum#
Profitsum=aggregate(Profit~Product+Market,cc,sum)
#add a colume in Profitsum dataframe to show it is profit or loss#
Profitsum$porl <- ifelse(Profitsum$Profit>0, "Profit", "Loss")
###Draw Profit Heat map by using geom_point
Profit_Heat_Map<-ggplot(Profitsum,aes(Product,Market,color=Profitsum$porl,size=Profitsum$Profit))+#choose x variable and y variable#
  geom_point(pch=15)+ #set the profit of transactions,by using different colors, and change the cycle into square#
  scale_size(range=c(2,20))+#choose the suitable size range of the square#
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ #change X's text into vertical#
  xlab("Product") + ylab("Market") +ggtitle("Profit Heat Map")+#add some labeling#
  guides(fill=FALSE,size=FALSE)+#remove useless mark#
  scale_color_manual(values = c("red3","green4")) #pick right colours#
##Show the Map
Profit_Heat_Map
##Save database
save.image(file = "Profit of Coffee Chain.RData")