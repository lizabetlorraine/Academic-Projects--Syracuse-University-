###______FINAL PROJECT____######


#install.packages("GGally")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("rworldmap")

library(rworldmap)
library(gridExtra)
library(ggplot2)
library(readr)
library(tidyverse)
library(GGally)
library(arules)
library(arulesViz)
library(rpart)
library(e1071)
library(tidyverse)
library(kernlab)
library(caret)
library(rpart)
library(rpart.plot)


#Code to Import Data

url<-"https://intro-datascience.s3.us-east-2.amazonaws.com/Resort01.csv"
finalProject<-read_csv(url)
finalProject

##--Code for sorting and organizing data and identifying nulls

finalProject<-arrange(finalProject, IsCanceled)
finalProject$RoomChange<-finalProject$AssignedRoomType==finalProject$ReservedRoomType
allCancels<-finalProject[28939:40060,]
noCancels<-finalProject[1:28938,]

CanceledDF <- subset(finalProject, IsCanceled == 1)
notcancelDf <- subset(finalProject, IsCanceled == 0)

findNull <- function(data) {
  
  nullCount <- table(data[data=='NULL'])
  return(nullCount)
}

findNull(finalProject$Country)

###----Reservation Totals for Adults, Deposit Type, Customer Type, Market Segment and Country

par(mfrow=c(2,2))
barplot(table(finalProject$Adults),xlab="Reservations By Num of Adults") #2Adults
barplot(table(finalProject$DepositType),xlab="Reservations By Deposit Type") #No Deposit
barplot(table(finalProject$CustomerType),xlab="Reservations By Customer Type") #Transient
barplot(table(finalProject$MarketSegment),xlab="Reservations By Market Segment") #Online Travel Agent

#For Country
df.wmap<-data.frame(table(finalProject$Country, finalProject$IsCanceled))
colnames(df.wmap)<-c('Country','IsCanceled','Reservations')
df.wmap<-df.wmap[!df.wmap$Country == "NULL", ]
divBY<-sum(as.numeric(table(finalProject$Country)))- sum(as.numeric(table(finalProject$Country)['NULL']))
df.wmap$reservationPercent<-as.numeric(df.wmap$Reservations/divBY)*100

#Barplot by Country
CountryReservations<-df.wmap%>%group_by(Country)%>%mutate(ResPercent=prop.table(Reservations)*100)
CountryReservations$Total<-CountryReservations$Reservations/CountryReservations$ResPercent*100
CountryReservations$TotalPercent<-CountryReservations$Total/divBY*100
CountryReservations<-arrange(CountryReservations, Country)
Top_CountryReservations <- subset(CountryReservations, TotalPercent > 1)

#Plot - PRT
Wplot<-ggplot(Top_CountryReservations, aes(x=Country, fill=IsCanceled))+
  geom_bar(aes(y=Reservations), position = 'dodge',stat = "identity") + ggtitle("Top 1 Percent of Reservations by Country")



#WorldMap
joinData<-joinCountryData2Map(df.wmap, joinCode = "ISO3", 
                              nameJoinColumn ='Country')

worldMap<-mapCountryData(joinData, 
                         nameColumnToPlot="reservationPercent", 
                         catMethod='categorical', mapTitle = "Percentage of Total Reservations",
                         addLegend='FALSE')

do.call(addMapLegend, c(worldMap, legendWidth=1,legendMar=2))



###---Histogram, Boxplot, and Histogram for Lead Time

mean(allCancels$LeadTime)
mean(noCancels$LeadTime)

hist1 <-ggplot(allCancels)
hist1 <- hist1 + aes(x=LeadTime)
hist1 <- hist1 + geom_histogram (binwidth = 50, color='black', fill='white')
hist1 <- hist1 + ggtitle("Lead Time for Cancelled Guests")
hist1

hist2 <-ggplot(noCancels)
hist2 <- hist2 + aes(x=LeadTime)
hist2 <- hist2 + geom_histogram (binwidth = 50, color='black', fill='white')
hist2 <- hist2 + ggtitle("Lead Time for Non-Cancelled Guests")
hist2

LeadTimeHistogram<-grid.arrange(hist1, hist2, ncol=2)

leadTimePlot <- boxplot(notcancelDf$LeadTime,CanceledDF$LeadTime, xlab= "LeadTime", names = c("not cancelled", "cancel"))
boxplot(finalProject$LeadTime,finalProject$IsCanceled)

####-----Lead time against Customer Type, Deposit Type, Market Segment Type, Country

finalProject$IsCanceled<-as.factor(finalProject$IsCanceled)

dfDepositType<-finalProject%>%group_by(DepositType, IsCanceled)%>%summarise(avgLead=mean(LeadTime))
dfCustomerType<-finalProject%>%group_by(CustomerType, IsCanceled)%>%summarise(avgLead=mean(LeadTime))
dfMarketType<-finalProject%>%group_by(MarketSegment, IsCanceled)%>%summarise(avgLead=mean(LeadTime))


Dplot<-ggplot(dfDepositType, aes(x=DepositType, fill=IsCanceled))+
  geom_bar(aes(y=avgLead),position="dodge", stat = "identity") + ggtitle("Deposit Type Grouped By Average Lead Time")

Cplot<-ggplot(dfCustomerType, aes(x=CustomerType, fill=IsCanceled))+
  geom_bar(aes(y=avgLead),postition="dodge',stat = "identity") + ggtitle("Customer Type Grouped By Average Lead Time")

Mplot<-ggplot(dfMarketType, aes(x=MarketSegment, fill=IsCanceled))+
  geom_bar(aes(y=avgLead),stat = "identity") + ggtitle("MarketSegment Type Grouped By Average Lead Time")

LeadTimeGroups<-grid.arrange(Dplot, Cplot, Mplot, ncol=2)

###----Tree Model------######


#Create DataSet
finalProject.tmodel3<-subset(finalProject, select=c("IsCanceled","LeadTime",
                                                    "CustomerType", "RoomChange", "Country", "MarketSegment"))
#Change to Factors
finalProject.tmodel3$CustomerType<-as.factor(finalProject.tmodel3$CustomerType)
finalProject.tmodel3$IsCanceled<-as.factor(finalProject.tmodel3$IsCanceled)
finalProject.tmodel3$RoomChange<-as.factor(finalProject.tmodel3$RoomChange)
finalProject.tmodel3$MarketSegment<-as.factor(finalProject.tmodel3$MarketSegment)

#Separate and Train
set.seed(111)
trainList3 <-
createDataPartition(y=finalProject.tmodel3$IsCanceled,p=.60,list=FALSE)

trainSet3 <-finalProject.tmodel3[trainList3,]
testSet3 <- finalProject.tmodel3[-trainList3,]


#Run the Model
t.model3 <- rpart(IsCanceled~.,data=finalProject.tmodel3)

#Plot the Model
rpart.plot(t.model3)

#Test the Model
tModelpred3<-predict(t.model3, newdata=testSet3, type='class')

#Model Accuracy
confusionMatrix(tModelpred3,testSet3$IsCanceled)

#Important Markers
varImp(t.model3)




