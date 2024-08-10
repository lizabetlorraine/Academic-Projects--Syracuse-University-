######Association Model######

library(tidyverse)
library(readr)
library(arules)
library(arulesViz)



###Code to Import Data
url<-"https://intro-datascience.s3.us-east-2.amazonaws.com/Resort01.csv"
finalProject<-read_csv(url)
finalProject


###Add new column named "RoomChange" which indicates whether a customer's room 
###is different from the one they originally reserved
finalProject$RoomChange<-(finalProject$ReservedRoomType==finalProject$AssignedRoomType)



###-----Converting the dataframe into a transaction dataset-----###

##Step 1: convert columns in to factors
str(finalProject)

#Fixing "Adults" column
finalProject.new<-finalProject%>%mutate(Adults=case_when(Adults==1 ~ "single", Adults==2 ~ "couple", Adults>2 ~ "group"))


#Changing the rest of the columns to factors
finalProject2<- data.frame(IsCanceled=as.factor(finalProject.new$IsCanceled),
                           #BookedinAdvanceLow=as.factor(finalProject.new$LeadTime<75),
                           WeekendStays=as.factor(finalProject.new$StaysInWeekendNights<1),
                           LongStayWD=as.factor(finalProject.new$StaysInWeekNights>median(finalProject.new$StaysInWeekNights)),
                           Adults=as.factor(finalProject.new$Adults),
                           Children=as.factor(finalProject.new$Children>0),
                           Babies=as.factor(finalProject.new$Babies>0),
                           BookingChanges=as.factor(finalProject.new$BookingChanges>0),
                           TotalOfSpecialRequests=as.factor(finalProject.new$TotalOfSpecialRequests>0),
                           Meal=as.factor(finalProject.new$Meal),
                           ReservedRoomType=as.factor(finalProject.new$ReservedRoomType),
                           CustomerType=as.factor(finalProject.new$CustomerType),
                           RoomChange=as.factor(finalProject.new$RoomChange),
                           AssignedRoomType=as.factor(finalProject.new$AssignedRoomType))

summary(finalProject2) 

##Step 2: Turn dataframe into a transactions dataset
finalProject.mx<-as(finalProject2,"transactions")
inspect(finalProject.mx[1:10])


#---Review ItemFrequency 
finalProjectFreq<- itemFrequency(finalProject.mx)
finalProjectFreq<-sort(finalProjectFreq)
finalProjectFreq


#Plotting the frequency
itemFrequencyPlot(finalProject.mx, cex.names=0.40)
summary(finalProject.mx)
#Top Frequencies 
#-No Babies appear in 99% of cases/bookings
#-No Children appear in 91% of cases/bookings
#-Room Change appear in 81% of cases/bookings
#-No-Booking Changes appear in 81% of cases/bookings
#-Adults-Couple appear in 78% of cases/bookings

#Frequency of items with a suport value of at least 70% 
itemFrequencyPlot(finalProject.mx,support=0.7)



#Getting list of rules for IsCanceled=0  
finalProject.rules<-apriori(finalProject.mx,
                            parameter = list(support = 0.05,
                                             confidence = 0.95 ),
                            control = list(verbose=F),
                            appearance = list(default="lhs",rhs = ("IsCanceled=0")))
summary(finalProject.rules)


#Narrow down rule county.Only select rules with a high lift value
#lift value of 1 or greater will ensure that this is a more interesting rule
plot(finalProject.rules)
finalProject.highlift<-finalProject.rules[quality(finalProject.rules)$lift>1.340]
summary(finalProject.highlift)
inspect(finalProject.highlift)

#Top 13 Rules  plotted
plot(finalProject.highlift)




#Getting list of rules for IsCanceled=1 
finalProject.canceledrules<-apriori(finalProject.mx,
                                    parameter = list(support = 0.005,
                                                     confidence = 0.90),
                                    control = list(verbose=F),
                                    appearance = list(default="lhs",rhs = ("IsCanceled=1")))

summary(finalProject.canceledrules)


#Selecting rules with high lift values
plot(finalProject.canceledrules)
finalProject.canceledhighlift<-finalProject.canceledrules[quality(finalProject.canceledrules)$lift>3.45]
summary(finalProject.canceledhighlift)
inspect(finalProject.canceledhighlift)

#Top 14 Rules plotted
plot(finalProject.canceledhighlift)








