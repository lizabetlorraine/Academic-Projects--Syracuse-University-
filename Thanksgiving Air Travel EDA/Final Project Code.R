#Elizabeth Jones
#Final Project: Resubmission

#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("tidyverse")

library(dplyr)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(tidyverse)
library(maps)
library(scales)
library(treemapify)
library(ggthemes)

#COLORS

my.cols2 <- c("#3E9DD7", "#5F2F9F","#07074F", "#2F2F9F", "#07574A"  )

my.cols <- c("#9D9DD7", "#5555AF", "#2F2F9F","#171779", "#07074F", "#07574A" )

liz.col5<-c("#44bfef", 
            "#ecf8fd",
            "#1b4C5f", 
            "#7bc477", 
            "#69cbf2",
            "#56c5f0",
            "#c6ebfa",
            "#287284",
            "#3E9DD7",
            "#3698bf",
            "#8ed8f5",
            "#a1dff7"
)

liz.col<-c("#44bfef", "#7bc477", "#ee8f71", "#FCCB06", "#222E50","#BFC0C0" )

liz.col2<-c("#44bfef", "#7bc477")

#--------IMPORT DATA AND FILTER OUT DATES AND LOCATIONS OUTSIDE 50 STATES

#link to download data: https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FGJ&QO_fu146_anzr=b0-gvzr

data.dir <-"C:\\Users\\eljon\\OneDrive - Syracuse University\\IST 719\\data\\"
Flights<-read.csv(paste0(data.dir, "T_ONTIME_REPORTING.csv"))

Flights<-filter(Flights,Flights$DEST_STATE_FIPS < 72 )
Flights<-filter(Flights,Flights$ORIGIN_STATE_FIPS < 72 )

Flights$FL_DATE<-format(as.POSIXct(Flights$FL_DATE,
                                   format = '%m/%d/%Y %H:%M'),
                        format = '%m/%d')

Flights <- Flights[Flights$FL_DATE > "11/17" &    
                     Flights$FL_DATE < "11/29", ]
View(Flights)

#--------ADD REGION INFO TO DATASET

state.fips
state.name

new.map<-data.frame(state.name)
new.map$regions <- state.region
new.map$state.name<-tolower(state.name)
new.map

state.r<-state.fips
state.r[nrow(state.r) + 1,] <- list("", "", 4, "", "AK", "alaska") #Add row for missing Alaska and Hawaii info
state.r[nrow(state.r) + 1,] <- list("", "", 4, "", "HI", "hawaii")
state.r$state.name<-state.r$polyname
state.r$state.name<-sub(":.*", "", state.r$state.name)
state.r
class(state.r$state.name)

merge.map<-left_join(new.map, state.r, by = "state.name")
merge.map$regions<-as.character(merge.map$regions)

merge.map<-merge.map[!duplicated(merge.map$state.name),]
merge.map<-merge.map[,-c(3,4,6,8)]
colnames(merge.map)<-c("region", "subregion", "division", "abb")

stateNregions<-merge.map
colnames(stateNregions)<-c("state.name", "ORIGIN_REGION", "REGION_CODE", "ORIGIN_STATE_ABR" )
US.FlightsNRegions<-left_join(Flights,stateNregions, by = "ORIGIN_STATE_ABR")

stateNregions<-merge.map
colnames(stateNregions)<-c("state.name", "DEST_REGION", "REGION_CODE", "DEST_STATE_ABR" )
US.FlightsNRegions<-left_join(US.FlightsNRegions,stateNregions, by = "DEST_STATE_ABR")

View(US.FlightsNRegions)

#--------CREATE PLOT TO DISCOVER BEST DAY TO TRAVEL (BY LEAST CANCELLATIONS AND DELAYS AS A PERCENTAGE)

bestDay<-cbind.data.frame(US.FlightsNRegions$FL_DATE, 
                          US.FlightsNRegions$FLIGHTS,
                          US.FlightsNRegions$CANCELLED, 
                          US.FlightsNRegions$ARR_DEL15)

colnames(bestDay)<-c("Date", "Flights", "Cancelled", "Delayed")
bestDay$Flights<-as.integer(bestDay$Flights)
bestDay$Cancelled<-as.integer(bestDay$Cancelled)
bestDay$Delayed<-as.integer(bestDay$Delayed)

bestDay$CancelNDelays <- 0
bestDay$CancelNDelays[bestDay$Cancelled==1]<- 1
bestDay$CancelNDelays[bestDay$Delayed==1]<- 1

bestDay<-bestDay%>%group_by(Date)%>%summarise(TotalFlights=sum(Flights),
                                              TotalCancels=sum(Cancelled),
                                              TotalCancelNDelays=sum(CancelNDelays))
bestDay<-data.frame(bestDay)
bestDay$PercentDisrupted<-(bestDay$TotalCancelNDelays/bestDay$TotalFlights)
bestDay$PercentonTime<-(bestDay$TotalFlights-bestDay$TotalCancelNDelays)/bestDay$TotalFlights
bestDay$PercentonTime<-percent(bestDay$PercentonTime, accuracy =1)


#Plot

p1<-ggplot(bestDay) + aes (x = Date, y = PercentonTime, fill = TotalFlights) + 
  geom_col(position="dodge")+
  geom_text(aes(label = PercentonTime), 
            vjust = 1, size = 5,color = "white")+
  theme_classic() +
  scale_fill_distiller(trans = "reverse", limits = c(20000,0))
p1


#--------CREATE TREE PLOT SHOWING POPULAR FLIGHT DESTINATIONS (BY REGION)

#Create new Dataframe
topLocal<-cbind.data.frame(US.FlightsNRegions$DEST_STATE_NM, 
                           US.FlightsNRegions$DEST_REGION,
                           US.FlightsNRegions$ORIGIN_REGION,
                           US.FlightsNRegions$ORIGIN_STATE_NM,
                           US.FlightsNRegions$FLIGHTS,
                           US.FlightsNRegions$AIR_TIME, 
                           US.FlightsNRegions$DEP_DELAY,
                           US.FlightsNRegions$DEP_TIME)

colnames(topLocal)<-c("Dest.State", 
                      "Dest.Region",
                      "Origin.Region",
                      "Origin.State",
                      "Flights", 
                      "Flight.Time", 
                      "Delay", 
                      "Depart.Time")

topLocal$Flights<-as.integer(topLocal$Flights)

#Add additional columns/Discretize

topLocal<-topLocal%>% mutate(Flight.Length = case_when(Flight.Time <= 180 ~ "short-haul",
                                                       Flight.Time > 180 & Flight.Time <= 360 ~ "medium-haul",
                                                       Flight.Time > 360 ~ "long-haul"))

topLocal<-topLocal%>% mutate(DelayExist = case_when(Delay <= 14 ~ "No",
                                                    Delay > 15 ~ "Yes"))

topLocal<-topLocal%>% mutate(Time.Type = case_when(Depart.Time < 1200 ~ "Morning",
                                                   Depart.Time >= 1200 & Delay < 1800 ~ "Midday",
                                                   Depart.Time >= 1800 ~ "Night"))

#aggregate by regions and states
topLocalagg<-aggregate(topLocal$Flights, 
                       list(topLocal$Dest.State,
                            topLocal$Origin.Region,
                            topLocal$DelayExist,
                            topLocal$Flight.Length,
                            topLocal$Time.Type), sum)

colnames(topLocalagg)<-c("Destination",
                         "Origin",
                         "Delay", 
                         "Flight.Length",
                         "TimeofDay",
                         "Flights")

#subset highest values

top10local <- topLocalagg %>%                                      # Top N highest values by group
  arrange(desc(Flights)) %>% 
  group_by(Origin) %>%
  slice(1:10)
top10local  



p.tree<-ggplot(top10local) + aes(area = Flights, fill = Destination, subgroup = Origin) + 
  geom_treemap()+
  geom_treemap_subgroup_text(color = "white")+
  geom_treemap_text(aes(label = Destination), color = "black")+
  theme_classic() +
  scale_fill_manual(values = liz.col5) +
  theme(legend.position = "none")
p.tree


#--------CREATE PLOT SHOWING AIRLINE WITH THE MOST ONTIME FLIGHTS BY REGION

onTime2<-cbind.data.frame(US.FlightsNRegions$FLIGHTS, 
                          US.FlightsNRegions$OP_CARRIER_AIRLINE_ID,
                          US.FlightsNRegions$DEP_DELAY_GROUP,
                          US.FlightsNRegions$DEP_TIME,
                          US.FlightsNRegions$DEST_REGION,
                          US.FlightsNRegions$DEST_STATE_NM,
                          US.FlightsNRegions$ORIGIN_REGION,
                          US.FlightsNRegions$ORIGIN_STATE_NM
)

colnames(onTime2)<-c("Flights",
                     "Airline",
                     "Delay", 
                     "Depart.Time",
                     "Destination",
                     "Dest.State",
                     "Origin",
                     "Origin.State")

sum(is.na(onTime2$Delay))

onTime2$Flights<-as.integer(onTime2$Flights)
onTime2$Depart.Time<-as.integer(onTime2$Depart.Time)
onTime2$Delay<-as.integer(onTime2$Delay)
class(onTime2$Airline)

onTime2<-onTime2%>%drop_na(Delay)

View(onTime2)

#Add Columns
onTime2<-onTime2%>% mutate(YNonTime = case_when(Delay <= 0 ~ 0,
                                                Delay > 0 ~ 1))

onTime2<-onTime2%>% mutate(Time.Type = case_when(Depart.Time < 1200 ~ "Morning",
                                                 Depart.Time >= 1200 & Delay < 1800 ~ "Midday",
                                                 Depart.Time >= 1800 ~ "Night"))

onTime2<-onTime2%>% mutate(Airline.Name = case_when(Airline == 20363 ~ "Endeavor Air",
                                                    Airline == 20397 ~ "PSA Airlines",
                                                    Airline == 19790 ~ "Delta Airlines",
                                                    Airline == 20398 ~ "Envoy Air",
                                                    Airline == 20378 ~ "Mesa Airlines", 
                                                    Airline == 19977 ~ "United Air Lines"))

table(onTime2$Airline.Name)

class(onTime2$YNonTime)
onTime2$YNonTime<-as.integer(onTime2$YNonTime)
sum(is.na(onTime2$YNonTime))

#Ontime flights per day by airline percentage
onTime5<-onTime2%>%group_by(Origin, Airline.Name)%>%summarise(TotalFlight=sum(Flights),
                                                              TotalDelays=sum(YNonTime))
onTime5$Percent<-(onTime5$TotalFlight-onTime5$TotalDelays)/onTime5$TotalFlight
onTime5$onTimePercent<-percent(onTime5$Percent, accuracy =1)


top3air <- onTime5 %>%                                      # Top N highest values by group
  arrange(desc(onTimePercent)) %>% 
  group_by(Origin) %>%
  slice(1:3)
top3air


p4<-ggplot(top3air) + aes (x = Origin, y = onTimePercent, fill = Airline.Name) + 
  geom_col(position="dodge")+scale_fill_manual(values =liz.col)+theme_classic()
p4

#--------CREATE PLOT FOR NUMBER OF ISSUES CAUSNG DELAYS BY TIME OF DAY

#Create new Dataframe
Delays<-cbind.data.frame( 
  US.FlightsNRegions$FLIGHTS, 
  US.FlightsNRegions$AIR_TIME, 
  US.FlightsNRegions$DEP_DELAY,
  US.FlightsNRegions$DEP_TIME,
  US.FlightsNRegions$CARRIER_DELAY,
  US.FlightsNRegions$WEATHER_DELAY,
  US.FlightsNRegions$NAS_DELAY,
  US.FlightsNRegions$SECURITY_DELAY,
  US.FlightsNRegions$LATE_AIRCRAFT_DELAY)

colnames(Delays)<-c("Flights", 
                    "Flight.Time",
                    "Depart.Delay",
                    "Depart.Time",
                    "Carrier.Delay",
                    "Weather.Delay", 
                    "AirSystem.Delay",
                    "Security.Delay", 
                    "LateAircraft.Delay" 
)

Delays$Flights<-as.integer(Delays$Flights)
#Delays$Flights<-as.integer(Delays$Flights)
#Delays$Flights<-as.integer(Delays$Flights)
#Delays$Flights<-as.integer(Delays$Flights)
#Delays$Flights<-as.integer(Delays$Flights)
#Delays$Flights<-as.integer(Delays$Flights)

class(Delays$Carrier.Delay)

#Add additional columns/Discrentize

Delays<-Delays%>%mutate(Delay.Length = case_when(Depart.Delay <= 14 ~ "No Delay",
                                                  Depart.Delay > 14 & Depart.Delay <= 45 ~ "Small Delay",
                                                  Depart.Delay > 45 ~ "Large Delay"))
Delays<-Delays%>% mutate(TimeODay = case_when(Depart.Time < 1200 ~ "Morning",
                                              Depart.Time >= 1200 & Depart.Time < 1800 ~ "Midday",
                                              Depart.Time >= 1800 ~ "Night"))
#Remove NAs
Delays<-Delays%>%drop_na(Carrier.Delay)


Delays<-Delays%>% mutate(Carrier.Count = case_when(Carrier.Delay == 0 ~ 0,
                                                   Carrier.Delay > 0 ~ 1))
Delays<-Delays%>% mutate(Weather.Count = case_when(Weather.Delay == 0 ~ 0,
                                                   Weather.Delay > 0 ~ 1))
Delays<-Delays%>% mutate(AirSystem.Count = case_when(AirSystem.Delay == 0 ~ 0,
                                                     AirSystem.Delay > 0 ~ 1))
Delays<-Delays%>% mutate(Security.Count = case_when(Security.Delay == 0 ~ 0,
                                                    Security.Delay > 0 ~ 1))
Delays<-Delays%>% mutate(LateAircraft.Count = case_when(LateAircraft.Delay == 0 ~ 0,
                                                        LateAircraft.Delay > 0 ~ 1))

DelaysAgg<-Delays%>%group_by(TimeODay,Delay.Length)%>%summarise(TotalFlight=sum(Flights),
                                                                Carrier = sum(Carrier.Count),
                                                                Weather = sum(Weather.Count),
                                                                AirSystem = sum(AirSystem.Count),
                                                                Security = sum(Security.Count),
                                                                LateAircraft = sum(LateAircraft.Count))
elaysAgg$Carrier<-as.integer(DelaysAgg$Carrier)
DelaysAgg$Weather<-as.integer(DelaysAgg$Weather)
DelaysAgg$AirSystem<-as.integer(DelaysAgg$AirSystem)
DelaysAgg$Security<-as.integer(DelaysAgg$Security)
DelaysAgg$LateAircraft<-as.integer(DelaysAgg$LateAircraft)


DelaysAgg$IssueCount<-DelaysAgg$Carrier + 
  DelaysAgg$Weather +
  DelaysAgg$AirSystem +
  DelaysAgg$Security +
  DelaysAgg$LateAircraft


DelaySubset = subset(DelaysAgg[DelaysAgg$Delay.Length !="No Delay",], select = c(TimeODay, 
                                                                             LateAircraft,
                                                                             IssueCount,
                                                                             Delay.Length))

DelaySubset$TimeODay<-as.factor(DelaySubset$TimeODay)
levels(DelaySubset$TimeODay)
DelaySubset$TimeODay<-relevel(DelaySubset$TimeODay, "Morning")

delay.p<-ggplot(DelaySubset) + aes (x = TimeODay, y = IssueCount, fill = Delay.Length) + 
  geom_col(position="dodge")+scale_fill_manual(values =liz.col2)+
  theme_classic()
delay.p

#--------CREATE MAP SHOWING MOST POPULAR DESTINATION 


Map<-cbind.data.frame(US.FlightsNRegions$DEST_STATE_NM, 
                  US.FlightsNRegions$DEST_REGION,
                  US.FlightsNRegions$FLIGHTS)

colnames(Map)<-c("region",
                  "division",
                  "flights")

Map$flights<-as.integer(arrivalMap$flights)

#Aggregate

Agg<-aggregate(Map$flights, list(Map$region, Map$division), sum)
colnames(Agg)<-c("region", "division", "flights")
Agg$region<-tolower(Agg$region)

#Merge
map.d<-map_data("state")
Dest<-left_join(map.d, Agg, by = "region")

#Create Map
ggplot(Dest)+
  aes(long, lat, group = group) + #group has to do with the states
  geom_polygon(aes(fill = flights), color = "white") +
  scale_fill_gradientn(colors=brewer.pal(3, "GnBu"),
                       na.value="azure2")+
  #Repositions the victims bar, gets rid of grid background and numbers
  xlab("") + ylab("") +ggtitle("Popular Destinations")+
  theme(legend.position = "right",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank())+
  coord_map("albers", at0 = 45.5, lat1 = 29.5)








