#open csv file
HHdata <- read.csv("D:/SynologyDrive/22-2/mobility_data/Household_travel_survey/Travel_diary_data.csv")

#create "income group"
HHdata$IncomeG <- NA
HHdata <- within(HHdata, {
  IncomeG[HHinc<=2] <- 1 #low
  IncomeG[HHinc>=3 & HHinc<=4] <- 2 #middle
  IncomeG[HHinc>=5 & HHinc<=6] <- 3 #high
})

#create "trip purpose group"
HHdata$Trip_purpG <- NA
HHdata <- within(HHdata, {
  Trip_purpG[Trip_purp==4] <- 1 #work
  Trip_purpG[Trip_purp==5|Trip_purp==6] <- 2 #education
  Trip_purpG[Trip_purp==2|Trip_purp==7] <- 3 #business
  Trip_purpG[Trip_purp==8|Trip_purp==9|Trip_purp==10|Trip_purp==11] <- 4 #shopping/social/leisure
  Trip_purpG[Trip_purp==3] <- 5 #back home
  Trip_purpG[Trip_purp==1|Trip_purp==12] <- 6 #other
})

#create "trip mode group"
HHdata$TripL_modeG <- NA
HHdata <- within(HHdata, {
  TripL_modeG[TripL_mode==2|TripL_mode==3] <- 1 #car
  TripL_modeG[TripL_mode>=4 & TripL_mode<=9] <- 2 #bus
  TripL_modeG[TripL_mode>=10 & TripL_mode<=13] <- 3 #rail
  TripL_modeG[TripL_mode==14] <- 4 #taxi
  TripL_modeG[TripL_mode==17|TripL_mode==18] <- 5 #bike
  TripL_modeG[TripL_mode==1] <- 6 #walk
  TripL_modeG[TripL_mode==15|TripL_mode==16|TripL_mode==19|TripL_mode==20|TripL_mode==21] <- 7 #other
})

#create "travel time of a trip leg"
HHdata$TripL_time <- (((HHdata$TripL_a_P-1)*12+HHdata$TripL_a_hh)*60+HHdata$TripL_a_mm) - (((HHdata$TripL_d_P-1)*12+HHdata$TripL_d_hh)*60+HHdata$TripL_d_mm)

#create "travel time of a trip"
HHdata$Trip_time <- (((HHdata$Trip_a_P-1)*12+HHdata$Trip_a_hh)*60+HHdata$Trip_a_mm) - (((HHdata$Trip_d_P-1)*12+HHdata$Trip_d_hh)*60+HHdata$Trip_d_mm)

#Compute travel mode share by income group
Mode_share_incomeG <-table(HHdata$TripL_modeG,HHdata$IncomeG)
Mode_shareP_incomeG=prop.table(Mode_share_incomeG,2)*100
round(Mode_shareP_incomeG,2) #round to two decimal places

#draw bar plot 
barplot(Mode_shareP_incomeG,main="Travel mode share", 
        xlab="Income group", ylab="Percentage", 
        names.arg=c("low (~200M/month)","middle (200M~500M/month)","high (500M~/month)"),
        legend=c("Car","Bus","Rail","Taxi","Bike","Walk","Others"), beside=TRUE)
#Q1 ends here

#Q2 starts here
#Create a data of linked trips
Tripdata <- unique(HHdata[,c(1:32,46:47,50)]) #46:IncomeG/47:Trip_purpG/50:Trip_time

#Distribution of trip purpose
Purpose_share <-table(Tripdata$Trip_purpG)
prop.table(Purpose_share)*100

#excluding “back home” trip
Purpose_share_nobackH <- table(Tripdata$Trip_purpG[Tripdata$Trip_purpG!=5])
prop.table(Purpose_share_nobackH)*100

#Compute travel purpose by income group (important)
Purpose_incomeG <-table(HHdata$Trip_purpG,HHdata$IncomeG)
PurposeP_incomeG=prop.table(Purpose_incomeG,2)*100
round(PurposeP_incomeG,2) #round to two decimal places

#Compute the average travel time by trip purpose and draw a bar plot
Tirp_time_by_purp=aggregate(Tripdata$Trip_time, by=list(Purpose=Tripdata$Trip_purpG),mean)

barplot(PurposeP_incomeG,main="Distribution of trip purpose by income groups", 
        xlab="Income group", ylab="percentage", 
        names.arg=c("low","middle","high"),
legend=c("work","education","business","shopping/social/leisure","returning home", "others"),beside=TRUE)

barplot(PurposeP_incomeG,main="Distribution of trip purpose by income groups", 
        xlab="Income group", ylab="percentage", 
        names.arg=c("low","middle","high"),
        beside=TRUE)

#kernel density distribution
install.packages("sm")
library(sm)

sm.density.compare(HHdata$TripL_time, HHdata$IncomeG, xlab="Travel time",xlim=c(-10,100))

title(main="Travel time distribution by income groups")

TripL_incomeG.f <- factor(HHdata$IncomeG, levels=c(1,2,3), labels=c("low","middle","high"))
colfill <-c(2:(1+length(levels(TripL_incomeG.f))))
legend(80,0.077, levels(TripL_incomeG.f), fill=colfill)

