#open csv file
HHdata <- read.csv("/Users/user/projects/mobility_data/Travel_diary_data.csv")

#sort only 4 distract (가좌동)
HHdata_S <- HHdata[HHdata$Dcode==1141072000|HHdata$Dcode==1141070000|HHdata$Dcode==1141071000|HHdata$Dcode==1141072000,]

#create "income group" and divide into each group 
HHdata_S$IncomeG <- NA
HHdata_S <- within(HHdata_S, {
  IncomeG[HHinc<=2] <- 1 #low
  IncomeG[HHinc>=3 & HHinc<=4] <- 2 #middle
  IncomeG[HHinc>=5 & HHinc<=6] <- 3 #high
})

#create "trip purpose group" and divide into each group
HHdata_S$Trip_purpG <- NA
HHdata_S <- within(HHdata_S, {
  Trip_purpG[Trip_purp==4] <- 1 #work
  Trip_purpG[Trip_purp==5|Trip_purp==6] <- 2 #education
  Trip_purpG[Trip_purp==2|Trip_purp==7] <- 3 #business
  Trip_purpG[Trip_purp==8|Trip_purp==9|Trip_purp==10|Trip_purp==11] <- 4 #shopping/social/leisure
  Trip_purpG[Trip_purp==3] <- 5 #back home
  Trip_purpG[Trip_purp==1|Trip_purp==12] <- 6 #other
})
# HHdata (you can use it when you erase #)
#HHdata$Trip_purpG <- NA
#HHdata <- within(HHdata, {
  Trip_purpG[Trip_purp==4] <- 1 #work
  Trip_purpG[Trip_purp==5|Trip_purp==6] <- 2 #education
  Trip_purpG[Trip_purp==2|Trip_purp==7] <- 3 #business
  Trip_purpG[Trip_purp==8|Trip_purp==9|Trip_purp==10|Trip_purp==11] <- 4 #shopping/social/leisure
  Trip_purpG[Trip_purp==3] <- 5 #back home
  Trip_purpG[Trip_purp==1|Trip_purp==12] <- 6 #other
})

#create "trip mode group"
HHdata_S$TripL_modeG <- NA
HHdata_S <- within(HHdata_S, {
  TripL_modeG[TripL_mode==2|TripL_mode==3] <- 1 #car
  TripL_modeG[TripL_mode>=4 & TripL_mode<=9] <- 2 #bus
  TripL_modeG[TripL_mode>=10 & TripL_mode<=13] <- 3 #rail
  TripL_modeG[TripL_mode==14] <- 4 #taxi
  TripL_modeG[TripL_mode==17|TripL_mode==18] <- 5 #bike
  TripL_modeG[TripL_mode==1] <- 6 #walk
  TripL_modeG[TripL_mode==15|TripL_mode==16|TripL_mode==19|TripL_mode==20|TripL_mode==21] <- 7 #other
})
# HHdata (you can use it when you erase #)
#HHdata$TripL_modeG <- NA
#HHdata <- within(HHdata, {
  TripL_modeG[TripL_mode==2|TripL_mode==3] <- 1 #car
  TripL_modeG[TripL_mode>=4 & TripL_mode<=9] <- 2 #bus
  TripL_modeG[TripL_mode>=10 & TripL_mode<=13] <- 3 #rail
  TripL_modeG[TripL_mode==14] <- 4 #taxi
  TripL_modeG[TripL_mode==17|TripL_mode==18] <- 5 #bike
  TripL_modeG[TripL_mode==1] <- 6 #walk
  TripL_modeG[TripL_mode==15|TripL_mode==16|TripL_mode==19|TripL_mode==20|TripL_mode==21] <- 7 #other
})

#create "travel time of a trip leg"
HHdata_S$TripL_time <- (((HHdata_S$TripL_a_P-1)*12+HHdata_S$TripL_a_hh)*60+HHdata_S$TripL_a_mm) - (((HHdata_S$TripL_d_P-1)*12+HHdata_S$TripL_d_hh)*60+HHdata_S$TripL_d_mm)

#create "travel time of a trip"
HHdata_S$Trip_time <- (((HHdata_S$Trip_a_P-1)*12+HHdata_S$Trip_a_hh)*60+HHdata_S$Trip_a_mm) - (((HHdata_S$Trip_d_P-1)*12+HHdata_S$Trip_d_hh)*60+HHdata_S$Trip_d_mm)

#1 Compute travel mode share by income group
Mode_share_incomeG <-table(HHdata_S$TripL_modeG,HHdata_S$IncomeG)
Mode_shareP_incomeG=prop.table(Mode_share_incomeG,2)*100
round(Mode_shareP_incomeG,2)
barplot(Mode_shareP_incomeG,main="Travel mode share", 
        xlab="Income group", ylab="Percentage", 
        names.arg=c("low (~200M/month)","middle (200M~500M/month)","high (500M~/month)"),
        legend=c("Car","Bus","Rail","Taxi","Bike","Walk","Others"), beside=TRUE)

#2 Comput travel mode share by trip purpose
Mode_share_purpG <- table(HHdata_S$TripL_modeG,HHdata_S$Trip_purpG)
Mode_shareP_purpG=prop.table(Mode_share_purpG,2)*100
round(Mode_shareP_purpG,2)
barplot(Mode_shareP_purpG,main="Travel mode share", 
        xlab="Purpose", ylab="Percentage", 
        names.arg=c("work","education","business","shopping/social/leisure","back home","other"),
        legend=c("Car","Bus","Rail","Taxi","Bike","Walk","Others"), beside=TRUE)
#HHdata
#Mode_share_purpG <- table(HHdata$TripL_modeG,HHdata$Trip_purpG)
#Mode_shareP_purpG=prop.table(Mode_share_purpG,2)*100
#round(Mode_shareP_purpG,2)
#barplot(Mode_shareP_purpG,main="Travel mode share", 
#        xlab="Purpose", ylab="Percentage", 
#        names.arg=c("work","education","business","shopping/social/leisure","back home","other"),
#        legend=c("Car","Bus","Rail","Taxi","Bike","Walk","Others"), beside=TRUE)


#Q2 starts here
#Create a data of linked trips
Tripdata <- unique(HHdata_S[,c(1:32,46:47,50)]) #46:Trip_purpG/47:IncomeG/50:Trip_time

#Distribution of trip purpose
Purpose_share <-table(Tripdata$Trip_purpG)
prop.table(Purpose_share)*100

#excluding “back home” trip
Purpose_share_nobackH <- table(Tripdata$Trip_purpG[Tripdata$Trip_purpG!=5])
prop.table(Purpose_share_nobackH)*100

#Compute travel purpose by income group (important)
Purpose_incomeG <-table(HHdata_S$Trip_purpG,HHdata_S$IncomeG)
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
library("sm")

sm.density.compare(HHdata_S$TripL_time, HHdata_S$IncomeG, xlab="Travel time",xlim=c(-10,100))

title(main="Travel time distribution by income groups")

TripL_incomeG.f <- factor(HHdata_S$IncomeG, levels=c(1,2,3), labels=c("low","middle","high"))
colfill <-c(2:(1+length(levels(TripL_incomeG.f))))
legend(80,0.077, levels(TripL_incomeG.f), fill=colfill)

