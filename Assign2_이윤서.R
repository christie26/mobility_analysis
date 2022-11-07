# open data 
Cdata <- read.csv("/Users/user/projects/mobility_data/Transit_card.csv")

# Q1 starts here!
# the number of trips
mobilityPattern <-table(Cdata$User,Cdata$Mode)
mobilityPattern
table(Cdata$User)

# percentage of trips / again
patternP <- prop.table(mobilityPattern,1)*100
round(patternP,2)


# Compare hourly distributions among different user groups
Time_dist_user=table(Cdata$On_hour,Cdata$User)
Time_dist_user.p=prop.table(Time_dist_user,2)*100

Timeofday=c(0:23)
plot(Timeofday,Time_dist_user.p[,1],type="o",xlab="Time of day",ylab="%",ylim = c(0,max(Time_dist_user.p)))
lines(Timeofday,Time_dist_user.p[,2],type="b",col=2)
lines(Timeofday,Time_dist_user.p[,3],type="b",col=3)
lines(Timeofday,Time_dist_user.p[,4],type="b",col=4)
lines(Timeofday,Time_dist_user.p[,5],type="b",col=5)
legend('topright',c("disabled","elderly","kids","normal","teen"),lwd=c(1,1),col=c("black",2,3,4,5))
# Q1 ends here

# Q2 starts here
# get station data
Station <- read.csv("/Users/user/projects/mobility_data/station_code_data.csv")
Station <- read.csv("/Users/user/projects/mobility_data/station_code_data_uni.csv",encoding="UTF-8")

names(Station) <- c("station_code","name","long","lat")

# extract data with "subset"
#Mdata_on <- subset(Cdata,On_hour==7|On_hour==8|On_hour==9)
#Mdata_off <- subset(Cdata,Off_hour==7|Off_hour==8|Off_hour==9)

Mdata_on <- subset(Cdata,On_hour==7|On_hour==8)
Mdata_off <- subset(Cdata,Off_hour==7|Off_hour==8)

#Ndata_on <- subset(Cdata,On_hour==18|On_hour==19)
#Ndata_off <- subset(Cdata,Off_hour==18|Off_hour==19)

Station.on <- data.frame(table(Mdata_on$On_station))
Station.off <- data.frame(table(Mdata_off$Off_station))

#Station.on <- data.frame(table(Ndata_on$On_station))
#Station.off <- data.frame(table(Ndata_off$Off_station))

names(Station.on) <- c("station_code","On_freq")
names(Station.off) <- c("station_code","Off_freq")

Station <- merge(x=Station,y=Station.on,by='station_code',all.x=TRUE)
Station <- merge(x=Station,y=Station.off,by='station_code',all.x=TRUE)

Station <- na.omit(Station)

# On_freq, Off_freq 를 기준으로 정렬
Station_order.on <- Station[order(-Station$On_freq),]
Station_order.off <- Station[order(-Station$Off_freq),]

Station_order.on[1:10,]
Station_order.off[1:10,]
