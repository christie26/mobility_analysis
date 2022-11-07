#card_data_with dong_yoon.R
# open data 
Cdata <- read.csv("/Users/user/projects/mobility_data/Transit_card.csv")

# get station data
Station <- read.csv("/Users/user/projects/mobility_data/station_code_with_dong_data.csv",encoding="UTF-8")
Station <- Station[,-3:-8]
Station <- Station[,-6:-8]
colnames(Station)
Station <- na.omit(Station)
Station_on <- Station
names(Station_on) <- c("On_station","on_name","on_h1","on_h2","on_h3")
Station_off <- Station
names(Station_off) <- c("Off_station","off_name","off_h1","off_h2","off_h3")

Cdata <- merge(x=Cdata,y=Station_on, by="On_station")
Cdata <- merge(x=Cdata,y=Station_off, by="Off_station")

#서대문구
Sdata = Cdata[Cdata$on_h2=="서대문구",] #departure
Sdata = Sdata[Sdata$On_hour==7|Sdata$On_hour==8,] #departure time
Station.off <- data.frame(table(Sdata$off_name))
names(Station.off) <- c("off_name","Off_freq")
Station.off <- na.omit(Station.off)
Station_order.off <- Station.off[order(-Station.off$Off_freq),]
Station_order.off[1:10,]

#가좌동 4동
Sdata = Cdata[Cdata$on_h3=="남가좌1동"|Cdata$on_h3=="남가좌2동"|Cdata$on_h3=="북가좌1동"|Cdata$on_h3=="북가좌2동",] #departure
Sdata = Sdata[Sdata$On_hour==7|Sdata$On_hour==8,] #departure time
Station.off <- data.frame(table(Sdata$off_name))
names(Station.off) <- c("off_name","Off_freq")
Station.off <- na.omit(Station.off)
Station_order.off <- Station.off[order(-Station.off$Off_freq),]
Station_order.off[1:10,]



Mdata_on=Cdata[Cdata$On_hour==7|Cdata$On_hour==8,]
Mdata_off=Cdata[Cdata$Off_hour==7|Cdata$Off_hour==8,]
Station.on <- data.frame(table(Mdata_on$On_station))
Station.off <- data.frame(table(Mdata_off$Off_station))
names(Station.on) <- c("station_code","On_freq")
names(Station.off) <- c("station_code","Off_freq")
Station <- merge(x=Station,y=Station.on,by='station_code',all.x=TRUE)
Station <- merge(x=Station,y=Station.off,by='station_code',all.x=TRUE)
Station <- Station[,-8]
Station <- Station[,-11]
Station <- Station[,-5:-7]
Station <- na.omit(Station)
Station_order.on <- Station[order(-Station$On_freq),]
Station_order.off <- Station[order(-Station$Off_freq),]
Station_order.on[1:10,]
Station_order.off[1:10,]

# it works but really slow 
Cdata$dong_on <- NA
Cdata$dong_off <- NA
nrow(Cdata)
nrow(Station)
for (i in 1:nrow(Cdata)){
  for (j in 1:nrow(Station)) {
    if (Cdata$On_station[i] == Station$station_code[j]){
      Cdata$dong_on[i] <- Station$H3[j]
    }
    else{}
  }
  if(i %% 100 ==0){print(100*i/409573)}
  else{}
}