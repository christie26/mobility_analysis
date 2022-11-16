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


#Trip chain data 생성하기. 통행 단위로 묶어서 환승들 다 하나로 묶은 것 trip_seq가 다른 것은 다른 통행으로 간주
Cdata_Tripchain<-data.frame()
for(i in 1:nrow(Cdata)){
  transfer_time_1=0
  transfer_time_2=0
  if(Cdata[i,3]==1){
    Cdata_Tripchain=rbind(Cdata_Tripchain,c(Cdata[i,1],Cdata[i,2],Cdata[i,3]-1,Cdata[i,7],Cdata[i,8],Cdata[i,9],Cdata[i,10],Cdata[i,12],Cdata[i,13],transfer_time_1,transfer_time_2))
  }
  else if(Cdata[i,3]==2){{
    Off_station_N=Cdata[i,8]
    A_time_H_N=Cdata[i,12]
    A_time_M_N=Cdata[i,13]
    Leg_seq_N=Cdata[i,3]-1
  }
    Cdata_Tripchain[nrow(Cdata_Tripchain),3]=Leg_seq_N
    Cdata_Tripchain[nrow(Cdata_Tripchain),5]=Off_station_N
    Cdata_Tripchain[nrow(Cdata_Tripchain),8]=A_time_H_N
    Cdata_Tripchain[nrow(Cdata_Tripchain),9]=A_time_M_N
    Cdata_Tripchain[nrow(Cdata_Tripchain),10]=Cdata[i,9]*60+Cdata[i,10]+Cdata[i,11]/60-Cdata[i-1,12]*60-Cdata[i-1,13]-Cdata[i-1,14]/60
    if(Cdata_Tripchain[nrow(Cdata_Tripchain),10]<0){
      Cdata_Tripchain[nrow(Cdata_Tripchain),10]=(24+Cdata[i,9])*60+Cdata[i,10]+Cdata[i,11]/60-Cdata[i-1,12]*60-Cdata[i-1,13]-Cdata[i-1,14]/60
    }
  }
  else if(Cdata[i,3]==3){
    Cdata_Tripchain[nrow(Cdata_Tripchain),11]=Cdata[i,9]*60+Cdata[i,10]+Cdata[i,11]/60-Cdata[i-1,12]*60-Cdata[i-1,13]-Cdata[i-1,14]/60
    if(Cdata_Tripchain[nrow(Cdata_Tripchain),11]<0){
      Cdata_Tripchain[nrow(Cdata_Tripchain),11]=(24+Cdata[i,9])*60+Cdata[i,10]+Cdata[i,11]/60-Cdata[i-1,12]*60-Cdata[i-1,13]-Cdata[i-1,14]/60
    }
  }
}
names(Cdata_Tripchain)<-c("user_ID","통행순서","환승횟수","On_station","Off_station","D_time_H","D_time_M","A_time_H","A_time_M","Transfer_time1","Transfer_time2")
Cdata_Tripchain$환승횟수[Cdata_Tripchain$Transfer_time2!=0]=2



#서대문구
Sdata = Cdata[Cdata$on_h2=="서대문구",] #departure
Sdata = Sdata[Sdata$On_hour==7|Sdata$On_hour==8,] #departure time
Station.off <- data.frame(table(Sdata$off_name))
names(Station.off) <- c("off_name","Off_freq")
Station.off <- na.omit(Station.off)
Station_order.off <- Station.off[order(-Station.off$Off_freq),]
Station_order.off[1:10,]

#가좌동 4동
Sdata = Cdata[Cdata$on_h2=="서대문구",] #departure
Sdata = Sdata[Sdata$On_hour==7|Sdata$On_hour==8,] #departure time
table(Sdata$on_h3)
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