HHdata <-read.csv("/Users/user/projects/mobility_data/Travel_diary_data.csv")
Dcode <-read.csv("/Users/user/projects/mobility_data/District_code.csv",encoding="UTF-8")

colnames(Dcode)
names(Dcode)[7] <- "Dcode"
names(Dcode)[1] <- "D1"
names(Dcode)[2] <- "D2"
names(Dcode)[4] <- "D3"

#Total <- merge(HHdata,Dcode,by="Dcode")
#HHdata1 <- HHdata[(HHdata$Dcode==1141052000)|(HHdata$Dcode==1141055500)|(HHdata$Dcode==1141056500)|(HHdata$Dcode==1141058500)|(HHdata$Dcode==1141061500)|(HHdata$Dcode==1141062000)|(HHdata$Dcode==1141064000)|(HHdata$Dcode==1141065500)|(HHdata$Dcode==1141066000)|(HHdata$Dcode==1141068500)|(HHdata$Dcode==1141069000)|(HHdata$Dcode==1141070000)|(HHdata$Dcode==1141071000)|(HHdata$Dcode==1141072000),]

remove.packages("rlang")
remove.packages("dplyr")

install.packages("rlang")
install.packages("acepack")
install.packages("Rcpp")
install.packages("bindrcpp")
install.packages("dplyr")

library(rlang)
library(dplyr)
Total <- inner_join(HHdata, Dcode, by = 'Dcode')

#create new column that has district data
HHdata$D1 <- NA
HHdata$D2 <- NA
HHdata$D3 <- NA

table(HHdata$Dcode)
HHdata1$district <- NA
HHdata1 <- within(HHdata1,{
  district[HHdata1$Dcode==1141052000] <- "천연동"
  district[HHdata1$Dcode==1141055500] <- "북아현동"
  district[HHdata1$Dcode==1141056500] <- "충현동"
  district[HHdata1$Dcode==1141058500] <- "신촌동"
  district[HHdata1$Dcode==1141061500] <- "연희동"
  district[HHdata1$Dcode==1141062000] <- "홍제1동"
  district[HHdata1$Dcode==1141064000] <- "홍제3동"
  district[HHdata1$Dcode==1141065500] <- "홍제2동"
  district[HHdata1$Dcode==1141066000] <- "홍은1동"
  district[HHdata1$Dcode==1141068500] <- "홍은2동"
  district[HHdata1$Dcode==1141069000] <- "남가좌1동"
  district[HHdata1$Dcode==1141070000] <- "남가좌2동"
  district[HHdata1$Dcode==1141071000] <- "북가좌1동"
  district[HHdata1$Dcode==1141072000] <- "북가좌2동"
  })

table(HHdata1$TripL_mode)

HHdata_sub <- HHdata1[HHdata1$TripL_mode==10,]
table(HHdata_sub$district)

HHdata_sub_d <- table(HHdata_sub$district)
HHdata1_d <- table(HHdata1$district)

barplot(HHdata_sub_d/HHdata1_d)
