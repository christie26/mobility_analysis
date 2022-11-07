HHdata <-read.csv("C:/Users/JW/Desktop/22-2 도공/도빌리티/도빌리티 4주차/Travel_diary_data.csv")


HHdata1 <- HHdata[(HHdata$Dcode==1141070000)|(HHdata$Dcode==1141070000)|(HHdata$Dcode==1141071000)|(HHdata$Dcode==1141072000),]

HHdata1$Trip_time_d <-((HHdata1$Trip_d_P-1)*12+(HHdata1$Trip_d_hh)*60+(HHdata1$Trip_d_mm))


HHdata11=HHdata1[HHdata1$Trip_time_d>=420 & HHdata1$Trip_time_d<=600,]

Trip_time_d_den <- density(na.omit(HHdata11$Trip_time_d))

plot(Trip_time_d_den,xlim=c(420,600))


HHdata11$job1 <- NA
HHdata11 <- within(HHdata11,{
job1[Job==1] <- 1 
job1[Job==2] <- 2 
job1[Job==3] <- 3 
job1[Job==4] <- 4
job1[Job==5] <- 5 
job1[Job==6] <- 6 
job1[Job==7] <- 7
job1[Job==8] <- 8 
})

#Trip mode > Trip purp로 바꿔서 출근,업무,여가같은 핵심 5가지로 추려서 보기
#Trip purp, Job 두개로 나눠서 봐도 좋을 듯 ? 


install.packages("sm")
library(sm)

sm.density.compare(HHdata11$Trip_time_d, HHdata11$Job, xlab="Travel time (departure time=Xyalue/60)",xlim=c(420,600))

title(main="Travel time distribution by job")

job1 <- factor(HHdata11$Job, levels=c(1,2,3,4,5,6,7,8,9), labels=c("specialized","service","Retail","management or office","Agricultural","simple labor","homemaker","unempolyed of student","etc"))
colfill <-c((2:(1+length(levels(job1)))))
legend("topright", levels(job1),cex=0.6,lwd=c(1,1),fill=colfill)

