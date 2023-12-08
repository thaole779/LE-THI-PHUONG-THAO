Junggu<-read.csv('Junggu.csv',fileEncoding='euc-kr',check.names=F)
remove(mydata)
tongteamun<-read.csv('tongteamun.csv',fileEncoding='euc-kr',check.names=F)
install.packages('sysfonts')
library(sysfonts)
font_add("NanumGothic","/usr/share/fonts/NanumFont/NanumGothic.ttf")
library(showtext)
font_add_google("Nanum Gothic", "NanumGothic")
showtext_auto()
plot(1:10, main="가나다", family="NanumGothic")
Junggu$상세영업상태명.freq <- table(Junggu$상세영업상태명)
barplot(table(Junggu$상세영업상태명), main="중구구 영업/폐엽 현황",family="NanumGothic")
tongteamun$상세영업상태명.freq <- table(tongteamun$상세영업상태명)
barplot(table(tongteamun$상세영업상태명), main="동대문구 영업/폐엽 현황",family="NanumGothic")
 pie(table(Junggu$업태분명))
 pie(table(tongteamun$업태구분명))
hist(plot(density(age),add=TRUE))
hist(tongteamun$open_day, main="오픈일별 빈도분포", xlab="영업일", ylab="가게")
hist(Junggu$Open_day)
hist(Junggu$Open_day, main="오픈일별 빈도분포", xlab="영업일", ylab="가게")
plot(density(tongteamun$open_day),add=TRUE)
lipid <- data.frame(tongteamun$open_day,tongteamun$영업상태명)
# Transfer ordinal variable into numeric variable
tongteamun$상세영업상태명 <-ifelse(tongteamun$상세영업상태명 == "영업", 1,ifelse(tongteamun$상세영업상태명 == "폐업", 2)
# Frequency Table
Frequency <- table(tongteamun$상세영업상태명)
Frequency
# Frequency Table
Frequency <- table(Junggu$상세영업상태명)
# Summary data
summary(tongteamun)                            
# Setting interval
intervals <- seq(-50, 100, by=25)
# Setting left & right options
# e.g.) -50 < x <= -25 and so on
value.cut <- cut(tongteamun$open_day, intervals,
                 left=FALSE, right=TRUE)
value.freq <- (value.cut)
hist(tongteamun$open_day, breaks = intervals, right = T,
     main = "Histogram for the annual returns(%) for the Value Fund",
     xlab = "Annual Returns(%) for Value",
     col = "blue")
# Check number of observations and variables
dim(tongteamun)
# Missing value checking
tongteamun[!complete.cases(tongteamun),]
# Table
myTabletong <- table(tongteamun$open_day, tongteamun$상세영업상태명)
myTable
# Table
myTablejung <- table(Junggu$Open_day, Junggu$상세영업상태명)
myTable
# Proportional Table
prop.table(myTabletong)
# Proportional Table
prop.table(myTablejung)
# Table
myTable2 <- table(tongteamun$영업상태명, tongteamun$상세영업상태명)
barplot(myTable2, main = "Location and Purchase",
        col = c('blue', 'red'), legend=rownames(myTable2),
        xlab = 'Location', ylab = 'Count', ylim = c(0, 200))
plot(tongteamun$인허가일자,
     main="A Line Chart for the Growth and Value Mutual Funds", xlab="Year", ylab="Annual Returns", col="blue",
     type="l", ylim=c(-100, 100))



library(tidyverse)
library(plotly)
library(COUNT)
a <- read.csv('~/Documents/b/tongteamun.csv', 
               header=T,
               fileEncoding = 'euc-kr',
               encoding = 'utf-8') 
b <- read.csv('~/Documents/b/Junggu.csv', 
              header=T,
              fileEncoding = 'euc-kr',
              encoding = 'utf-8') 
date <- as.Date(as.character(a$인허가일자), format = "%Y%m%d")
lt <- unclass(as.POSIXlt(date))
a[, c("날짜", "연", "월", "일")] <- with(lt, 
                                     data.frame(날짜 = date
                                                , 연 = year + 1900
                                                , 월 = mon + 1
                                                , 일 = mday))
a$인허가일자 <- cut(a$인허가일자, seq(20190101,20190131,7),right = F)

levels(a$인허가일자) <- c("w1","w2","w3","w4")
myTable(a$인허가일자)

a$re.date <- cut(a$인허가일자, 
                  br=c(20190100,20190106,20190113,20190120,20190127,20190132),
                  labels=c("w1","w2","w3","w4","w5"))

myTable(a$re.date)
a$re.date

head(a)

df2 <- cbind(a[,11:13], a[,2:4], a[,6:7], a[9])

head(a)
head(df2)
levels(df2$영업상태명) <- c(1, 0)
p <- df2 %>%
  plot_ly(
    x = ~ openday, 
    y = ~통화건수, 
    color = ~시군구, 
    frame = ~일, 
    type = 'scatter',
    mode = 'markers'
  ) 

ggplotly(p)
