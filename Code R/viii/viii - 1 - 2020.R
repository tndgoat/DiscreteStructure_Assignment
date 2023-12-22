setwd("C:/Users/Tung Nguyen/Downloads/HK212/BTL CTRR")

library(ggplot2)
library(tidyverse)
library(xts)
library(reshape2)
library(lubridate)
library(dplyr)
library(purrr)
library(hrbrthemes)

mydata1 <- read.csv("owid-covid-data.csv", header = TRUE)
save(mydata1, file = 'mydata1.rda')
mydata2 <- mydata1[mydata1$location == "World",]
attach(mydata2)

mydata2 %>%
  select(c(new_cases, new_deaths)) %>%
  abs()
mydata2$date <- as.Date(mydata2$date, "%m/%d/%Y")

result <- vector(length = 760)
for(i in 1: 7){
  result[i]<- mean(mydata2$new_cases[1:i])
}
for (i in 8: 760){
  result[i] <- mean(mydata2$new_cases[(i-6):i])
}
mydata2$new_cases <- result

thang1 <- mydata2[mydata2$date >= "2020-1-1" & mydata2$date <= "2020-1-31", ]
thang2 <- mydata2[mydata2$date >= "2020-2-1" & mydata2$date <= "2020-2-29", ]
thang4 <- mydata2[mydata2$date >= "2020-4-1" & mydata2$date <= "2020-4-30", ]
thang10 <- mydata2[mydata2$date >= "2020-10-1" & mydata2$date <= "2020-10-31", ]

ketqua <- ggplot()+
  geom_line(data = thang1, mapping = aes(x = date, y = new_cases, color = date))+
  geom_line(data = thang2, mapping = aes(x = date, y = new_cases, color = date))+
  geom_line(data = thang4, mapping = aes(x = date, y = new_cases, color = date))+
  geom_line(data = thang10, mapping = aes(x = date, y = new_cases, color = date))
require(scales)
res <- ketqua + scale_y_continuous(labels = comma) + scale_x_date(date_breaks = "1 month", date_labels = "%b - %Y")
print(res + ggtitle("Data of average new cases over the last 7 days in Jan-Feb-Apr-Oct of 2020")
      + labs(x = "Date", y = "Average of new cases"))