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
  result[i]<- mean(mydata2$new_deaths[1:i])
}
for (i in 8: 760){
  result[i] <- mean(mydata2$new_deaths[(i-6):i])
}
mydata2$new_deaths <- result
mydata2 <- mydata2[mydata2$date >= "2021-11-1" & mydata2$date <= "2021-12-31", ]

ketqua <- ggplot() + geom_line(data = mydata2, mapping = aes(x = date, y = new_deaths, color = date))
require(scales)
res <- ketqua + scale_y_continuous(labels = comma) + scale_x_date(date_breaks = "1 week", date_labels = "%b %d")
print(res + ggtitle("Data of average new deaths over the last 7 days in Nov-Dec of 2021")
      + labs(x = "Date", y = "Average of new deaths"))