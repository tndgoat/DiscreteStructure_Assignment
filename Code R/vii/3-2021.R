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

thang11 <- mydata2[mydata2$date >= "2021-11-1" & mydata2$date <= "2021-11-30", ]
thang12 <- mydata2[mydata2$date >= "2021-12-1" & mydata2$date <= "2021-12-31", ]

thang11$date <- day(thang11$date)
thang12$date <- day(thang12$date)

ketqua <- ggplot()+
  geom_line(data = thang11, mapping = aes(x = date, y = new_cases, color = "November"))+
  geom_line(data = thang12, mapping = aes(x = date, y = new_cases, color = "December"))+
  scale_color_manual(name = "Note:", values = c("November" = "orange", "December" = "green"))
require(scales)
res <- ketqua + scale_y_continuous(labels = comma)
print(res + ggtitle("New cases in Nov-Dec of 2021")
      + labs(x = "Date", y = "New cases"))