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

thang1 <- mydata2[mydata2$date >= "2022-1-1" & mydata2$date <= "2022-1-31", ]
thang2 <- mydata2[mydata2$date >= "2022-2-1" & mydata2$date <= "2022-2-28", ]

thang1$date <- day(thang1$date)
thang2$date = day(thang2$date)

ketqua <- ggplot()+
  geom_line(data = thang1, mapping = aes(x = date, y = new_deaths, color = "January"))+
  geom_line(data = thang2, mapping = aes(x = date, y = new_deaths, color = "February"))+
  scale_color_manual(name = "Note:", values = c("January" = "blue", "February" = "red"))
require(scales)
res <- ketqua + scale_y_continuous(labels = comma)
print(res + ggtitle("New deaths in Jan-Feb of 2022")
      + labs(x = "Date", y = "New deaths"))