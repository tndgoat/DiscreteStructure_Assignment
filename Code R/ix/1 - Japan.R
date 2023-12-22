setwd("C:/Users/Tung Nguyen/Downloads/HK212/BTL CTRR")

library(ggplot2)
library(tidyverse)
library(xts)
library(reshape2)
library(lubridate)
library(dplyr)
library(purrr)
library(hrbrthemes)

mydata <- read.csv("owid-covid-data.csv", header = TRUE)
save(mydata, file = 'mydata.rda')
japan <- mydata[mydata$location == "Japan",]
attach(japan)

japan %>%
  select(c(new_cases, new_deaths)) %>%
  abs()
japan$date <- as.Date(japan$date, "%m/%d/%Y")
japannhiem <- japan[japan$date >= "2020-1-23", ]
japantuvong <- japan[japan$date >= "2020-2-13", ]

tongcanhiem <- sum(japan$new_cases, na.rm = TRUE)
tongcatuvong <- sum(japan$new_deaths, na.rm = TRUE)

tongnhiemtichluy <- cumsum(japannhiem$new_cases)
tongtuvongtichluy <- cumsum(japantuvong$new_deaths)

result1 <- 100*tongnhiemtichluy/tongcanhiem
japannhiem$new_cases <- result1
result2 <- 100*tongtuvongtichluy/tongcatuvong
japantuvong$new_deaths <- result2

ketqua <- ggplot()+
  geom_line(data = japannhiem, mapping = aes(x = date, y = new_cases, color = "Percentage between cumulative infections/total infections"))+
  geom_line(data = japantuvong, mapping = aes(x = date, y = new_deaths, color = "Percentage between cumulative deaths/total deaths"))+
  scale_color_manual(name = "Note:", values = c("Percentage between cumulative infections/total infections" = "red", "Percentage between cumulative deaths/total deaths" = "blue"))
require(scales)
res <- ketqua + scale_y_continuous(labels = comma) + scale_x_date(date_breaks = "3 month", date_labels = "%b - %Y")
print(res + ggtitle("Percentage between cumulative infections/total infections and cumulative deaths/total deaths in Japan")
      + labs(x = "Date", y = "Percentage"))