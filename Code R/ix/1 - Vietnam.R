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
vietnam <- mydata[mydata$location == "Vietnam",]
attach(vietnam)

vietnam %>%
  select(c(new_cases, new_deaths)) %>%
  abs()
vietnam$date <- as.Date(vietnam$date, "%m/%d/%Y")
vietnamnhiem <- vietnam
vietnamtuvong <- vietnam[vietnam$date >= "2020-7-31", ]

tongcanhiem <- sum(vietnam$new_cases, na.rm = TRUE)
tongcatuvong <- sum(vietnam$new_deaths, na.rm = TRUE)

tongnhiemtichluy <- cumsum(vietnamnhiem$new_cases)
tongtuvongtichluy <- cumsum(vietnamtuvong$new_deaths)

result1 <- 100*tongnhiemtichluy/tongcanhiem
vietnamnhiem$new_cases <- result1
result2 <- 100*tongtuvongtichluy/tongcatuvong
vietnamtuvong$new_deaths <- result2

ketqua <- ggplot()+
  geom_line(data = vietnamnhiem, mapping = aes(x = date, y = new_cases, color = "Percentage between cumulative infections/total infections"))+
  geom_line(data = vietnamtuvong, mapping = aes(x = date, y = new_deaths, color = "Percentage between cumulative deaths/total deaths"))+
  scale_color_manual(name = "Note:", values = c("Percentage between cumulative infections/total infections" = "red", "Percentage between cumulative deaths/total deaths" = "blue"))
require(scales)
res <- ketqua + scale_y_continuous(labels = comma) + scale_x_date(date_breaks = "3 month", date_labels = "%b - %Y")
print(res + ggtitle("Percentage between cumulative infections/total infections and cumulative deaths/total deaths in Vietnam")
      + labs(x = "Date", y = "Percentage"))