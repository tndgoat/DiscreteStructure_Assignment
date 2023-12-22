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
indo <- mydata[mydata$location == "Indonesia",]
attach(indo)

indo %>%
  select(c(new_cases, new_deaths)) %>%
  abs()
indo$date <- as.Date(indo$date, "%m/%d/%Y")
indonhiem <- indo[indo$date <= "2022-2-18", ]
indotuvong <- indo[indo$date >= "2020-3-11", ]

tongcanhiem <- sum(indo$new_cases, na.rm = TRUE)
tongcatuvong <- sum(indo$new_deaths, na.rm = TRUE)

tongnhiemtichluy <- cumsum(indonhiem$new_cases)
tongtuvongtichluy <- cumsum(indotuvong$new_deaths)

result1 <- 100*tongnhiemtichluy/tongcanhiem
indonhiem$new_cases <- result1
result2 <- 100*tongtuvongtichluy/tongcatuvong
indotuvong$new_deaths <- result2

ketqua <- ggplot()+
  geom_line(data = indonhiem, mapping = aes(x = date, y = new_cases, color = "Percentage between cumulative infections/total infections"))+
  geom_line(data = indotuvong, mapping = aes(x = date, y = new_deaths, color = "Percentage between cumulative deaths/total deaths"))+
  scale_color_manual(name = "Note:", values = c("Percentage between cumulative infections/total infections" = "red", "Percentage between cumulative deaths/total deaths" = "blue"))
require(scales)
res <- ketqua + scale_y_continuous(labels = comma) + scale_x_date(date_breaks = "3 month", date_labels = "%b - %Y")
print(res + ggtitle("Percentage between cumulative infections/total infections and cumulative deaths/total deaths in Indonesia")
      + labs(x = "Date", y = "Percentage"))