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
indo <- mydata[mydata$location == "Indonesia", ]
attach(indo)

indo$new_cases[is.na(indo$new_cases)] <- 0
indo$new_deaths[is.na(indo$new_deaths)] <- 0

indo %>%
  select(c(new_cases, new_deaths)) %>%
  abs()
indo$date <- as.Date(indo$date, "%m/%d/%Y")

indo$year <- strftime(indo$date, "%Y")
indo$month <- strftime(indo$date, "%m")
indo$dates<- strftime(indo$date, "%d")

indo_2020 <- subset(indo, (year == "2020"))
indo_2021 <- subset(indo, (year == "2021"))
indo_2022 <- subset(indo, (year == "2022"))

indo_2020_4 <- subset(indo_2020, (month == "04"))
indo_2020_10 <- subset(indo_2020, (month == "10"))

indo_2021_1 <- subset(indo_2021,(month=="01"))
indo_2021_2 <- subset(indo_2021,(month=="02"))
indo_2021_4 <- subset(indo_2021,(month=="04"))
indo_2021_10 <- subset(indo_2021,(month=="10"))

indo_2022_1 <- subset(indo_2022,(month=="01"))
indo_2022_2 <- subset(indo_2022,(month=="02"))

ggplot(indo_2020_4,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 4/2020 cua Indonesia") + geom_smooth()
print(cor(indo_2020_4$new_cases,indo_2020_4$new_deaths))

ggplot(indo_2020_10,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 10/2020 cua Indonesia") + geom_smooth()
print(cor(indo_2020_10$new_cases,indo_2020_10$new_deaths))

ggplot(indo_2021_1,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 1/2021 cua Indonesia") + geom_smooth()
print(cor(indo_2021_1$new_cases,indo_2021_1$new_deaths))

ggplot(indo_2021_2,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 2/2021 cua Indonesia")+ geom_smooth()
print(cor(indo_2021_2$new_cases,indo_2021_2$new_deaths))

ggplot(indo_2021_4,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 4/2021 cua Indonesia")+ geom_smooth()
print(cor(indo_2021_4$new_cases,indo_2021_4$new_deaths))

ggplot(indo_2021_10,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 10/2021 cua Indonesia")+ geom_smooth()
print(cor(indo_2021_10$new_cases,indo_2021_10$new_deaths))

ggplot(indo_2022_1,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 1/2022 cua Indonesia")+ geom_smooth()
print(cor(indo_2022_1$new_cases,indo_2022_1$new_deaths))

ggplot(indo_2022_2,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong thang 2/2022 cua Indonesia")+ geom_smooth()
print(cor(indo_2022_2$new_cases,indo_2022_2$new_deaths))