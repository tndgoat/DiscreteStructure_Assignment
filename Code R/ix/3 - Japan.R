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
japan <- mydata[mydata$location == "Japan", ]
attach(japan)

japan$new_cases[is.na(japan$new_cases)] <- 0
japan$new_deaths[is.na(japan$new_deaths)] <- 0

japan %>%
  select(c(new_cases, new_deaths)) %>%
  abs()
japan$date <- as.Date(japan$date, "%m/%d/%Y")

japan$year <- strftime(japan$date, "%Y")
japan$month <- strftime(japan$date, "%m")
japan$dates<- strftime(japan$date, "%d")

result1 <- vector(length = 720)
for(i in 1: 7){
  result1[i]<- mean(japan$new_cases[1:i])
}
for (i in 8: 720){
  result1[i] <- mean(japan$new_cases[(i-6):i])
}
japan$new_cases <- result1

result2 <- vector(length = 720)
for(i in 1: 7){
  result2[i]<- mean(japan$new_deaths[1:i])
}
for (i in 8: 720){
  result2[i] <- mean(japan$new_deaths[(i-6):i])
}
japan$new_deaths <- result2

japan_2020 <- subset(japan, (year == "2020"))
japan_2021 <- subset(japan, (year == "2021"))
japan_2022 <- subset(japan, (year == "2022"))

japan_2020_1 <- subset(japan_2020,(month=="01"))
japan_2020_2 <- subset(japan_2020,(month=="02"))
japan_2020_4 <- subset(japan_2020, (month == "04"))
japan_2020_10 <- subset(japan_2020, (month == "10"))

japan_2021_1 <- subset(japan_2021,(month=="01"))
japan_2021_2 <- subset(japan_2021,(month=="02"))
japan_2021_4 <- subset(japan_2021,(month=="04"))
japan_2021_10 <- subset(japan_2021,(month=="10"))

japan_2022_1 <- subset(japan_2022,(month=="01"))
japan_2022_2 <- subset(japan_2022,(month=="02"))

ggplot(japan_2020_1,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 1/2020 cua Japan") + geom_smooth()
print(cor(japan_2020_1$new_cases,japan_2020_1$new_deaths))

ggplot(japan_2020_2,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 2/2020 cua Japan")+ geom_smooth()
print(cor(japan_2020_2$new_cases,japan_2020_2$new_deaths))

ggplot(japan_2020_4,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 4/2020 cua Japan") + geom_smooth()
print(cor(japan_2020_4$new_cases,japan_2020_4$new_deaths))

ggplot(japan_2020_10,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 10/2020 cua Japan") + geom_smooth()
print(cor(japan_2020_10$new_cases,japan_2020_10$new_deaths))

ggplot(japan_2021_1,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 1/2021 cua Japan") + geom_smooth()
print(cor(japan_2021_1$new_cases,japan_2021_1$new_deaths))

ggplot(japan_2021_2,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 2/2021 cua Japan")+ geom_smooth()
print(cor(japan_2021_2$new_cases,japan_2021_2$new_deaths))

ggplot(japan_2021_4,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 4/2021 cua Japan")+ geom_smooth()
print(cor(japan_2021_4$new_cases,japan_2021_4$new_deaths))

ggplot(japan_2021_10,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 10/2021 cua Japan")+ geom_smooth()
print(cor(japan_2021_10$new_cases,japan_2021_10$new_deaths))

ggplot(japan_2022_1,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 1/2022 cua Japan")+ geom_smooth()
print(cor(japan_2022_1$new_cases,japan_2022_1$new_deaths))

ggplot(japan_2022_2,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 2/2022 cua Japan")+ geom_smooth()
print(cor(japan_2022_2$new_cases,japan_2022_2$new_deaths))