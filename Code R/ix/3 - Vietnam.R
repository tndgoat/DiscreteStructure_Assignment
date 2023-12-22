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
vietnam <- mydata[mydata$location == "Vietnam", ]
attach(vietnam)

vietnam$new_cases[is.na(vietnam$new_cases)] <- 0
vietnam$new_deaths[is.na(vietnam$new_deaths)] <- 0

vietnam %>%
  select(c(new_cases, new_deaths)) %>%
  abs()
vietnam$date <- as.Date(vietnam$date, "%m/%d/%Y")

vietnam$year <- strftime(vietnam$date, "%Y")
vietnam$month <- strftime(vietnam$date, "%m")
vietnam$dates<- strftime(vietnam$date, "%d")

result1 <- vector(length = 720)
for(i in 1: 7){
  result1[i]<- mean(vietnam$new_cases[1:i])
}
for (i in 8: 720){
  result1[i] <- mean(vietnam$new_cases[(i-6):i])
}
vietnam$new_cases <- result1

result2 <- vector(length = 720)
for(i in 1: 7){
  result2[i]<- mean(vietnam$new_deaths[1:i])
}
for (i in 8: 720){
  result2[i] <- mean(vietnam$new_deaths[(i-6):i])
}
vietnam$new_deaths <- result2

vietnam_2020 <- subset(vietnam, (year == "2020"))
vietnam_2021 <- subset(vietnam, (year == "2021"))
vietnam_2022 <- subset(vietnam, (year == "2022"))

vietnam_2020_1 <- subset(vietnam_2020,(month=="01"))
vietnam_2020_2 <- subset(vietnam_2020,(month=="02"))
vietnam_2020_4 <- subset(vietnam_2020, (month == "04"))
vietnam_2020_10 <- subset(vietnam_2020, (month == "10"))

vietnam_2021_1 <- subset(vietnam_2021,(month=="01"))
vietnam_2021_2 <- subset(vietnam_2021,(month=="02"))
vietnam_2021_4 <- subset(vietnam_2021,(month=="04"))
vietnam_2021_10 <- subset(vietnam_2021,(month=="10"))

vietnam_2022_1 <- subset(vietnam_2022,(month=="01"))
vietnam_2022_2 <- subset(vietnam_2022,(month=="02"))

ggplot(vietnam_2020_1,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 1/2020 cua Vietnam") + geom_smooth()
print(cor(vietnam_2020_1$new_cases,vietnam_2020_1$new_deaths))

ggplot(vietnam_2020_2,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 2/2020 cua Vietnam")+ geom_smooth()
print(cor(vietnam_2020_2$new_cases,vietnam_2020_2$new_deaths))

ggplot(vietnam_2020_4,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 4/2020 cua Vietnam") + geom_smooth()
print(cor(vietnam_2020_4$new_cases,vietnam_2020_4$new_deaths))

ggplot(vietnam_2020_10,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 10/2020 cua Vietnam") + geom_smooth()
print(cor(vietnam_2020_10$new_cases,vietnam_2020_10$new_deaths))

ggplot(vietnam_2021_1,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 1/2021 cua Vietnam") + geom_smooth()
print(cor(vietnam_2021_1$new_cases,vietnam_2021_1$new_deaths))

ggplot(vietnam_2021_2,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 2/2021 cua Vietnam")+ geom_smooth()
print(cor(vietnam_2021_2$new_cases,vietnam_2021_2$new_deaths))

ggplot(vietnam_2021_4,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 4/2021 cua Vietnam")+ geom_smooth()
print(cor(vietnam_2021_4$new_cases,vietnam_2021_4$new_deaths))

ggplot(vietnam_2021_10,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 10/2021 cua Vietnam")+ geom_smooth()
print(cor(vietnam_2021_10$new_cases,vietnam_2021_10$new_deaths))

ggplot(vietnam_2022_1,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 1/2022 cua Vietnam")+ geom_smooth()
print(cor(vietnam_2022_1$new_cases,vietnam_2022_1$new_deaths))

ggplot(vietnam_2022_2,(aes(x=new_cases,y=new_deaths)))+geom_point(shape=1,color="red")+facet_wrap( ~ month)+ ggtitle("Bieu do tuong quan giua ca nhiem va tu vong theo trung binh 7 ngay gan nhat thang 2/2022 cua Vietnam")+ geom_smooth()
print(cor(vietnam_2022_2$new_cases,vietnam_2022_2$new_deaths))