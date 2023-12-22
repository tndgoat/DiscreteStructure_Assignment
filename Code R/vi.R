library(slider)
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
data <- read_csv(file='D:/BTL/covidData.csv')
#country
data %>%
  mutate(date = as.Date(date,"%m/%d/%Y")) %>%
  group_by(date) %>%
  filter(iso_code=="IDN") %>%
  summarise(new_cases,new_deaths)-> IDN
data %>%
  mutate(date = as.Date(date,"%m/%d/%Y")) %>%
  group_by(date) %>%
  filter(iso_code=="JPN") %>%
  summarise(new_cases,new_deaths)-> JPN
data %>%
  mutate(date = as.Date(date,"%m/%d/%Y")) %>%
  group_by(date) %>%
  filter(iso_code=="VNM") %>%
  summarise(new_cases,new_deaths)-> VNM
#7 days average replacement
fix_cases <- function(df) {
  df$new_cases =
    ifelse(
      (is.na(df$new_cases)|df$new_cases<=0),
      floor(slider::slide_dbl(df$new_cases, mean, na.rm = TRUE, .before = 7, .after = -1)),
      df$new_cases
    )
}
fix_deaths <- function(df) {
  df$new_deaths =
    ifelse(
      (is.na(df$new_deaths)|df$new_deaths<=0),
      floor(slider::slide_dbl(df$new_deaths, mean, na.rm = TRUE, .before = 7, .after = -1)),
      df$new_deaths
    )
}
IDN$new_cases <- fix_cases(IDN)
JPN$new_cases <- fix_cases(JPN)
VNM$new_cases <- fix_cases(VNM)
IDN$new_deaths <- fix_deaths(IDN)
JPN$new_deaths <- fix_deaths(JPN)
VNM$new_deaths <- fix_deaths(VNM)
#check NA
IDN$new_cases[is.na(IDN$new_cases)] = 0
JPN$new_cases[is.na(JPN$new_cases)] = 0
VNM$new_cases[is.na(VNM$new_cases)] = 0
IDN$new_deaths[is.na(IDN$new_deaths)] = 0
JPN$new_deaths[is.na(JPN$new_deaths)] = 0
VNM$new_deaths[is.na(VNM$new_deaths)] = 0
#extract data
#IDN 1204
IDN %>% subset((date>="2020/01/01"&date<="2020/01/31")
               |(date>="2020/02/01"&date<="2020/02/29")
               |(date>="2020/04/01"&date<="2020/04/30")
               |(date>="2020/10/01"&date<="2020/10/31")
) -> IDN2020
IDN %>% subset((date>="2021/01/01"&date<="2021/01/31")
               |(date>="2021/02/01"&date<="2021/02/28")
               |(date>="2021/04/01"&date<="2021/04/30")
               |(date>="2021/10/01"&date<="2021/10/31")
) -> IDN2021
IDN %>% subset((date>="2022/01/01"&date<="2022/01/31")
               |(date>="2022/02/01"&date<="2022/02/28")
               |(date>="2022/04/01"&date<="2022/04/30")
               |(date>="2022/10/01"&date<="2022/10/31")
) -> IDN2022

JPN %>% subset((date>="2020/01/01"&date<="2020/01/31")
               |(date>="2020/02/01"&date<="2020/02/29")
               |(date>="2020/04/01"&date<="2020/04/30")
               |(date>="2020/10/01"&date<="2020/10/31")
) -> JPN2020
JPN %>% subset((date>="2021/01/01"&date<="2021/01/31")
               |(date>="2021/02/01"&date<="2021/02/28")
               |(date>="2021/04/01"&date<="2021/04/30")
               |(date>="2021/10/01"&date<="2021/10/31")
) -> JPN2021
JPN %>% subset((date>="2022/01/01"&date<="2022/01/31")
               |(date>="2022/02/01"&date<="2022/02/28")
               |(date>="2022/04/01"&date<="2022/04/30")
               |(date>="2022/10/01"&date<="2022/10/31")
) -> JPN2022

VNM %>% subset((date>="2020/01/01"&date<="2020/01/31")
               |(date>="2020/02/01"&date<="2020/02/29")
               |(date>="2020/04/01"&date<="2020/04/30")
               |(date>="2020/10/01"&date<="2020/10/31")
) -> VNM2020
VNM %>% subset((date>="2021/01/01"&date<="2021/01/31")
               |(date>="2021/02/01"&date<="2021/02/28")
               |(date>="2021/04/01"&date<="2021/04/30")
               |(date>="2021/10/01"&date<="2021/10/31")
) -> VNM2021
VNM %>% subset((date>="2022/01/01"&date<="2022/01/31")
               |(date>="2022/02/01"&date<="2022/02/28")
               |(date>="2022/04/01"&date<="2022/04/30")
               |(date>="2022/10/01"&date<="2022/10/31")
) -> VNM2022

IDN %>% subset((date>="2020/11/01"&date<="2020/12/31")
) -> IDN2020end
IDN %>% subset((date>="2021/11/01"&date<="2021/12/31")
) -> IDN2021end
JPN %>% subset((date>="2020/11/01"&date<="2020/12/31")
) -> JPN2020end
JPN %>% subset((date>="2021/11/01"&date<="2021/12/31")
) -> JPN2021end
VNM %>% subset((date>="2020/11/01"&date<="2020/12/31")
) -> VNM2020end
VNM %>% subset((date>="2021/11/01"&date<="2021/12/31")
) -> VNM2021end
#VI.1204
#1
#IDN
IDN2020 %>% ggplot(aes(x=date,y= new_cases)) +
  geom_col()+
  labs(title = "*1.Indonesia 2020 new cases", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
IDN2021 %>% ggplot(aes(x=date,y= new_cases)) +
  geom_col()+
  labs(title = "*1.Indonesia 2021 new cases", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
IDN2022 %>% ggplot(aes(x=date,y= new_cases)) +
  geom_col()+
  labs(title = "*1.Indonesia 2022 new cases", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#JPN
JPN2020 %>% ggplot(aes(x=date,y= new_cases)) +
  geom_col()+
  labs(title = "*1.Japan 2020 new cases", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
JPN2021 %>% ggplot(aes(x=date,y= new_cases)) +
  geom_col()+
  labs(title = "*1.Japan 2021 new cases", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
JPN2022 %>% ggplot(aes(x=date,y= new_cases)) +
  geom_col()+
  labs(title = "*1.Japan 2022 new cases", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#VNM
VNM2020 %>% ggplot(aes(x=date,y= new_cases)) +
  geom_col()+
  labs(title = "*1.Vietnam 2020 new cases", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
VNM2021 %>% ggplot(aes(x=date,y= new_cases)) +
  geom_col()+
  labs(title = "*1.Vietnam 2021 new cases", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks") 
VNM2022 %>% ggplot(aes(x=date,y= new_cases)) +
  geom_col()+
  labs(title = "*1.Vietnam 2022 new cases", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#2
#IDN
IDN2020 %>% ggplot(aes(x=date,y= new_deaths)) +
  geom_col()+
  labs(title = "*2.Indonesia 2020 new deaths", x = "date", y = "new deaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
IDN2021 %>% ggplot(aes(x=date,y= new_deaths)) +
  geom_col()+
  labs(title = "*2.Indonesia 2021 new deaths", x = "date", y = "new deaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
IDN2022 %>% ggplot(aes(x=date,y= new_deaths)) +
  geom_col()+
  labs(title = "*2.Indonesia 2022 new deaths", x = "date", y = "new deaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#JPN
JPN2020 %>% ggplot(aes(x=date,y= new_deaths)) +
  geom_col()+
  labs(title = "*2.Japan 2020 new deaths", x = "date", y = "new deaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
JPN2021 %>% ggplot(aes(x=date,y= new_deaths)) +
  geom_col()+
  labs(title = "*2.Japan 2021 new deaths", x = "date", y = "new deaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
JPN2022 %>% ggplot(aes(x=date,y= new_deaths)) +
  geom_col()+
  labs(title = "*2.Japan 2022 new deaths", x = "date", y = "new deaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#VNM
VNM2020 %>% ggplot(aes(x=date,y= new_deaths)) +
  geom_col()+
  labs(title = "*2.Vietnam 2020 new deaths", x = "date", y = "new deaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
VNM2021 %>% ggplot(aes(x=date,y= new_deaths)) +
  geom_col()+
  labs(title = "*2.Vietnam 2021 new deaths", x = "date", y = "new deaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
VNM2022 %>% ggplot(aes(x=date,y= new_deaths)) +
  geom_col()+
  labs(title = "*2.Vietnam 2022 new deaths", x = "date", y = "new deaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#3
#IDN
IDN2020 %>% ggplot(aes(x=date,y= new_cases + new_deaths)) +
  geom_col()+
  labs(title = "*3.Indonesia 2020 new cases/deaths", x = "date", y = "new cases/newdeaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
IDN2021 %>% ggplot(aes(x=date,y= new_cases + new_deaths)) +
  geom_col()+
  labs(title = "*3.Indonesia 2021 new cases/deaths", x = "date", y = "new cases/newdeaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
IDN2022 %>% ggplot(aes(x=date,y= new_cases + new_deaths)) +
  geom_col()+
  labs(title = "*3.Indonesia 2022 new cases/deaths", x = "date", y = "new cases/newdeaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#JPN
JPN2020 %>% ggplot(aes(x=date,y= new_cases + new_deaths)) +
  geom_col()+
  labs(title = "*3.Japan 2020 new cases/deaths", x = "date", y = "new cases/newdeaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
JPN2021 %>% ggplot(aes(x=date,y= new_cases + new_deaths)) +
  geom_col()+
  labs(title = "*3.Japan 2021 new cases/deaths", x = "date", y = "new cases/newdeaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
JPN2022 %>% ggplot(aes(x=date,y= new_cases + new_deaths)) +
  geom_col()+
  labs(title = "*3.Japan 2022 new cases/deaths", x = "date", y = "new cases/newdeaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#VNM
VNM2020 %>% ggplot(aes(x=date,y= new_cases + new_deaths)) +
  geom_col()+
  labs(title = "*3.Vietnam 2020 new cases/deaths", x = "date", y = "new cases/newdeaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
VNM2021 %>% ggplot(aes(x=date,y= new_cases + new_deaths)) +
  geom_col()+
  labs(title = "*3.Vietnam 2021 new cases/deaths", x = "date", y = "new cases/newdeaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
VNM2022 %>% ggplot(aes(x=date,y= new_cases + new_deaths)) +
  geom_col()+
  labs(title = "*3.Vietnam 2022 new cases/deaths", x = "date", y = "new cases/newdeaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#4
#IDN
IDN2020end %>% ggplot(aes(x=date,y= new_cases)) +
  geom_col()+
  labs(title = "*4.Indonesia 2020 end new cases", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
IDN2021end %>% ggplot(aes(x=date,y= new_cases)) +
  geom_col()+
  labs(title = "*4.Indonesia 2021 end new cases", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#JPN
JPN2020end %>% ggplot(aes(x=date,y= new_cases)) +
  geom_col()+
  labs(title = "*4.Japan 2020 end new  cases", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
JPN2021end %>% ggplot(aes(x=date,y= new_cases)) +
  geom_col()+
  labs(title = "*4.Japan 2021 end new cases", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#VNM
VNM2020end %>% ggplot(aes(x=date,y= new_cases)) +
  geom_col()+
  labs(title = "*4.Vietnam 2020 end new cases", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
VNM2021end %>% ggplot(aes(x=date,y= new_cases)) +
  geom_col()+
  labs(title = "*4.Vietnam 2021 end new cases", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#5
#IDN
IDN2020end %>% ggplot(aes(x=date,y= new_deaths)) +
  geom_col()+
  labs(title = "*5.Indonesia 2020 end new deaths", x = "date", y = "new deaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
IDN2021end %>% ggplot(aes(x=date,y= new_deaths)) +
  geom_col()+
  labs(title = "*5.Indonesia 2021 end new deaths", x = "date", y = "new deaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#JPN
JPN2020end %>% ggplot(aes(x=date,y= new_deaths)) +
  geom_col()+
  labs(title = "*5.Japan 2020 end new deaths", x = "date", y = "new deaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
JPN2021end %>% ggplot(aes(x=date,y= new_deaths)) +
  geom_col()+
  labs(title = "*5.Japan 2021 end new deaths", x = "date", y = "new deaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#VNM
VNM2020end %>% ggplot(aes(x=date,y= new_deaths)) +
  geom_col()+
  labs(title = "*5.Vietnam 2020 end new deaths", x = "date", y = "new deaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
VNM2021end %>% ggplot(aes(x=date,y= new_deaths)) +
  geom_col()+
  labs(title = "*5.Vietnam 2021 end new deaths", x = "date", y = "new deaths") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#6
#IDN
IDN2020end %>% ggplot(aes(x=date,y= new_cases + new_deaths)) +
  geom_col()+
  labs(title = "*6.Indonesia 2020 end new cases/deaths", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
IDN2021end %>% ggplot(aes(x=date,y= new_cases + new_deaths)) +
  geom_col()+
  labs(title = "*6.Indonesia 2021 end new cases/deaths", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#JPN
JPN2020end %>% ggplot(aes(x=date,y= new_cases + new_deaths)) +
  geom_col()+
  labs(title = "*6.Japan 2020 end new  cases/deaths", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
JPN2021end %>% ggplot(aes(x=date,y= new_cases + new_deaths)) +
  geom_col()+
  labs(title = "*6.Japan 2021 end new cases/deaths", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#VNM
VNM2020end %>% ggplot(aes(x=date,y= new_cases + new_deaths)) +
  geom_col()+
  labs(title = "*6.Vietnam 2020 end new cases/deaths", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
VNM2021end %>% ggplot(aes(x=date,y= new_cases + new_deaths)) +
  geom_col()+
  labs(title = "*6.Vietnam 2021 end new cases/deaths", x = "date", y = "new cases") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#7
#cumulative data
IDN2020 %>% 
  mutate(new_cases=cumsum(new_cases),new_deaths=cumsum(new_deaths)) -> CUMIDN2020
IDN2021 %>% 
  mutate(new_cases=cumsum(new_cases),new_deaths=cumsum(new_deaths)) -> CUMIDN2021
IDN2022 %>% 
  mutate(new_cases=cumsum(new_cases),new_deaths=cumsum(new_deaths)) -> CUMIDN2022
JPN2020 %>% 
  mutate(new_cases=cumsum(new_cases),new_deaths=cumsum(new_deaths)) -> CUMJPN2020
JPN2021 %>% 
  mutate(new_cases=cumsum(new_cases),new_deaths=cumsum(new_deaths)) -> CUMJPN2021
JPN2022 %>% 
  mutate(new_cases=cumsum(new_cases),new_deaths=cumsum(new_deaths)) -> CUMJPN2022
VNM2020 %>% 
  mutate(new_cases=cumsum(new_cases),new_deaths=cumsum(new_deaths)) -> CUMVNM2020
VNM2021 %>% 
  mutate(new_cases=cumsum(new_cases),new_deaths=cumsum(new_deaths)) -> CUMVNM2021
VNM2022 %>% 
  mutate(new_cases=cumsum(new_cases),new_deaths=cumsum(new_deaths)) -> CUMVNM2022

CUMIDN2020 %>% ggplot(aes(x=date, y= new_cases, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*7.Indonesia 2020 cumulative new cases", x = "date", y = "new cases value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
CUMIDN2021 %>% ggplot(aes(x=date, y= new_cases, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*7.Indonesia 2021 cumulative new cases", x = "date", y = "new cases value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
CUMIDN2022 %>% ggplot(aes(x=date, y= new_cases, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*7.Indonesia 2022 cumulative new cases", x = "date", y = "new cases value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
CUMJPN2020 %>% ggplot(aes(x=date, y= new_cases, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*7.Japan 2020 cumulative new cases", x = "date", y = "new cases value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
CUMJPN2021 %>%ggplot(aes(x=date, y= new_cases, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*7.Japan 2021 cumulative new cases", x = "date", y = "new cases value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
CUMJPN2022 %>%ggplot(aes(x=date, y= new_cases, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*7.Japan 2022 cumulative new cases", x = "date", y = "new cases value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
CUMVNM2020 %>% ggplot(aes(x=date, y= new_cases, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*7.Vietnamese 2020 cumulative new cases", x = "date", y = "new cases value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
CUMVNM2021 %>% ggplot(aes(x=date, y= new_cases, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*7.Vietnamese 2021 cumulative new cases", x = "date", y = "new cases value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
CUMVNM2022 %>% ggplot(aes(x=date, y= new_cases, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*7.Vietnamese 2022 cumulative new cases", x = "date", y = "new cases value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
#8
CUMIDN2020 %>% ggplot(aes(x=date, y= new_deaths, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*8.Indonesia 2020 cumulative new deaths", x = "date", y = "new deaths value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
CUMIDN2021 %>% ggplot(aes(x=date, y= new_deaths, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*8.Indonesia 2021 cumulative new deaths", x = "date", y = "new deaths value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
CUMIDN2022 %>% ggplot(aes(x=date, y= new_deaths, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*8.Indonesia 2022 cumulative new deaths", x = "date", y = "new deaths value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
CUMJPN2020 %>% ggplot(aes(x=date, y= new_deaths, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*8.Japan 2020 cumulative new deaths", x = "date", y = "new deaths value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
CUMJPN2021 %>% ggplot(aes(x=date, y= new_deaths, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*8.Japan 2021 cumulative new deaths", x = "date", y = "new deaths value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
CUMJPN2022 %>% ggplot(aes(x=date, y= new_deaths, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*8.Japan 2022 cumulative new deaths", x = "date", y = "new deaths value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
CUMVNM2020 %>% ggplot(aes(x=date, y= new_deaths, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*8.Vietnamese 2020 cumulative new deaths", x = "date", y = "new deaths value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
CUMVNM2021 %>% ggplot(aes(x=date, y= new_deaths, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*8.Vietnamese 2021 cumulative new deaths", x = "date", y = "new deaths value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")
CUMVNM2022 %>% ggplot(aes(x=date, y= new_deaths, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "*8.Vietnamese 2022 cumulative new deaths", x = "date", y = "new deaths value") +
  scale_x_date(date_labels = "%d/%m",date_breaks = "weeks")