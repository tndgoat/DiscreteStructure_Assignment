pacman::p_load(
  rio,
  readr,
  here,
  ggplot2,
  dplyr,
  tidyr,
  janitor,
  zoo,
  tidyverse,
  base,
  lubridate,
  scales,
  cowplot #ghep bieu do
)

file_raw <- import("owid-covid-data.csv")
file_raw <- file_raw %>% mutate(date = lubridate::mdy(date))
world <- file_raw %>% filter(iso_code == "OWID_WRL")
world <- world %>% mutate(
  cumulative_cases = cumsum(new_cases)) %>%
  mutate(
    cumulative_deaths = cumsum(new_deaths)
  )
twenty <- world %>% filter(year(date) == 2020)
twenty1 <- world %>% filter(year(date) == 2021)
twenty2 <- world %>% filter(year(date) == 2022)


#Biểu đồ thu thập nhiễm bệnh cho từng tháng (1, 2, 4 ,10)
#2020
Jan2020 <- twenty %>% filter(month(date) == 1)
Feb2020 <- twenty %>% filter(month(date) == 2)
Apr2020 <- twenty %>% filter(month(date) == 4)
Oct2020 <- twenty %>% filter(month(date) == 10)
#2021
Jan2021 <- twenty1 %>% filter(month(date) == 1)
Feb2021 <- twenty1 %>% filter(month(date) == 2)
Apr2021 <- twenty1 %>% filter(month(date) == 4)
Oct2021 <- twenty1 %>% filter(month(date) == 10)
#2022
Jan2022 <- twenty2 %>% filter(month(date) == 1)
Feb2022 <- twenty2 %>% filter(month(date) == 2)

#Ve bieu do

options(scipen = 999) #tat ky hieu khoa hoc