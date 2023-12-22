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
  cowplot 
)

file_raw <- import("owid-covid-data.csv")
file_raw <- file_raw %>% mutate(date = lubridate::mdy(date))
world <- file_raw %>% filter(iso_code == "OWID_WRL")
twenty <- world %>% filter(year(date) == 2020)
twenty1 <- world %>% filter(year(date) == 2021)
twenty2 <- world %>% filter(year(date) == 2022)

#Bieu do thu thap nhiem benh cho tung thang (1, 2, 4 ,10)
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
###01.2020
cases_0120 <- ggplot(data = Jan2020, aes(x = date, y = new_cases))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New cases in January 2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
cases_0120 #print

###02.2020
cases_0220 <- ggplot(data = Feb2020, aes(x = date, y = new_cases))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New cases in February 2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
cases_0220 #print

###04.2020
cases_0420 <- ggplot(data = Apr2020, aes(x = date, y = new_cases))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New cases in April 2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
cases_0420 #print

###10.2020
cases_1020 <- ggplot(data = Oct2020, aes(x = date, y = new_cases))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New cases in October 2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
cases_1020 #print
#################-------------2021-----------########
###01.2021
cases_0121 <- ggplot(data = Jan2021, aes(x = date, y = new_cases))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New cases in January 2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
cases_0121 #print

###02.2021
cases_0221 <- ggplot(data = Feb2021, aes(x = date, y = new_cases))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New cases in February 2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
cases_0221 #print

###04.2021
cases_0421 <- ggplot(data = Apr2021, aes(x = date, y = new_cases))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New cases in April 2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
cases_0421 #print

###10.2021
cases_1021 <- ggplot(data = Oct2021, aes(x = date, y = new_cases))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New cases in October 2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
cases_1021 #print

#################-------------2022-----------########
###01.2022
cases_0122 <- ggplot(data = Jan2022, aes(x = date, y = new_cases))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New cases in January 2022" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
cases_0122 #print

###02.2022
cases_0222 <- ggplot(data = Feb2022, aes(x = date, y = new_cases))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New cases in February 2022" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "4 days",
    date_labels = "%b %d"
  ) 
cases_0222 #print


##Ghep bieu do
cases_plots2020 <- cowplot::plot_grid(cases_0120, cases_0220, cases_0420, cases_1020)
cases_plots2021 <- cowplot::plot_grid(cases_0121, cases_0221, cases_0421, cases_1021)
cases_plots2022 <- cowplot::plot_grid(cases_0122, cases_0222)
cases_plots2020
cases_plots2021
cases_plots2022

#save image
# ggsave("cases_0120.jpg", plot = cases_0120, device = NULL, path = here("plots"), dpi = 300)
# ggsave("cases_0220.jpg", plot = cases_0220, device = NULL, path = here("plots"), dpi = 300)
# ggsave("cases_0420.jpg", plot = cases_0420, device = NULL, path = here("plots"), dpi = 300)
# ggsave("cases_1020.jpg", plot = cases_1020, device = NULL, path = here("plots"), dpi = 300)
# ggsave("cases_0121.jpg", plot = cases_0121, device = NULL, path = here("plots"), dpi = 300)
# ggsave("cases_0221.jpg", plot = cases_0221, device = NULL, path = here("plots"), dpi = 300)
# ggsave("cases_0421.jpg", plot = cases_0421, device = NULL, path = here("plots"), dpi = 300)
# ggsave("cases_1021.jpg", plot = cases_1021, device = NULL, path = here("plots"), dpi = 300)
# ggsave("cases_0122.jpg", plot = cases_0122, device = NULL, path = here("plots"), dpi = 300)
# ggsave("cases_0222.jpg", plot = cases_0222, device = NULL, path = here("plots"), dpi = 300)

#Biểu đồ thu thập tử vong cho từng tháng

###01.2020
dead_0120 <- ggplot(data = Jan2020, aes(x = date, y = new_deaths))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New deaths in January 2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
dead_0120 #print

###02.2020
dead_0220 <- ggplot(data = Feb2020, aes(x = date, y = new_deaths))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New deaths in February 2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
dead_0220 #print

###04.2020
dead_0420 <- ggplot(data = Apr2020, aes(x = date, y = new_deaths))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New deaths in April 2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
dead_0420 #print

###10.2020
dead_1020 <- ggplot(data = Oct2020, aes(x = date, y = new_deaths))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New deaths in October 2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
dead_1020 #print
#################-------------2021-----------########
###01.2021
dead_0121 <- ggplot(data = Jan2021, aes(x = date, y = new_deaths))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New deaths in January 2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
dead_0121 #print

###02.2021
dead_0221 <- ggplot(data = Feb2021, aes(x = date, y = new_deaths))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New deaths in February 2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
dead_0221 #print

###04.2021
dead_0421 <- ggplot(data = Apr2021, aes(x = date, y = new_deaths))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New deaths in April 2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
dead_0421 #print

###10.2021
dead_1021 <- ggplot(data = Oct2021, aes(x = date, y = new_deaths))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New deaths in October 2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
dead_1021 #print

#################-------------2022-----------########
###01.2022
dead_0122 <- ggplot(data = Jan2022, aes(x = date, y = new_deaths))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New deaths in January 2022" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
dead_0122 #print

###02.2022
dead_0222 <- ggplot(data = Feb2022, aes(x = date, y = new_deaths))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New deaths in February 2022" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "4 days",
    date_labels = "%b %d"
  ) 
dead_0222 #print

# ggsave("dead_0120.jpg", plot = dead_0120, device = NULL, path = here("plots"), dpi = 300)
# ggsave("dead_0220.jpg", plot = dead_0220, device = NULL, path = here("plots"), dpi = 300)
# ggsave("dead_0420.jpg", plot = dead_0420, device = NULL, path = here("plots"), dpi = 300)
# ggsave("dead_1020.jpg", plot = dead_1020, device = NULL, path = here("plots"), dpi = 300)
# ggsave("dead_0121.jpg", plot = dead_0121, device = NULL, path = here("plots"), dpi = 300)
# ggsave("dead_0221.jpg", plot = dead_0221, device = NULL, path = here("plots"), dpi = 300)
# ggsave("dead_0421.jpg", plot = dead_0421, device = NULL, path = here("plots"), dpi = 300)
# ggsave("dead_1021.jpg", plot = dead_1021, device = NULL, path = here("plots"), dpi = 300)
# ggsave("dead_0122.jpg", plot = dead_0122, device = NULL, path = here("plots"), dpi = 300)
# ggsave("dead_0222.jpg", plot = dead_0222, device = NULL, path = here("plots"), dpi = 300)
# #gop bieu do
dead_plots2020 <- cowplot::plot_grid(dead_0120, dead_0220, dead_0420, dead_1020)
dead_plots2021 <- cowplot::plot_grid(dead_0121, dead_0221, dead_0421, dead_1021)
dead_plots2022 <- cowplot::plot_grid(dead_0122, dead_0222)

#Biểu đồ thu thập gồm nhiễm bệnh và tử vong cho từng tháng
#01/2020
CnD_0120 <- ggplot(Jan2020, aes(date)) + 
  geom_line(aes(y = new_cases, colour = "new cases")) + 
  geom_line(aes(y = new_deaths, colour = "new deaths")) +
  labs(x = "", y = "Cases", title = "January 2020", color = "" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_y_continuous(
    breaks = seq(
      from = 0,
      to = 3000,
      by = 100
    )
  )
CnD_0120

#02/2020
CnD_0220 <- ggplot(Feb2020, aes(date)) + 
  geom_line(aes(y = new_cases, colour = "new cases")) + 
  geom_line(aes(y = new_deaths, colour = "new deaths")) +
  labs(x = "", y = "Cases", title = "February 2020", color = "" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_y_continuous(
    breaks = seq(
      from = 0,
      to = 15000,
      by = 1000
    )
  ) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
CnD_0220

#04/2020
CnD_0420 <- ggplot(Apr2020, aes(date)) + 
  geom_line(aes(y = new_cases, colour = "new cases")) + 
  geom_line(aes(y = new_deaths, colour = "new deaths")) +
  labs(x = "", y = "Cases", title = "April 2020", color = "" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_y_continuous(
    breaks = seq(
      from = 0,
      to = 150000,
      by = 5000
    )
  ) +
  scale_x_date(
    date_breaks = "7 days",
    date_labels = "%b %d"
  )
CnD_0420

#10/2020
CnD_1020 <- ggplot(Oct2020, aes(date)) + 
  geom_line(aes(y = new_cases, colour = "new cases")) + 
  geom_line(aes(y = new_deaths, colour = "new deaths")) +
  labs(x = "", y = "Cases", title = "October 2020", color = "" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_y_continuous(
    breaks = seq(
      from = 0,
      to = 600000,
      by = 25000
    )
  ) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
CnD_1020

# ggsave("CnD_0220.jpg", plot = CnD_0220, device = NULL,path = here("plots", "casesNdeaths"), width = 1920, height = 1920, units = "px")
# ggsave("CnD_0420.jpg", plot = CnD_0420, device = NULL,path = here("plots", "casesNdeaths"), width = 1920, height = 1920, units = "px")
# ggsave("CnD_1020.jpg", plot = CnD_1020, device = NULL,path = here("plots", "casesNdeaths"), width = 1920, height = 1920, units = "px")

#---------------------------2021--------------------------------
#01/21
CnD_0121 <- ggplot(Jan2021, aes(date)) + 
  geom_line(aes(y = new_cases, colour = "new cases")) + 
  geom_line(aes(y = new_deaths, colour = "new deaths")) +
  labs(x = "", y = "Cases", title = "January 2021", color = "" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_y_continuous(
    breaks = seq(
      from = 0,
      to = 1000000,
      by = 50000
    )
  )
CnD_0121

#02/21
CnD_0221 <- ggplot(Feb2021, aes(date)) + 
  geom_line(aes(y = new_cases, colour = "new cases")) + 
  geom_line(aes(y = new_deaths, colour = "new deaths")) +
  labs(x = "", y = "Cases", title = "February 2021", color = "" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_y_continuous(
    breaks = seq(
      from = 0,
      to = 1000000,
      by = 25000
    )
  )
CnD_0221

#04/21
CnD_0421 <- ggplot(Apr2021, aes(date)) + 
  geom_line(aes(y = new_cases, colour = "new cases")) + 
  geom_line(aes(y = new_deaths, colour = "new deaths")) +
  labs(x = "", y = "Cases", title = "April 2021", color = "" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_y_continuous(
    breaks = seq(
      from = 0,
      to = 1000000,
      by = 50000
    )
  )
CnD_0421

#10/21
CnD_1021 <- ggplot(Oct2021, aes(date)) + 
  geom_line(aes(y = new_cases, colour = "new cases")) + 
  geom_line(aes(y = new_deaths, colour = "new deaths")) +
  labs(x = "", y = "Cases", title = "October 2021", color = "" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_y_continuous(
    breaks = seq(
      from = 0,
      to = 1000000,
      by = 25000
    )
  )
CnD_1021

#----------------2022-----------

#01/22
CnD_0122 <- ggplot(Jan2022, aes(date)) + 
  geom_line(aes(y = new_cases, colour = "new cases")) + 
  geom_line(aes(y = new_deaths, colour = "new deaths")) +
  labs(x = "", y = "Cases", title = "Jan 2022", color = "" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_y_continuous(
    breaks = seq(
      from = 0,
      to = 10000000,
      by = 200000
    )
  )
CnD_0122

#02/22
CnD_0222 <- ggplot(Feb2022, aes(date)) + 
  geom_line(aes(y = new_cases, colour = "new cases")) + 
  geom_line(aes(y = new_deaths, colour = "new deaths")) +
  labs(x = "", y = "Cases", title = "February 2022", color = "" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_y_continuous(
    breaks = seq(
      from = 0,
      to = 10000000,
      by = 200000
    )
  ) + 
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
CnD_0222

ggsave("CnD_0121.jpg", plot = CnD_0121, device = NULL,path = here("plots", "casesNdeaths"), width = 1920, height = 1920, units = "px")
ggsave("CnD_0221.jpg", plot = CnD_0221, device = NULL,path = here("plots", "casesNdeaths"), width = 1920, height = 1920, units = "px")
ggsave("CnD_0421.jpg", plot = CnD_0421, device = NULL,path = here("plots", "casesNdeaths"), width = 1920, height = 1920, units = "px")
ggsave("CnD_1021.jpg", plot = CnD_1021, device = NULL,path = here("plots", "casesNdeaths"), width = 1920, height = 1920, units = "px")
ggsave("CnD_0122.jpg", plot = CnD_0122, device = NULL,path = here("plots", "casesNdeaths"), width = 1920, height = 1920, units = "px")
ggsave("CnD_0222.jpg", plot = CnD_0222, device = NULL,path = here("plots", "casesNdeaths"), width = 1920, height = 1920, units = "px")


