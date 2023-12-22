pacman::p_load(
  rio,
  readr,
  here,
  ggplot2,
  dplyr,
  tidyr,
  extrafont,
  janitor,
  zoo,
  skimr,
  tidyverse,
  base,
  lubridate,
  scales,
  cowplot 
)
file_raw <- import("owid-covid-data.csv")
file_raw <- file_raw %>% mutate(date = lubridate::mdy(date))
countryName <- file_raw %>% select(continent, location)
countryName <- countryName %>% distinct()
countryName <- countryName %>% filter(continent != "") #loai bo cac hang nhu "World,..."

nCountry <- countryName %>% tabyl(continent)

#1
nCountry_plot <- ggplot(data = nCountry, aes(x = continent, y =  n)) +
  geom_col(fill = "#3c92de", width =  0.5) + theme_minimal() +
  labs(
    x = "Continent",
    y = "Number of countries",
    title = "Bieu do tan so tich luy quoc gia cho cac chau luc"
  ) + theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) + 
  scale_y_continuous(
    breaks = seq (
      from = 0, 
      to = 60,
      by = 10
    )
  )
#2
ratCountry_plot <- ggplot(data = nCountry, aes(x = continent, y =  percent)) +
  geom_col(fill = "#3c92de", width =  0.5) + theme_minimal() +
  labs(
    x = "Continent",
    y = "Percent",
    title = "Bieu do tan so tuong doi tich luy quoc gia cho cac chau luc"
  ) + theme(plot.title = element_text(size = 13, face = "italic"),
            axis.title.x = element_text(size = 10, face = "italic"),
            axis.title.y = element_text(size = 10, face = "italic"))+
  scale_y_continuous(
  labels = scales::percent                  
  )

ggsave("nCountry_plot.jpg", plot = nCountry_plot, path = here("plots", "iv"), width = 1920, height = 1920, units = "px")
ggsave("ratCountry_plot.jpg", plot = ratCountry_plot, path = here("plots", "iv"), width = 1920, height = 1920, units = "px")
#3
sevVie = file_raw %>% filter(location == "Vietnam")
sevVie = sevVie[(nrow(sevVie)-6):nrow(sevVie),4:6]
sevInd = file_raw %>% filter(location == "Indonesia")
sevInd = sevInd[(nrow(sevInd)-6):nrow(sevInd),4:6]
sevJap = file_raw %>% filter(location == "Japan")
sevJap = sevJap[(nrow(sevJap)-6):nrow(sevJap),4:6]
sevVie_Pcase<-ggplot(data = sevVie, aes(x = date, y = new_cases))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "Date", y = "Cases", title = "Vietnam's new deaths in the last week" )+
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
sevInd_Pcase<-ggplot(data = sevInd, aes(x = date, y = new_cases))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "Date", y = "Cases", title = "Indonesia's new deaths in the last week" )+
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
sevJap_Pcase<-ggplot(data = sevJap, aes(x = date, y = new_cases))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "Date", y = "Cases", title = "Japan's new deaths in the last week" )+
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
#4
sevVie_Pdead <- ggplot(data = sevVie, aes(x = date, y = new_deaths))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "Date", y = "Cases", title = "Vietnam's new deaths in the last week" )+
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
sevInd_Pdead <- ggplot(data = sevInd, aes(x = date, y = new_deaths))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "Date", y = "Cases", title = "Indonesia's new deaths in the last week" )+
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
sevJap_Pdead <- ggplot(data = sevJap, aes(x = date, y = new_deaths))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "Date", y = "Cases", title = "Japan's new deaths in the last week" )+
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))
ggsave("sevInd_Pdead.jpg", plot = sevInd_Pdead, path = here("plots", "iv"), width = 1920, height = 1920, units = "px")

#5
data_VietNam <- file_raw %>% filter(location == "Vietnam")
data_VietNam <- data_VietNam %>% mutate(new_deaths_dips = replace_na(new_deaths, 0))
data_Indo <- file_raw %>% filter(location == "Indonesia")
data_Indo <- data_Indo %>% mutate(new_deaths_dips = replace_na(new_deaths, 0))
data_Japan <- file_raw %>% filter(location == "Japan")
data_Japan <- data_Japan %>% mutate(new_deaths_dips = replace_na(new_deaths, 0))

newcasesVie <- ggplot(data = data_VietNam, aes(x = new_cases)) + labs(
  x = "New case",
  y = "Count",
  title = "Bieu do pho cua Viet Nam cho nhiem benh"
)+
  theme_minimal()+
  geom_histogram(bins = 20, fill = "#3c92de") +
  scale_y_continuous(
    breaks = seq (
      from = 0,
      to = 600,
      by = 20
    )
  ) +
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 55000,
      by = 10000
    )
  )


newdeathsVie <- ggplot(data = data_VietNam, aes(x = new_deaths_dips)) +labs(
  x = "New deaths",
  y = "Count",
  title = "Bieu do pho cua Viet Nam cho tu vong"
)+
  theme_minimal()+
  geom_histogram(bins = 20, fill = "#3c92de") +
  scale_y_continuous(
    breaks = seq (
      from = 0,
      to = 600,
      by = 20
    )
  ) +
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 1000,
      by = 100
    )
  )
newcasesIndo <- ggplot(data = data_Indo, aes(x = new_cases)) + labs(
  x = "New case",
  y = "Count",
  title = "Bieu do pho cua Indonesia cho nhiem benh"
)+
  theme_minimal()+
  geom_histogram(bins = 20, fill = "#3c92de") +
  scale_y_continuous(
    breaks = seq (
      from = 0,
      to = 600,
      by = 20
    )
  ) +
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 55000,
      by = 10000
    )
  )

newdeathsIndo <- ggplot(data = data_Indo, aes(x = new_deaths_dips)) + labs(
  x = "New deaths",
  y = "Count",
  title = "Bieu do pho cua Indonesia cho tu vong"
)+
  theme_minimal()+
  geom_histogram(bins = 20, fill = "#3c92de") +
  scale_y_continuous(
    breaks = seq (
      from = 0,
      to = 600,
      by = 20
    )
  ) +
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 1000,
      by = 100
    )
  )
newcasesJapan <- ggplot(data = data_Japan, aes(x = new_cases)) + labs(
  x = "New case",
  y = "Count",
  title = "Bieu do pho cua Japan cho nhiem benh"
)+
  theme_minimal()+
  geom_histogram(bins = 20, fill = "#3c92de") +
  scale_y_continuous(
    breaks = seq (
      from = 0,
      to = 600,
      by = 20
    )
  ) +
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 55000,
      by = 10000
    )
  )

newdeathsJapan <- ggplot(data = data_Japan, aes(x = new_deaths_dips)) + labs(
  x = "New deaths",
  y = "Count",
  title = "Bieu do pho cua Japan cho tu vong"
)+
  theme_minimal()+
  geom_histogram(bins = 20, fill = "#3c92de") +
  scale_y_continuous(
    breaks = seq (
      from = 0,
      to = 600,
      by = 20
    )
  ) +
  scale_x_continuous(
    breaks = seq(
      from = 0,
      to = 1000,
      by = 100
    )
  )
ggsave("newcasesVie.jpg", plot = newcasesVie, path = here("plots", "iv"), width = 3840, height = 1920, units = "px")
ggsave("newdeathsVie.jpg", plot = newdeathsVie, path = here("plots", "iv"), width = 3840, height = 1920, units = "px")
ggsave("newcasesIndo.jpg", plot = newcasesIndo, path = here("plots", "iv"), width = 3840, height = 1920, units = "px")
ggsave("newdeathsIndo.jpg", plot = newdeathsIndo, path = here("plots", "iv"), width = 3840, height = 1920, units = "px")
ggsave("newcasesJapan.jpg", plot = newcasesJapan, path = here("plots", "iv"), width = 3840, height = 1920, units = "px")
ggsave("newdeathsJapan.jpg", plot = newdeathsJapan, path = here("plots", "iv"), width = 3840, height = 1920, units = "px")



