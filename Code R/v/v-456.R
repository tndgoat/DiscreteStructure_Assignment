library(base)
source("source.R")

NovDec2020 <- twenty %>% filter(month(date) == 11 | month(date) == 12)
NovDec2021 <- twenty1 %>% filter(month(date) == 11 | month(date) == 12)

#4 Biểu đồ thu thập nhiễm bệnh gồm 2 tháng cuối của năm

###2020
cases_NovDec2020 <- ggplot(data = NovDec2020, aes(x = date, y = new_cases))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New cases in November & December 2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  ) 
cases_NovDec2020 #print

#2021
cases_NovDec2021 <- ggplot(data = NovDec2021, aes(x = date, y = new_cases))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New cases in November & December 2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cases_NovDec2021 #print

ggsave("cases_NovDec2020.jpg", plot = cases_NovDec2020, device = NULL,path = here("plots", "4"), width = 3840, height = 1920, units = "px")
ggsave("cases_NovDec2021.jpg", plot = cases_NovDec2021, device = NULL,path = here("plots", "4"), width = 3840, height = 1920, units = "px")

#5 Biểu đồ thu thập tử vong gồm 2 tháng cuối của năm

#2020
dead_NovDec2020 <- ggplot(data = NovDec2020, aes(x = date, y = new_deaths))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New deaths in November & December 2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
dead_NovDec2020 #print

#2021
dead_NovDec2021 <- ggplot(data = NovDec2021, aes(x = date, y = new_deaths))+ 
  geom_line(color = "lightblue") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "New deaths in November & December 2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
dead_NovDec2021 #print

ggsave("dead_NovDec2020.jpg", plot = dead_NovDec2020, device = NULL,path = here("plots", "5"), width = 3840, height = 1920, units = "px")
ggsave("dead_NovDec2021.jpg", plot = dead_NovDec2021, device = NULL,path = here("plots", "5"), width = 3840, height = 1920, units = "px")

#6 Biểu đồ thu thập gồm nhiễm bệnh và tử vong gồm 2 tháng cuối của năm

#2021
CnD_NovDec2020 <- ggplot(NovDec2020, aes(date)) + 
  geom_line(aes(y = new_cases, colour = "new cases")) + 
  geom_line(aes(y = new_deaths, colour = "new deaths")) +
  labs(x = "", y = "Cases", title = "November & December 2020", color = "" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_y_continuous(
    breaks = seq (
      from = 0,
      to = 1000000,
      by = 50000
    )
  ) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
CnD_NovDec2020

#2021
CnD_NovDec2021 <- ggplot(NovDec2021, aes(date)) + 
  geom_line(aes(y = new_cases, colour = "new cases")) + 
  geom_line(aes(y = new_deaths, colour = "new deaths")) +
  labs(x = "", y = "Cases", title = "November & December 2021", color = "" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
CnD_NovDec2021

ggsave("CnD_NovDec2020.jpg", plot = CnD_NovDec2020, device = NULL,path = here("plots", "6"), width = 3840, height = 3840, units = "px")
ggsave("CnD_NovDec2021.jpg", plot = CnD_NovDec2021, device = NULL,path = here("plots", "6"), width = 3840, height = 3840, units = "px")


