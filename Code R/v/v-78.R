library(base)
source("source.R")



#7 Biểu đồ thu thập nhiễm bệnh tích lũy cho từng tháng
#01.2020
cumsumCases_0120 <- ggplot(data = Jan2020, aes(x = date, y = cumulative_cases))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative cases 01/2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumCases_0120 #print

#02.2020
cumsumCases_0220 <- ggplot(data = Feb2020, aes(x = date, y = cumulative_cases))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative cases 02/2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumCases_0220 #print

#04.2020
cumsumCases_0420 <- ggplot(data = Apr2020, aes(x = date, y = cumulative_cases))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative cases 04/2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))+
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumCases_0420 #print

#10.2020
cumsumCases_1020 <- ggplot(data = Oct2020, aes(x = date, y = cumulative_cases))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative cases 10/2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumCases_1020 #print

ggsave("cumsumCases_0120.jpg", plot = cumsumCases_0120, device = NULL,path = here("plots", "7"), width = 1920, height = 1920, units = "px")
ggsave("cumsumCases_0220.jpg", plot = cumsumCases_0220, device = NULL,path = here("plots", "7"), width = 1920, height = 1920, units = "px")
ggsave("cumsumCases_0420.jpg", plot = cumsumCases_0420, device = NULL,path = here("plots", "7"), width = 1920, height = 1920, units = "px")
ggsave("cumsumCases_1020.jpg", plot = cumsumCases_1020, device = NULL,path = here("plots", "7"), width = 1920, height = 1920, units = "px")
############-------2021---------------
#01.2021
cumsumCases_0121 <- ggplot(data = Jan2021, aes(x = date, y = cumulative_cases))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative cases 01/2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumCases_0121 #print

#02.2021
cumsumCases_0221 <- ggplot(data = Feb2021, aes(x = date, y = cumulative_cases))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative cases 02/2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumCases_0221 #print

#04.2021
cumsumCases_0421 <- ggplot(data = Apr2021, aes(x = date, y = cumulative_cases))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative cases 04/2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))+
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumCases_0421 #print

#10.2021
cumsumCases_1021 <- ggplot(data = Oct2021, aes(x = date, y = cumulative_cases))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative cases 10/2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumCases_1021 #print

ggsave("cumsumCases_0121.jpg", plot = cumsumCases_0121, device = NULL,path = here("plots", "7"), width = 1920, height = 1920, units = "px")
ggsave("cumsumCases_0221.jpg", plot = cumsumCases_0221, device = NULL,path = here("plots", "7"), width = 1920, height = 1920, units = "px")
ggsave("cumsumCases_0421.jpg", plot = cumsumCases_0421, device = NULL,path = here("plots", "7"), width = 1920, height = 1920, units = "px")
ggsave("cumsumCases_1021.jpg", plot = cumsumCases_1021, device = NULL,path = here("plots", "7"), width = 1920, height = 1920, units = "px")

############-------2022---------------
#01.2022
cumsumCases_0122 <- ggplot(data = Jan2022, aes(x = date, y = cumulative_cases))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative cases 01/2022" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumCases_0122 #print

#02.2022
cumsumCases_0222 <- ggplot(data = Feb2022, aes(x = date, y = cumulative_cases))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative cases 02/2022" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumCases_0222 #print

ggsave("cumsumCases_0122.jpg", plot = cumsumCases_0122, device = NULL,path = here("plots", "7"), width = 1920, height = 1920, units = "px")
ggsave("cumsumCases_0222.jpg", plot = cumsumCases_0222, device = NULL,path = here("plots", "7"), width = 1920, height = 1920, units = "px")


#8 Biểu đồ thu thập tu vong tích lũy cho từng tháng
#01.2020
cumsumDeaths_0120 <- ggplot(data = Jan2020, aes(x = date, y = cumulative_deaths))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative deaths 01/2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumDeaths_0120 #print

#02.2020
cumsumDeaths_0220 <- ggplot(data = Feb2020, aes(x = date, y = cumulative_deaths))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative deaths 02/2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumDeaths_0220 #print

#04.2020
cumsumDeaths_0420 <- ggplot(data = Apr2020, aes(x = date, y = cumulative_deaths))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative deaths 04/2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))+
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumDeaths_0420 #print

#10.2020
cumsumDeaths_1020 <- ggplot(data = Oct2020, aes(x = date, y = cumulative_deaths))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative deaths 10/2020" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumDeaths_1020 #print

ggsave("cumsumDeaths_0120.jpg", plot = cumsumDeaths_0120, device = NULL,path = here("plots", "8"), width = 1920, height = 1920, units = "px")
ggsave("cumsumDeaths_0220.jpg", plot = cumsumDeaths_0220, device = NULL,path = here("plots", "8"), width = 1920, height = 1920, units = "px")
ggsave("cumsumDeaths_0420.jpg", plot = cumsumDeaths_0420, device = NULL,path = here("plots", "8"), width = 1920, height = 1920, units = "px")
ggsave("cumsumDeaths_1020.jpg", plot = cumsumDeaths_1020, device = NULL,path = here("plots", "8"), width = 1920, height = 1920, units = "px")
############-------2021---------------
#01.2021
cumsumDeaths_0121 <- ggplot(data = Jan2021, aes(x = date, y = cumulative_deaths))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative deaths 01/2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumDeaths_0121 #print

#02.2021
cumsumDeaths_0221 <- ggplot(data = Feb2021, aes(x = date, y = cumulative_deaths))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative deaths 02/2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumDeaths_0221 #print

#04.2021
cumsumDeaths_0421 <- ggplot(data = Apr2021, aes(x = date, y = cumulative_deaths))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative deaths 04/2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic"))+
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumDeaths_0421 #print

#10.2021
cumsumDeaths_1021 <- ggplot(data = Oct2021, aes(x = date, y = cumulative_deaths))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative deaths 10/2021" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumDeaths_1021 #print

ggsave("cumsumDeaths_0121.jpg", plot = cumsumDeaths_0121, device = NULL,path = here("plots", "8"), width = 1920, height = 1920, units = "px")
ggsave("cumsumDeaths_0221.jpg", plot = cumsumDeaths_0221, device = NULL,path = here("plots", "8"), width = 1920, height = 1920, units = "px")
ggsave("cumsumDeaths_0421.jpg", plot = cumsumDeaths_0421, device = NULL,path = here("plots", "8"), width = 1920, height = 1920, units = "px")
ggsave("cumsumDeaths_1021.jpg", plot = cumsumDeaths_1021, device = NULL,path = here("plots", "8"), width = 1920, height = 1920, units = "px")

############-------2022---------------
#01.2022
cumsumDeaths_0122 <- ggplot(data = Jan2022, aes(x = date, y = cumulative_deaths))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative deaths 01/2022" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumDeaths_0122 #print

#02.2022
cumsumDeaths_0222 <- ggplot(data = Feb2022, aes(x = date, y = cumulative_deaths))+ 
  geom_line(color = "#3897e0") + 
  geom_point(size = 1, color = "#0871c2") +
  labs(x = "", y = "Cases", title = "Cumulative deaths 02/2022" ) +
  theme_minimal()+
  theme(plot.title = element_text(size = 13, face = "italic"),
        axis.title.x = element_text(size = 10, face = "italic"),
        axis.title.y = element_text(size = 10, face = "italic")) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%b %d"
  )
cumsumDeaths_0222 #print

ggsave("cumsumDeaths_0122.jpg", plot = cumsumDeaths_0122, device = NULL,path = here("plots", "8"), width = 1920, height = 1920, units = "px")
ggsave("cumsumDeaths_0222.jpg", plot = cumsumDeaths_0222, device = NULL,path = here("plots", "8"), width = 1920, height = 1920, units = "px")
                
