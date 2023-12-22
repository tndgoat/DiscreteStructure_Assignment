data = read.csv("owid-covid-data.csv")
data$date = as.Date(data$date, format = "%m/%d/%Y")
data$new_cases = abs(data$new_cases)
data$new_deaths = abs(data$new_deaths)

indo = subset(data, data$location == "Indonesia")
japan = subset(data, data$location == "Japan")
vietnam = subset(data, data$location == "Vietnam")

# ii-1
indo.cases.MIN = min(na.omit(indo$new_cases))
indo.cases.MAX = max(na.omit(indo$new_cases))
japan.cases.MIN = min(na.omit(japan$new_cases))
japan.cases.MAX = max(na.omit(japan$new_cases))
vietnam.cases.MIN = min(na.omit(vietnam$new_cases))
vietnam.cases.MAX = max(na.omit(vietnam$new_cases))

indo.deaths.MIN = min(na.omit(indo$new_deaths))
indo.deaths.MAX = max(na.omit(indo$new_deaths))
japan.deaths.MIN = min(na.omit(japan$new_deaths))
japan.deaths.MAX = max(na.omit(japan$new_deaths))
vietnam.deaths.MIN = min(na.omit(vietnam$new_deaths))
vietnam.deaths.MAX = max(na.omit(vietnam$new_deaths))


# ii-2
indo.cases.Q1 = quantile(na.omit(indo$new_cases),c(0.25))
indo.cases.Q2 = quantile(na.omit(indo$new_cases),c(0.5))
indo.cases.Q3 = quantile(na.omit(indo$new_cases),c(0.75))
indo.deaths.Q1 = quantile(na.omit(indo$new_deaths),c(0.25))
indo.deaths.Q2 = quantile(na.omit(indo$new_deaths),c(0.5))
indo.deaths.Q3 = quantile(na.omit(indo$new_deaths),c(0.75))

japan.cases.Q1 = quantile(na.omit(japan$new_cases),c(0.25))
japan.cases.Q2 = quantile(na.omit(japan$new_cases),c(0.5))
japan.cases.Q3 = quantile(na.omit(japan$new_cases),c(0.75))
japan.deaths.Q1 = quantile(na.omit(japan$new_deaths),c(0.25))
japan.deaths.Q2 = quantile(na.omit(japan$new_deaths),c(0.5))
japan.deaths.Q3 = quantile(na.omit(japan$new_deaths),c(0.75))

vietnam.cases.Q1 = quantile(na.omit(vietnam$new_cases),c(0.25))
vietnam.cases.Q2 = quantile(na.omit(vietnam$new_cases),c(0.5))
vietnam.cases.Q3 = quantile(na.omit(vietnam$new_cases),c(0.75))
vietnam.deaths.Q1 = quantile(na.omit(vietnam$new_deaths),c(0.25))
vietnam.deaths.Q2 = quantile(na.omit(vietnam$new_deaths),c(0.5))
vietnam.deaths.Q3 = quantile(na.omit(vietnam$new_deaths),c(0.75))

# ii-3
indo.cases.avg = mean(na.omit(indo$new_cases))
indo.deaths.avg = mean(na.omit(indo$new_deaths))

japan.cases.avg = mean(na.omit(japan$new_cases))
japan.deaths.avg = mean(na.omit(japan$new_deaths))

vietnam.cases.avg = mean(na.omit(vietnam$new_cases))
vietnam.deaths.avg = mean(na.omit(vietnam$new_deaths))

# ii-4

indo.cases.std = sd(na.omit(indo$new_cases))
indo.deaths.std = sd(na.omit(indo$new_deaths))

japan.cases.std = sd(na.omit(japan$new_cases))
japan.deaths.std = sd(na.omit(japan$new_deaths))

vietnam.cases.std = sd(na.omit(vietnam$new_cases))
vietnam.deaths.std = sd(na.omit(vietnam$new_deaths))

# ii-5
indo.cases.IQR = indo.cases.Q3 - indo.cases.Q1

indo.cases.outlier = sum(na.omit(indo$new_cases < indo.cases.Q1 - 1.5*indo.cases.IQR 
                                 | indo$new_cases > indo.cases.Q3 + 1.5*indo.cases.IQR))

indo.deaths.IQR = indo.deaths.Q3 - indo.deaths.Q1

indo.deaths.outlier = sum(na.omit(indo$new_deaths < indo.deaths.Q1 - 1.5*indo.deaths.IQR 
                                  | indo$new_deaths > indo.deaths.Q3 + 1.5*indo.deaths.IQR))

japan.cases.IQR = japan.cases.Q3 - japan.cases.Q1

japan.cases.outlier = sum(na.omit(japan$new_cases < japan.cases.Q1 - 1.5*japan.cases.IQR 
                                  | japan$new_cases > japan.cases.Q3 + 1.5*japan.cases.IQR))

japan.deaths.IQR = japan.deaths.Q3 - japan.deaths.Q1

japan.deaths.outlier = sum(na.omit(japan$new_deaths < japan.deaths.Q1 - 1.5*japan.deaths.IQR 
                                   | japan$new_deaths > japan.deaths.Q3 + 1.5*japan.deaths.IQR))

vietnam.cases.IQR = vietnam.cases.Q3 - vietnam.cases.Q1

vietnam.cases.outlier = 
  sum(na.omit(vietnam$new_cases < vietnam.cases.Q1 - 1.5*vietnam.cases.IQR 
              | vietnam$new_cases > vietnam.cases.Q3 + 1.5*vietnam.cases.IQR))

vietnam.deaths.IQR = vietnam.deaths.Q3 - vietnam.deaths.Q1

vietnam.deaths.outlier = 
  sum(na.omit(vietnam$new_deaths < vietnam.deaths.Q1 - 1.5*vietnam.deaths.IQR
              | vietnam$new_deaths > vietnam.deaths.Q3 + 1.5*vietnam.deaths.IQR))

# ii-6
print ("New_Cases(infections)")
data.frame(Country = c("Indonesia","Japan","Vietnam"),
           Min = c(indo.cases.MIN,japan.cases.MIN,vietnam.cases.MIN),
           Q1 = c(indo.cases.Q1,japan.cases.Q1,vietnam.cases.Q1),
           Q2 = c(indo.cases.Q2,japan.cases.Q2,vietnam.cases.Q2),
           Q3 = c(indo.cases.Q3,japan.cases.Q3,vietnam.cases.Q3),
           Max = c(indo.cases.MAX,japan.cases.MAX,vietnam.cases.MAX),
           Avg = c(indo.cases.avg,japan.cases.avg,vietnam.cases.avg),
           Std = c(indo.cases.std,japan.cases.std,vietnam.cases.std),
           Outlier = c(indo.cases.outlier,japan.cases.outlier,vietnam.cases.outlier))

print ("New_Deaths(deaths)")
data.frame(Country = c("Indonesia","Japan","Vietnam"),
           Min = c(indo.deaths.MIN,japan.deaths.MIN,vietnam.deaths.MIN),
           Q1 = c(indo.deaths.Q1,japan.deaths.Q1,vietnam.deaths.Q1),
           Q2 = c(indo.deaths.Q2,japan.deaths.Q2,vietnam.deaths.Q2),
           Q3 = c(indo.deaths.Q3,japan.deaths.Q3,vietnam.deaths.Q3),
           Max = c(indo.deaths.MAX,japan.deaths.MAX,vietnam.deaths.MAX),
           Avg = c(indo.deaths.avg,japan.deaths.avg,vietnam.deaths.avg),
           Std = c(indo.deaths.std,japan.deaths.std,vietnam.deaths.std),
           Outlier = c(indo.deaths.outlier,japan.deaths.outlier,vietnam.deaths.outlier))


# ii-7
boxplot(indo$new_cases, ylab="New_Cases", main="Indonesia New Cases Boxplot")
boxplot(indo$new_deaths, ylab="New_Deaths", main="Indonesia New Deaths Boxplot")
boxplot(japan$new_cases, ylab="New_Cases", main="Japan New Cases Boxplot")
boxplot(japan$new_deaths, ylab="New_Deaths", main="Japan New Deaths Boxplot")
boxplot(vietnam$new_cases, ylab="New_Cases", main="Vietnam New Cases Boxplot")
boxplot(vietnam$new_deaths, ylab="New_Deaths", main="Vietnam New Deaths Boxplot")
