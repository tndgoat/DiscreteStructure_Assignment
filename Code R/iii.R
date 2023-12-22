# iii-1

indo.cases.No.Report = sum(indo$new_cases==0|is.na(indo$new_cases))
indo.deaths.No.Report = sum(indo$new_deaths==0|is.na(indo$new_deaths))
japan.cases.No.Report = sum(japan$new_cases==0|is.na(japan$new_cases))
japan.deaths.No.Report = sum(japan$new_deaths==0|is.na(japan$new_deaths))
vietnam.cases.No.Report = sum(vietnam$new_cases==0|is.na(vietnam$new_cases))
vietnam.deaths.No.Report = sum(vietnam$new_deaths==0|is.na(vietnam$new_deaths))

# iii-2

indo.cases.Report = subset (indo,indo$new_cases!=0)
indo.cases.Report.MIN = min (na.omit(indo.cases.Report$new_cases))
indo.count.day.cases.MIN = sum(na.omit(indo.cases.Report$new_cases==indo.cases.Report.MIN))

indo.deaths.Report = subset (indo,indo$new_deaths!=0)
indo.deaths.Report.MIN = min (na.omit(indo.deaths.Report$new_deaths))
indo.count.day.deaths.MIN = sum(na.omit(indo$new_deaths==indo.deaths.Report.MIN))

japan.cases.Report = subset (japan,japan$new_cases!=0)
japan.cases.Report.MIN = min (na.omit(japan.cases.Report$new_cases))
japan.count.day.cases.MIN = sum(na.omit(japan.cases.Report$new_cases==japan.cases.Report.MIN))

japan.deaths.Report = subset (japan,japan$new_deaths!=0)
japan.deaths.Report.MIN = min (na.omit(japan.deaths.Report$new_deaths))
japan.count.day.deaths.MIN = sum(na.omit(japan$new_deaths==japan.deaths.Report.MIN))

vietnam.cases.Report = subset (vietnam,vietnam$new_cases!=0)
vietnam.cases.Report.MIN = min (na.omit(vietnam.cases.Report$new_cases))
vietnam.count.day.cases.MIN = sum(na.omit(vietnam.cases.Report$new_cases==vietnam.cases.Report.MIN))

vietnam.deaths.Report = subset (vietnam,vietnam$new_deaths!=0)
vietnam.deaths.Report.MIN = min (na.omit(vietnam.deaths.Report$new_deaths))
vietnam.count.day.deaths.MIN = sum(na.omit(vietnam$new_deaths==vietnam.deaths.Report.MIN))
# iii-3

indo.cases.Report.MAX = max (na.omit(indo.cases.Report$new_cases))
indo.count.day.cases.MAX = sum(na.omit(indo.cases.Report$new_cases==indo.cases.Report.MAX))

indo.deaths.Report.MAX = max (na.omit(indo.deaths.Report$new_deaths))
indo.count.day.deaths.MAX = sum(na.omit(indo$new_deaths==indo.deaths.Report.MAX))

japan.cases.Report.MAX = max (na.omit(japan.cases.Report$new_cases))
japan.count.day.cases.MAX = sum(na.omit(japan.cases.Report$new_cases==japan.cases.Report.MAX))

japan.deaths.Report.MAX = max (na.omit(japan.deaths.Report$new_deaths))
japan.count.day.deaths.MAX = sum(na.omit(japan$new_deaths==japan.deaths.Report.MAX))

vietnam.cases.Report.MAX = max (na.omit(vietnam.cases.Report$new_cases))
vietnam.count.day.cases.MAX = sum(na.omit(vietnam.cases.Report$new_cases==vietnam.cases.Report.MAX))

vietnam.deaths.Report.MAX = max (na.omit(vietnam.deaths.Report$new_deaths))
vietnam.count.day.deaths.MAX = sum(na.omit(vietnam$new_deaths==vietnam.deaths.Report.MAX))

# iii-4

print ("No_Report")
data.frame(Countries = c("Indonesia","Japan","Vietnam"),
           Infections = c(indo.cases.No.Report,japan.cases.No.Report,vietnam.cases.No.Report),
           Deaths = c(indo.deaths.No.Report,japan.deaths.No.Report,vietnam.deaths.No.Report))

print ("Report_MIN")
data.frame(Countries = c("Indonesia","Japan","Vietnam"),
           Infections = c(indo.count.day.cases.MIN,japan.count.day.cases.MIN,vietnam.count.day.cases.MIN),
           Deaths = c(indo.count.day.deaths.MIN,japan.count.day.deaths.MIN,vietnam.count.day.deaths.MIN))

print ("Report_MAX")
data.frame(Countries = c("Indonesia","Japan","Vietnam"),
           Infections = c(indo.count.day.cases.MAX,japan.count.day.cases.MAX,vietnam.count.day.cases.MAX),
           Deaths = c(indo.count.day.deaths.MAX,japan.count.day.deaths.MAX,vietnam.count.day.deaths.MAX))
#iii-5-6-7-8
check.day.MIN = function(x){
  if (length(x)==1) print(1)
  else if (length(x)<1) print (0)
  else {
    min <- length(x)
    count <- 1
    for (i in 2:length(x)){
      if(as.numeric(x[i]-x[i-1])==1){
        count <- count + 1
      }
      else {
        if (count<min) {
          min <- count
        }
        count <- 1
      }
    }
    min
  }
}

check.day.MAX = function(x){
  if (length(x)==1) print(1)
  else if (length(x)<1) print (0)
  else {
    max <- 1
    count <- 1
    for (i in 2:length(x)){
      if(as.numeric(x[i]-x[i-1])==1){
        count <- count + 1
      }
      else {
        if (count>max) {
          max <- count
        }
        count <-1
      }
      if (count>max) {
        max <- count
      }
    }
    max
  }
}
#cau iii-5
indo.cases.unupdate = subset(indo,is.na(indo$new_cases))
japan.cases.unupdate = subset(japan,is.na(japan$new_cases))
vietnam.cases.unupdate = subset(vietnam,is.na(vietnam$new_cases))

indo.deaths.unupdate = subset(indo,is.na(indo$new_deaths))
japan.deaths.unupdate = subset(japan,is.na(japan$new_deaths))
vietnam.deaths.unupdate = subset(vietnam,is.na(vietnam$new_deaths))


check.day.MIN(indo.deaths.unupdate$date)
check.day.MIN(indo.cases.unupdate$date)
check.day.MIN(japan.deaths.unupdate$date)
check.day.MIN(japan.cases.unupdate$date)
check.day.MIN(vietnam.deaths.unupdate$date)
check.day.MIN(vietnam.cases.unupdate$date)

#cau iii-6


check.day.MAX(indo.deaths.unupdate$date)
check.day.MAX(indo.cases.unupdate$date)
check.day.MAX(japan.deaths.unupdate$date)
check.day.MAX(japan.cases.unupdate$date)
check.day.MAX(vietnam.deaths.unupdate$date)
check.day.MAX(vietnam.cases.unupdate$date)

#cau iii-7
indo.zero.cases = subset(indo,indo$new_cases=='0')
japan.zero.cases = subset(japan,japan$new_cases=='0')
vietnam.zero.cases = subset(vietnam,vietnam$new_cases=='0')

check.day.MIN(indo.zero.cases$date)
check.day.MIN(japan.zero.cases$date)
check.day.MIN(vietnam.zero.cases$date)

#cau iii-8

check.day.MAX(indo.zero.cases$date)
check.day.MAX(japan.zero.cases$date)
check.day.MAX(vietnam.zero.cases$date)