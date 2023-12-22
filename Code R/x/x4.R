#Vietnam
k = 20000
x4_Vietnam = mydata %>% filter(mydata$location == "Vietnam")
x4_Vietnam = x4_Vietnam[,4:6]
i = 1
while (i < nrow(x4_Vietnam)){
  temp = c()
  if (x4_Vietnam[i,2] >= k){
    
    temp = cbind(temp, x4_Vietnam[i,1], ' - ')
    while (x4_Vietnam[i,2] >= k){
      i = i + 1
      if (i > nrow(x4_Vietnam)) break
    }
    temp = cbind(temp, x4_Vietnam[i-1,1])
    cat(temp, '\n')
  }
  else i = i+1
}
 #Tuong tu voi Indonesia va Japan
