#i0
data = read.csv("owid-covid-data.csv")

#i1
temp = data %>% select(date)
rDate = strptime(temp$date, format = "%m/%d/%Y")
year = unique(format(rDate, "%Y"))
cat(year)

#i2
i2 = cbind(unique(data %>% select(iso_code)),
           unique(data %>% select(location)))
i2 = i2[1:10,]
colnames(i2) = c("iso_code:", "Country")
i2[11,] = c("Count", 10)
rownames(i2) = NULL

#i3
i3 = cbind(unique(data %>% select(continent) %>% filter(str_length(continent) != 0)))
i3 = arrange(i3, continent)
i3 = cbind(i3, rbind("Chau Phi","Chau A","Chau Au","Chau Bac Mi","Chau Dai Duong","Chau Nam Mi"))
colnames(i3) = c("Continent:", nrow(i3))

#i4
i4 = select(i3, -2)
x = table(data$continent)
i4 = cbind(i4, rbind(x[[2]],x[[3]],x[[4]],x[[5]],x[[6]],x[[7]]))
colnames(i4) = c("Continent:", "Observations")
a = colSums(i4[2])
i4[7,] = c("Tong:", a)

#i5
i5 = cbind(unique(data %>% select(iso_code)), unique(data %>% select(location)))
i5 = i5[(nrow(i5)-9):nrow(i5),]
y = table(data$iso_code)
for (i in 1:10){
  for (k in 1:length(y)){
    if (rownames(y)[k] == i5[i,1]) i5[i,2] = y[[k]]
  }
}
colnames(i5) = c("iso_code", "Observations")
i5[[2]] = as.numeric(i5[[2]])
a = colSums(i5[2])
i5[11,] = c("Tong:",a)
rownames(i5) = NULL

#i6
i6 = table(data$continent)
for (i in 1:length(i6)){
  if (i6[[i]] == min(i6)) print(i6[i])
}

#i7
i6 = table(data$continent)
for (i in 1:length(i6)){
  if (i6[[i]] == max(i6)) print(i6[i])
}

#i8
i8 = table(data$location)
for(i in 1:length(i8)){
  if(i8[[i]] == min(i8)) print(i8[i])
}

#i9
i8 = table(data$location)
for(i in 1:length(i8)){
  if(i8[[i]] == max(i8)) print(i8[i])
}

#i10
i10 = table(data$date)
for(i in 1:length(i10)){
  if(i10[[i]] == min(i10)) print(i10[i])
}

#i11
i10 = table(data$date)
for(i in 1:length(i10)){
  if(i10[[i]] == max(i10)) print(i10[i])
}

#i12
i12 = table(data$date, data$continent)
i12 = i12[order(as.Date(rownames(i12), format="%m/%d/%Y")),]

#i13
cat(max(i12))

#i14
cat(min(i12))

#i15
cat(i12[456,3])

#i16
i16 = table(data %>% select(iso_code) %>% filter(str_length(iso_code) <= 3))
i16 = sort(i16, decreasing = FALSE)
i = 1
while (i < nrow(i16)){
  temp = c()
  if (i16[[i]] == i16[[i+1]]) {
    
    temp = cbind(temp, i16[[i]])
    temp = cbind(temp, rownames(i16)[i])
    
    while (i16[[i]] == i16[[i+1]]){
      i = i + 1
      temp = cbind(temp, rownames(i16)[i])
      if (i == nrow(i16)) break
    }
    
    cat(temp,'\n')
  }
  i = i + 1
}

#i17
i17 = unique(data %>% select(iso_code, location) %>% filter(str_length(iso_code) >3))















