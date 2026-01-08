## exercise 1
4^7
log(10)*exp(10)
sqrt(4)
4^(1/2)

## exercise 2
# opiton1: environment -> import dataset
# option2:
library(readxl)
t1e2 <- read_excel("Dropbox/01 UoM-Teaching/2026-Summer-QM2/Tute1/t1e2.xlsx")

# generate a new variable
X = t1e2$`Age (year)` + t1e2$`Height (cm)`

# generate a new variable within the dataset
t1e2$X2 = X
# or
t1e2$X2 = t1e2$`Age (year)` + t1e2$`Height (cm)`

# generate a new variable within the dataset: log(age)
t1e2$log_age = log(t1e2$`Age (year)`)

# sorting
sort(t1e2$`Height (cm)`, decreasing = TRUE)

# censoring
t1e2$`Height (cm)` > 170
t1e2$Name[t1e2$`Height (cm)` > 170]
t1e2$`Weight (kg)`[t1e2$`Height (cm)` > 170]

## Visual
# scatter plot
plot(t1e2$`Height (cm)`, t1e2$`Weight (kg)`)

plot(t1e2$`Weight (kg)`,t1e2$`Height (cm)`, 
     main = "This is the title",
     xlab = "this is x label",
     ylab = "this is y label",
     pch = 22) # style of dot symbol

hist(t1e2$`Height (cm)`, main='this is title'
     , col = 'red')

# descirptive statistics
mean(t1e2$`Weight (kg)`)
median(t1e2$`Weight (kg)`)
sd(t1e2$`Weight (kg)`)

cor(t1e2$`Weight (kg)`, t1e2$`Height (cm)`)

summary(t1e2)

sapply(t1e2, is.numeric)
numeric_columns = t1e2[sapply(t1e2, is.numeric)]

sapply(numeric_columns, quantile)

barplot(t2e3$Visitors)






