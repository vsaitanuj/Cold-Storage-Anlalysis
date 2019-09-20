setwd("C:/R programs great lakes/smdm")
install.packages('TeachingDemos')
library(TeachingDemos)
### LIBRARIES
library(readr)
library(readxl)



### Importing the dataset in consideration
StorageMarchData = read.csv("K2_Cold_Storage_Mar2018.csv",header = TRUE)
int = read.csv("InternetMobileTime .csv")
Coldstorage = read.csv("K2_Cold_Storage_Temp_Data.csv",header = TRUE)
### Performing prelimnary tests on dataset
summary(StorageMarchData)
View(StorageMarchData)
dim(StorageMarchData)
str(StorageMarchData)
attach(StorageMarchData)
median(StorageMarchData$Temperature)
View(table(StorageMarchData$Temperature))


rpivotTable(Coldstorage)
# Finding the requiered parameters for the Z test
Mean.sample = mean(StorageMarchData$Temperature)
Mean.sample
Mean.population = mean(Coldstorage$Temperature) ##Taken from the total dataset where the mean of all the temperatures for the whole year was calculated
Mean.population = 3.9
Mean.population
SD.population = sd(Coldstorage$Temperature)
SD.population
Sample.size = nrow(StorageMarchData)
Sample.size
Z.value.1 = (Mean.sample - Mean.population)
Z.value.2 = (SD.population)/(Sample.size)^0.5
Z.value  = Z.value.1/Z.value.2
Z.stat = (Mean.sample - Mean.population)/(SD.population/(Sample.size)^0.5)
Z.stat
Z.value
##Finding the critical value
Z.critical = qnorm(0.90)
Z.critical
##Decision of rejection or accpetance of the null Hypothesis
if(Z.value > Z.not){
"The Null hypothesis is rejected"
  }else { 
"The null hypothesis is accepted"
  }

zplot = z.test(StorageMarchData$Temperature,Mean.population,SD.population,95,alternative = "greater")

zplot

z.t = c(3.8:4.6)
plot(zplot,z.t)

### T. test

Mean.sample = mean(StorageMarchData$Temperature)
Mean.sample
Mean.population = mean(Coldstorage$Temperature) ##Taken from the total dataset where the mean of all the temperatures for the whole year was calculated
Mean.population = 3.9
Mean.population
SD.sample = sd(StorageMarchData$Temperature)
SD.sample
Sample.size = nrow(StorageMarchData)
Sample.size
Degrees.of.freedom = Sample.size - 1
Degrees.of.freedom
T.value1 = (Mean.sample - Mean.population)
T.value1
T.value2 = (SD.sample)/(Sample.size)^0.5
T.value2
T.value = T.value1/T.value2
T.value
T.stat = (Mean.sample - Mean.population)/(SD.sample)/(Sample.size^0.5)
T.stat
hist(StorageMarchData$Temperature)
conf = 0.90
t.test(StorageMarchData$Temperature,mu = 3.9,alternative = "greater",conf.level = conf)

T.stat
P.value = 1-(pt(T.value,34))
P.value = 1-(pt(T.stat,34))
P.value

plot(P.value)



t.test()
t.test(int$Minutes,mu = 144,alternative = "two.sided",conf.level = 0.95)
