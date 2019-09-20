###  LIBRARIES ###
install.packages('ggplot2')
library(ggplot2)
install.packages('lattice')
library(lattice)
install.packages('readr')
library(readr)
install.packages('rpivotTable')
library(rpivotTable)



## IMPORTING THE REQUIERED DATASET #####

Coldstorage = read.csv("K2_Cold_Storage_Temp_Data.csv",header = TRUE)
View(Coldstorage)
class(Coldstorage$Temperature)
nrow(Coldstorage)
## PERFORMING PRELIMINARY ANALYSIS ON THE DATASET ##
summary(Coldstorage)
(Coldstorage)
str(Coldstorage)

### Univariate Analysis ###
Season.table = table(Coldstorage$Season)
Season.table
qplot(Season,data = Coldstorage,xlab = "Season",ylab = "Number of days",main = "Bar graph for Seasons")
Month.table = table(Coldstorage$Month)
View(Month.table)
qplot(Month,data = Coldstorage,xlab = "Month",ylab = "Number of days",main = "Bar graph for Months")
qplot(Temperature,data = Coldstorage,ylab = "Frequency",main = "Histogram for Temperature")
qplot(Temperature,data = Coldstorage,geom = "density",ylab = "Frequency",main = "Histogram for Temperature")

### Bivariate Analysis ####
histogram(~Temperature|factor(Month),data = Coldstorage)

histogram(~Temperature|factor(Season),data = Coldstorage)

qplot(Temperature,data = Coldstorage, fill = Season, ylab = "Frequency", title = "Coloured Histogram")
qplot(Temperature,data = Coldstorage, fill = Season,geom = "density",ylab = "Frequency",main = "Coloured Density graph")
qplot(Temperature,Date,data = Coldstorage, fill = Season,geom = "label",label = Temperature,main = "Label Graph")
qplot(Temperature,Date,data = Coldstorage, fill = Season,geom = "count",main = "Count Graph")
qplot(Temperature,Date,data = Coldstorage, fill = Season,geom = "density_2d",main = "2D Density Graph")
qplot(Temperature,Date,data = Coldstorage, fill = Season,geom = "violin")

qplot(Temperature,Date,data = Coldstorage,fill = Month, geom = "boxplot",main = "Box Plot")
qplot(Month,Temperature,data = Coldstorage,main = "Dot Plot")
qplot(Temperature,Date,data = Coldstorage,fill = Month, geom = "tile",main = "Tile Graph")
qplot(Temperature,Date,data = Coldstorage,fill = Month, geom = "polygon",main = "Polygon Graph")
qplot(Date,Temperature,data = Coldstorage, geom = "crossbar",ymin = 2, ymax = 4,main = "Crossbar for 2-4 Degrees range")

summary(Coldstorage$Temperature)
qplot(Temperature,data = Coldstorage,xlab = "Temperature",ylab = "Frequency",main = "Histogram for Temperature")




## 1.Finding the mean Temperature for Summer, Winter and Rainy ####
### Finding the mean for the summer season 
Summer = Coldstorage[Coldstorage$Season == "Summer",]
mean(Summer$Temperature)
### Finding the mean for the winter season
Winter = Coldstorage[Coldstorage$Season == "Winter",]
mean(Winter$Temperature)
### Finding the mean for the Rainy season
Rainy = Coldstorage[Coldstorage$Season == "Rainy",]
mean(Rainy$Temperature)



## 2.Finding the mean Temperature for the whole year ########
Mean = mean(Coldstorage$Temperature)
Mean
median(Coldstorage$Temperature)


## 3.Finding the Standard Deviation for the whole year ######
SD = sd(Coldstorage$Temperature)
SD


##4. Finding the probability of the temperature being below 2 Degrees ######
Q4 = pnorm(2,Mean,SD)
Q4

##5. Finding the probability of the temperature being above 4 Degrees ######
Q5 = 1-pnorm(4,Mean,SD)
Q5
Q4
Q5

###Total probability of temperature crossing the given thresholds(2-4)
prob = Q4 + Q5
prob = prob * 100
prob
if(prob > 2 && prob < 5) {
  print("Penalty is 10%")
} else if (prob > 5) {
  print("Penalty is 25%")
} else {
  print("No need to pay penalty")
}
