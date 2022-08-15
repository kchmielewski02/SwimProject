##Regression Analysis to determine the factors that make an elite 100m freestyle swimmer
##By: Kyle Chmielewski
##Date:May 5, 2022

##Reading in the data
my_data <- read.csv("/Users/kchmielewski/Documents/Junior_Spring/REGRESSION/swimming_data.csv")

##cleaning data (dropping missing values)
my_data <- my_data[complete.cases(my_data),]


##Scatterplot Matrix and Correlation matrix
library(car)
data = my_data
scatterplotMatrix(~ Height + Weight + Age + Avg.Time + Best.Time, smooth = FALSE, data = data)
print(cor(data[,c("Height", "Weight", "Age", "Avg.Time", "Best.Time")]))

##Variable Selection
##forward selection
m1 = lm(Best.Time ~ 1, data = data)
step(m1, scope = ~ Height + Weight + Age + Avg.Time, direction="forward")

##backward selection
m2 = lm(Best.Time ~ Height + Weight + Age + Avg.Time, data = data)
step(m2, scope = ~ 1, direction = "backward")

##regression analysis
model = lm(Best.Time ~ Height + Age, data = data)
summary(model)
confint(model)

##model diagnostics

##residual testing & heteroscedasticity
residualPlots(model)
ncvTest(model)

##Transformations
model_3 = lm(1/(Best.Time)~Height + Age, data = data)
summary(model_3)
residualPlots(model_3)
ncvTest(model_3)

##outlier testing
outlierTest(model)
cooks.distance(model)
infIndexPlot(model)

##running regression without outlier
library(dplyr)
my_data2 <- my_data %>% slice(-c(49))
model_4 = lm(1/(Best.Time) ~ Height + Age, data = my_data2)
summary(model_4)

##normality testing
ks.test(residuals(model_3), rnorm(dim(data)))
qqnorm(residuals(model_3), pch = 1, frame = FALSE)

##Linear Probability Model 
model_2 = lm(Medal ~ Height + Age, data = data)
summary(model_2)

