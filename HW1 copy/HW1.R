#----- HOMEWORK 1 -----#

#sets working directory
setwd("/Users/josephflynn/Documents/Baruch Content/Fall 2020/STA 4155/HW1")

#reads in text file into global environment as a list
my_data <- read.delim("Cost_of_Living_2013.txt",header = TRUE)

#Question 1: Scatter Plots of Cost of Living Index vs Other Index Variables

#1-graph per plot
par(mfrow  = c(1,1))

#Scatter Plot of Cost of Living Index vs Rent Index
plot(x = my_data$Rent.Index, y = my_data$Cost.of.Living.Index,
     xlab = "Rent Index", ylab = "Cost of Living Index",
     main = "Cost of Living Index vs Rent Index")

#Scatter Plot of Cost of Living Index vs Groceries Index
plot(x = my_data$Groceries.Index, y = my_data$Cost.of.Living.Index,
     xlab = "Groceries Index", ylab = "Cost of Living Index",
     main = "Cost of Living Index vs Groceries Index")

#Scatter Plot of Cost of Living Index vs Restaurant Price Index
plot(x = my_data$Restaurant.Price.Index, y = my_data$Cost.of.Living.Index,
     xlab = "Restaurant Price Index", ylab = "Cost of Living Index",
     main = "Cost of Living Index vs Restaurant Price Index")

#Scatter Plot of Cost of Living Index vs Local Purchasing Power Index
plot(x = my_data$Local.Purchasing.Power.Index, y = my_data$Cost.of.Living.Index,
     xlab = "Local Purchasing Power Index", ylab = "Cost of Living Index",
     main = "Cost of Living Index vs Local Purchasing Power Index")

#Question 2: Correlation Coefficient for Cost of Living Index vs Other Index Variables

#Correlation Coefficient for Cost of Living Index vs Rent Index
rent_cor <- cor(x = my_data$Rent.Index, y = my_data$Cost.of.Living.Index)

#Correlation Coefficient for Cost of Living Index vs Groceries Index
groceries_cor <- cor(x = my_data$Groceries.Index, y = my_data$Cost.of.Living.Index)

#Correlation Coefficient for Cost of Living Index vs Restaurant Price Index
restaurant_cor <- cor(x = my_data$Restaurant.Price.Index, y = my_data$Cost.of.Living.Index)

#Correlation Coefficient for Cost of Living Index vs Local Purchasing Power Index
local_cor <- cor(x = my_data$Local.Purchasing.Power.Index, y = my_data$Cost.of.Living.Index)

#Question 3: 

#Question 4(a): Linear Model for Cost of Living Index vs Other Index Variables

#Linear Model for Cost of Living Index vs Rent Index
rent_linear <- lm(Cost.of.Living.Index ~ Rent.Index, data = my_data)
rent_linear$coefficients

#Linear Model for Cost of Living Index vs Groceries Index 
groceries_linear <- lm(Cost.of.Living.Index ~ Groceries.Index, data = my_data)
groceries_linear$coefficients

#Linear Model for Cost of Living Index vs Restaurant Price Index
restaurant_linear <- lm(Cost.of.Living.Index ~ Restaurant.Price.Index, data = my_data)
restaurant_linear$coefficients

#Linear Model for Cost of Living Index vs Local Purchasing Power Index
local_linear <- lm(Cost.of.Living.Index ~ Local.Purchasing.Power.Index, data = my_data)
local_linear$coefficients

#Question 4(b): Re-plot Scatter Plots with Linear Model:

#1-graph per plot
par(mfrow  = c(1,1))

#Scatter Plot of Cost of Living Index vs Rent Index with Linear Regression
plot(x = my_data$Rent.Index, y = my_data$Cost.of.Living.Index,
     xlab = "Rent Index", ylab = "Cost of Living Index",
     main = "Cost of Living Index vs Rent Index")
abline(rent_linear)

#Scatter Plot of Cost of Living Index vs Groceries Index with Linear Regression
plot(x = my_data$Groceries.Index, y = my_data$Cost.of.Living.Index,
     xlab = "Groceries Index", ylab = "Cost of Living Index",
     main = "Cost of Living Index vs Groceries Index")
abline(groceries_linear)

#Scatter Plot of Cost of Living Index vs Restaurant Price Index with Linear Regression
plot(x = my_data$Restaurant.Price.Index, y = my_data$Cost.of.Living.Index,
     xlab = "Restaurant Price Index", ylab = "Cost of Living Index",
     main = "Cost of Living Index vs Restaurant Price Index")
abline(restaurant_linear)

#Scatter Plot of Cost of Living Index vs Local Purchasing Power Index with Linear Regression
plot(x = my_data$Local.Purchasing.Power.Index, y = my_data$Cost.of.Living.Index,
     xlab = "Local Purchasing Power Index", ylab = "Cost of Living Index",
     main = "Cost of Living Index vs Local Purchasing Power Index")
abline(local_linear)

#Question 4(c): Residual & Q-Q Plots for Cost of Living Index vs Other Index Variables

#2-graphs per plot
par(mfrow = c(1,2))

#Set 1: Cost of Living Index vs Rent Index
#Residuals vs Fitted Values for Cost of Living Index vs Rent Index
plot(x = rent_linear$fitted.values, y = rent_linear$residuals,
     xlab = "Fitted Value", ylab = "Residual",
     main = "Rent Index")
abline(0,0)

#Q-Q plot for Cost of Living Index vs Rent Index
qqnorm(rent_linear$residuals, main = "Rent Index")
qqline(rent_linear$residuals)

#Set 2: Cost of Living Index vs Groceries Index
#Residuals vs Fitted Values for Cost of Living Index vs Groceries Index
plot(x = groceries_linear$fitted.values, y = groceries_linear$residuals,
     xlab = "Fitted Value", ylab = "Residual",
     main = "Groceries Index")
abline(0,0)

#Q-Q plot for Cost of Living Index vs Groceries Index
qqnorm(groceries_linear$residuals, main = "Groceries Index")
qqline(groceries_linear$residuals)

#Set 3: Cost of Living Index vs Restaurant Price Index
#Residuals vs Fitted Values for Cost of Living Index vs Restaurant Price Index
plot(x = restaurant_linear$fitted.values, y = restaurant_linear$residuals,
     xlab = "Fitted Value", ylab = "Residual",
     main = "Restaurant Index")
abline(0,0)

#Q-Q plot for Cost of Living Index vs Restaurant Price Index
qqnorm(restaurant_linear$residuals, main = "Restaurant Index")
qqline(restaurant_linear$residuals)

#Set 4: Cost of Living Index vs Local Purchasing Power Index
#Residuals vs Fitted Values for Cost of Living Index vs Local Purchasing Power Index
plot(x = local_linear$fitted.values, y = local_linear$residuals,
     xlab = "Fitted Value", ylab = "Residual",
     main = "Local Purchasing Power Index")
abline(0,0)

#Q-Q plot for Cost of Living Index vs Local Purchasing Power Index
qqnorm(local_linear$residuals, main = "Local Purchasing Power Index")
qqline(local_linear$residuals)








