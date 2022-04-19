pizza <-read.table('/Users/josephflynn/Downloads/Frozen_Pizza.txt', sep ='\t', header = TRUE)
pizza$Week = as.Date(pizza$Week,format="%m/%d/%Y")

#Question 1:
#Baltimore
plot(pizza$Baltimore.Price,pizza$Baltimore.Volume,main="Sales vs Price for Baltimore",xlab="Price in Baltimore",ylab="Sales in Baltimore")
bal.mod = lm(Baltimore.Volume ~ Baltimore.Price, data = pizza)
abline(bal.mod)
summary(bal.mod)$coefficients
#y-hat= 126624.9 - 34,955.6x

#Dallas
plot(pizza$Dallas.Price,pizza$Dallas.Volume,main="Sales vs Price for Dallas",xlab="Price in Dallas",ylab="Sales in Dallas")
dal.mod = lm(Dallas.Volume ~ Dallas.Price, data = pizza)
abline(dal.mod)
summary(dal.mod)$coefficients
#y-hat= 139547.43 - 33,527.19x

#Chicago
plot(pizza$Chicago.Price,pizza$Chicago.Volume,main="Sales vs Price for Chicago",xlab="Price in Chicago",ylab="Sales in Chicago")
chi.mod = lm(Chicago.Volume ~ Chicago.Price, data = pizza)
abline(chi.mod)
summary(chi.mod)$coefficients
#y-hat= 1094047.1 - 331,151.8x
#Chicago is the most sensitive to price because it has the largest slope.
#So for an increase in price of one, it causes an decrease in volume by 331,151.8.
#This is the largest change in volume, when price changes, than any other city. 

#Denver
plot(pizza$Denver.Price,pizza$Denver.Volume,main="Sales vs Price for Denver",xlab="Price in Denver",ylab="Sales in Denver")
den.mod = lm(Denver.Volume ~ Denver.Price, data = pizza)
abline(den.mod)
summary(den.mod)$coefficients
#y-hat= 181218.15 - 52,795.75x

#Question 2:
#Baltimore
#Residual Plots:
par(mfrow = c(1, 2))
plot(pizza$Week, bal.mod$residuals, xlab = 'Time', ylab = 'Residual',main = "Baltimore Time")
abline(a = 0, b = 0)
plot(bal.mod$fitted.values, bal.mod$residuals, xlab = 'Fitted value', ylab = 'Residual',main = "Baltimore Fitted")
abline(a = 0, b = 0)
#The two residual plots are slightly different.
#Constant Variance: 
#   The residuals for the time are fairly constant, but spreads increase for the fitted.
#Independence: 
#   No pattern for the time residuals. A downward line for the fitted residuals.

#Failed independence for fitted values. Caution for constant variance for fitted values.

#Q-Q Plot:
par(mfrow = c(1, 1))
qqnorm(bal.mod$residuals,main="Baltimore Normal Q-Q Plot")
qqline(bal.mod$residuals) 
#Normality: Heavy tail that shows a skew right.

#Failed normality. 

#Dallas
#Residual Plots:
par(mfrow = c(1, 2))
plot(pizza$Week, dal.mod$residuals, xlab = 'Time', ylab = 'Residual',main = "Dallas Time")
abline(a = 0, b = 0)
plot(dal.mod$fitted.values, dal.mod$residuals, xlab = 'Fitted value', ylab = 'Residual',main = "Dallas Fitted")
abline(a = 0, b = 0)
#The two residual plots are fairly identical.
#Constant Variance: 
#   Constant spread for both.
#Independence:
#   No pattern for both.

#Passed both constant variance and independence.

#Q-Q Plot:
par(mfrow = c(1, 1))
qqnorm(dal.mod$residuals,main="Dallas Normal Q-Q Plot")
qqline(dal.mod$residuals)
#Normality:
#   Heavy tails for the distribution.


#Chicago
#Residual Plots:
par(mfrow = c(1, 2))
plot(pizza$Week, chi.mod$residuals, xlab = 'Time', ylab = 'Residual',main = "Chicago Time")
abline(a = 0, b = 0)
plot(chi.mod$fitted.values, chi.mod$residuals, xlab = 'Fitted value', ylab = 'Residual',main = "Chicago Fitted")
abline(a = 0, b = 0)
#The two residual plots look different.
#Constant Variance: 
#   Constant variance for time residuals. Fitted values variance increases with x.
#Independence:
#   No pattern for time residuals. Cluster of points for fitted values.

#Failed independence and constant variance for fitted values.

#Q-Q Plot:
par(mfrow = c(1, 1))
qqnorm(chi.mod$residuals, main="Chicago Normal Q-Q Plot")
qqline(chi.mod$residuals)
#Normality:
#   Uniform distribution.

#Denver
#Residual Plots:
par(mfrow = c(1, 2))
plot(pizza$Week, den.mod$residuals, xlab = 'Time', ylab = 'Residual',main = "Denver Time")
abline(a = 0, b = 0)
plot(den.mod$fitted.values, den.mod$residuals, xlab = 'Fitted value', ylab = 'Residual',main = "Denver Fitted")
abline(a = 0, b = 0)
#Constant Variance: 
#   Constant spread for time residuals. Slight increase in spread for fitted plot.
#Independence:
#   Both plots show no pattern.

#Caution for the fitted values because of the constant spread.

#Q-Q Plot:
par(mfrow = c(1, 1))
qqnorm(den.mod$residuals, main="Denver Normal Q-Q Plot")
qqline(den.mod$residuals)
#Normality:
#   Heavy tail. Right skew. 

#Failed normality.

#Question 3
confidence = confint(dal.mod, level = 0.90)
confidence
#Intercept 90% interval: (120844.48, 158250.38).
#Slope 90% interval:(-40655.79, -26398.58).
#We are 90% confident that as Price increases by 1 causes the Volume on average will increase by the amount between -40655.79 and -26398.58.
#Yes, there is a statistcaly signifcant relationship because the range doesn't contain 0 in both instances.

#Question 4

#H0: b1 = 0, Ha: b1 < 0

summary(dal.mod)$coefficients
#t-score for b1 = -7.782879 and p-value = 9.618115e-13

qt(c(.025, .975), df=154)
#alpha level: 0.05
#We reject if the t-score is outside the range of -1.975488 to 1.975488.

# |-7.782879| > 1.975488
# 9.618115e-13 < 0.05
#Reject the null hypothesis because absolute value of the t-score is greater than the rejection value.
#This is further supported by the p-value being less than 0.05

#Question 5

#Price 2.5
predict(dal.mod, newdata = data.frame(Dallas.Price = c(2.50)),interval ='confidence', level = 0.95)
#We are 95% confidence that the mean Sales for a Price of 2.5 is between 54063.14 and 57395.79.

#Price 3
predict(dal.mod, newdata = data.frame(Dallas.Price = c(3)),interval ='confidence', level = 0.95)
#We are 95% confidence that the mean Sales for a Price of 3 is between 35464.31 and 42467.44.

#Price 3.5
predict(dal.mod, newdata = data.frame(Dallas.Price = c(3.50)),interval ='confidence', level = 0.95)
#We can estamate the mean for a price outside the range, using a confidence interval, but it is not the best practice because it is extrapolation.
#Best practice would be to select an x value within the range. However, a confidence interval can still be performed.
#We are 95% confidence that the mean Sales for a Price of 3.5 is between 14589.42 and 29815.14.

#Question 6
#We can predict the sales for the following week by using the prediction interval for the price of 2.99 at an alpha of 0.05
#By doing a prediction interval at 95%. We can predict the sales at a certain price and a range is provided for uncertainty.

predict(dal.mod, newdata = data.frame(Dallas.Price = c(2.99)),interval ='prediction', level = 0.95)
#We are 95% confident that the Sales for next week is between 22425.65 and 56176.65 if the Price is 2.99.

#These results are useful because you can forecast estimated Sales based on Prices within the range.
