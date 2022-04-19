GDP <-read.table('/Users/josephflynn/Downloads/GDP.txt', sep ='\t', header = TRUE)

#Question 1

par(mfrow =c(1, 2))
hist(GDP$Personal.Income,xlab = "Personal Income", main = "Histogram of Personal Income")
qqnorm(GDP$Personal.Income)
qqline(GDP$Personal.Income)
#The histogram shows its heavily right skewed. Thus, violating the normal population assumption.
#The best transformation would use a log.

par(mfrow =c(1, 2))
y.log = log(GDP$Personal.Income, base = 19)
#After trying different log bases, base 19 created the closed to a belll-curve.
#However, the Q-Q plot seemed to remain the same for all different log bases
hist(y.log,xlab = "Personal Income", main = "Histogram of Log base 19")
#The histogram is closer to a bell shape than before
qqnorm(y.log)
qqline(y.log)
#The Q-Q plot seems to follow the Q-Q line closer than before.
#The left tail seems a bit heavy still and the right tail is a bit light. However, it isn't extreme.
#Thus, making it a good transformation 

#Question 2

par(mfrow =c(1,1))
plot(GDP$GDP,y.log, xlab="GDP", ylab="Transformed Personal Income", main="Transformed Personal Income vs GDP" )
#The scatter plot shows an arching curve.Thus, the linearity assumption isn't satisfied. 
#To make the scatterplot show a linear relationship, then the x-axis must be transformed.

x.log = log(GDP$GDP)
#GDP is transformed using the natural log.
plot(x.log,y.log, xlab="Transformed GDP", ylab="Transformed Personal Income", main="Transformed Personal Income vs Transformed GDP" )
#The scatterplot now shows a linear relationship. Showing the transformation as a good fit.

#Question 3

tran.mod = lm(log(Personal.Income, base = 19)~ log(GDP), data = GDP)
#A model is created using the transformed x and y values
plot(tran.mod$fitted.values, tran.mod$residuals, xlab = "Fitted Value", ylab = "Residual")
abline(a = 0, b = 0)
#From the residual, the equal-variance assumption seems to be satisfied because the residuals show no pattern and are space considantly from 0.

#Question 4

#There are two points that are possible outliers, since they are faily far from the trend. Being the points around 10.85 and 11.2.
#There aren't any points with high leverage. They are all fairly close. 
#The only influential points are the outliers 10.85 and 11.2, since they would pull the trend-line down.

#Question 5
summary(tran.mod)
#log(y, base = 19) = -0.034668 + 0.341324*log(x)
#transformed into y = 19^(-0.034668 + 0.341324*log(x))

y.test = predict(tran.mod, newdata = data.frame(GDP = 300000))
#The predicted log(Personal Income, base = 19) is 4.269949 .
y.pred = 19^(y.test)
#The predicted Personal Income is 288544.1 for a GDP of 300,000. This is the value of Personal Income when the transformation is undone.
#The predicted Personal Income is good to calculate because it is with the range of the data.

interval = predict(tran.mod, newdata = data.frame(GDP = 300000), interval ='prediction', level = 0.95)
tran.int = 19^interval
# We are 95% that Personal Income would be between 218840.9 and 380448.5, if the GDP is 300,000.

