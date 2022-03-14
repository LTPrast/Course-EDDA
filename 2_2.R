# want to investigate which explanatory variables need to be included into 
# a linear regression model with oxidant as the response variable.
airpollution <- read.table(file="airpollution.txt",header=TRUE)
head(airpollution)
attach(airpollution)
# 2a)  Make some graphical summaries of the data. Investigate the problem of 
# potential and influence points, and the problem of collinearity.
pairs(airpollution)

airpollutionlm=lm(oxidant~wind+temperature+humidity+insolation)
plot(residuals(airpollutionlm),wind)
plot(residuals(airpollutionlm),temperature)
plot(residuals(airpollutionlm),humidity)
plot(residuals(airpollutionlm),insolation)
plot(residuals(airpollutionlm),oxidant)

#problem of potential and influence points: A leverage (or potential) point 
#is an observation with an outlying value in the explanatory variable.
round(cooks.distance(airpollutionlm),2)
plot(1:30,cooks.distance(airpollutionlm),type="b")
# if model parameters change drastically with/without point it is considered and 
# influence point: if close to or larger than 1 it's an influence point. In our
# case largest point is 0.8 so is good

# problem of collinearity: problem of linear relations between explanatory 
# variables. A straight line in a scatter plot of two variables means they 
# explain the same. -> insolation is a horizontal line with every variable
# which suggests it doesn't have an effect.
round(cor(airpollution),2)
pairs(airpollution)
# some explanatory variables possible could be colinear -> straight line
# in scatter plot, however none are very correlated
# variance inflation factors (larger than 5 is concern)
library(car); vif(airpollutionlm)
# since values all look alright no need to worry about colinearty. If the values
# would be above 5, than remove a explanatory variable and compare again etc

# outliers (question didnt ask to investigate outliers):
orderedair <- order(abs(residuals(airpollutionlm)))
u11=rep(0,length(orderedair)); u11[11]=1; u11
forbeslm11=lm(oxidant~wind+temperature+humidity+insolation+u11); summary(forbeslm11)
# since u11 significant outlier is significant


#b)  Use the added variable plot to depict the relationship between response 
#oxidant and predictor wind. 

x=residuals(lm(wind~temperature+humidity+insolation))
y=residuals(lm(oxidant~temperature+humidity+insolation))
summary(lm(y~x)) # gives estimate of wind want slope is van fitted regression
summary(airpollutionlm)
plot(x,y,main="Added variable plot for wind", xlab="residual of wind",
     ylab="residual of oxidant")
#What is the meaning of the slope of fitted regression for this scatter plot?
#slope in this plot reflects the regression coefficients βj from the original 
#multiple regression model, and the residuals in this plot are precisely the 
#residuals from the original multiple regression. Outliers and heteroskedasticity
#(caused by Xj ) can be identified by looking at the plot of a simple rather than
#multiple regression model.

# So the meaning of the slope is the relationship between y and x once other
# predictors have been accounted for. It shows the effect of adding 
# x, wind to the model, So the slope is a estimate of the wind in our case.

avPlots(airpollutionlm)


# also possible plots:
plot(residuals(airpollutionlm),wind) # check x variables
plot(residuals(airpollutionlm),temperature)
plot(residuals(airpollutionlm),humidity)
plot(residuals(airpollutionlm),insolation)
plot(residuals(airpollutionlm),oxidant) # check y
plot(residuals(airpollutionlm),fitted(airpollutionlm)) # check estimates y

# normality check
qqnorm(residuals(airpollutionlm))
shapiro.test(residuals(airpollutionlm))

#c)  Fit a linear regression model to the data. Use both the step-up and 
#step-down methods to find the best model. If step-up and step-down yield 
#two different models, choose one and motivate your choice.  

#The step up method:
#  1. start with the background model Y = β0 + e;
#2. take the variable (that is not in the model) that yields the maximum
#increase in R 2;
#3. if this variable is significant (t -test) add it to the model and go to step 2,
#otherwise stop.

# Dont use day as variable since this can be seen as an id variable
summary(lm(oxidant~wind)) #Multiple R-squared:0.5863 and significant
summary(lm(oxidant~temperature)) #Multiple R-squared: 0.576 and significant
summary(lm(oxidant~humidity)) #Multiple R-squared:0.124, humidity ns
summary(lm(oxidant~insolation)) #Multiple R-squared:0.2552, intercept ns

# we take the variable with the highest increase in the multiple R-squared and 
# continue
summary(lm(oxidant~wind+temperature)) #Multiple R-squared:0.7773, intercept ns
summary(lm(oxidant~wind+humidity)) #Multiple R-squared: 0.5913, humidity ns
summary(lm(oxidant~wind+insolation)) #Multiple R-squared: 0.6613, all significant
# temperature has largest multiple R-sqaured and is significant
summary(lm(oxidant~wind+temperature+humidity)) #Multiple R-squared:0.7964 humidity ns
summary(lm(oxidant~wind+temperature+insolation)) #Multiple R-squared:  0.7816 insolation ns
# since humidity and insolation both aren't significant anymore best fit with
# step up method is:
summary(lm(oxidant~wind+temperature)) # does not matter intercept not significant
#The step down method:
#  1. start with the full model Y = β0 + β1X1 + ... + βp Xp + e;
#2. test all variables by using the t -test;
#3. if the largest p-value is larger than 0.05, remove the corresponding
#variable and go back to step 2.
summary(lm(oxidant~wind+temperature+humidity+insolation))
# insolation has highets p-value: 0.65728 so remove
summary(lm(oxidant~wind+temperature+humidity))
# humidity ns with p=0.131 so remove
airpolllm <- (lm(oxidant~wind+temperature));summary(airpolllm)
# all remaining variables are significant so this model is the best fit with
# the step up method. Both methods return same model however if they didnt:
# Look a multiple R-squared -> higher means better fit and if comparable than
# model with less variables is better

#d)  Determine 95% confidence and prediction intervals for oxidant using 
#the model you preferred in c) for wind=33, temperature=54, humidity=77 and 
#insolation=21.
newxdata=data.frame(wind=33,temperature=54,humidity=77,insolation=21)
predict(airpolllm,newxdata,interval="prediction",level=0.95)
predict(airpolllm,newxdata,interval="confidence",level=0.95)
# confidence is for population mean, prediction is for individual observation
#Difference: prediction interval is larger because it also takes error 
# into account