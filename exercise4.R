#1a 
#4 rate factors 2 method factors and each specific combination is repeated 
#for 10 fishes. With this code the 80 fishes are distributed randomly 
#over all combinations of levels of factors rate and method.
I=4; J=2; N=10
rbind(rep(1:I,each=N*J),rep(1:J,N*I),sample(1:(N*I*J)))

#1b
data = read.table("hemoglobin.txt", header=TRUE)

#necessary other wise rate is numerical and should be categorical
data$rate=as.factor(data$rate)

boxplot(hemoglobin ~ method, data = data)
boxplot(hemoglobin ~ rate, data = data)

interaction.plot(x.factor = data$rate, trace.factor = data$method, response = data$hemoglobin)
interaction.plot(x.factor = data$method, trace.factor = data$rate, response = data$hemoglobin)

hemoglobinaov=lm(hemoglobin ~ method * rate, data = data) 
anova(hemoglobinaov)
#The p-value for the interaction between method and the rate is larger than 0.05. 
#So, there is no evidence for interaction between the two factor. Therefore
#we will create a new model without the interaction called the additive model

hemoglobinaov2 =lm(hemoglobin ~ method + rate, data = data)
anova(hemoglobinaov2)
#With the two way anova additive model we can see that the method factor had a 
#p-value>0.05 and therefore has no  main effect for the outcome of this research
#the rate factor has a p-value<0.05 which means that the effect of different amounts 
#of sulfamerazine that is given to the  brown trout gives significantly different 
#amounts of hemoglobin in their blood.

qqnorm(residuals(hemoglobinaov2))
qqline(residuals(hemoglobinaov2))
#QQ plot looks like the residuals are normally distributed

shapiro.test(residuals(hemoglobinaov2))
#larger than confidence level: 0.05, therefore passed the test and can state 
#that there exists no significant departure from normality. 

plot(fitted(hemoglobinaov2),residuals(hemoglobinaov2))
hemoglobinaov2
#the spread in the residuals seems to be in the similar range for all the fitted 
#values. So, we can assume equal variance in the residuals.
#Because the requirements of normality and the assumption of equal variances
#are met we can use the two way ANOVA test

#4c
#Which of the two factors has the greatest influence? 
#The rate factor has a significant main effect on the the amount of hemoglobin,
#while the method factor does not have a significant main effect on the amount
#of hemoglobin. So the rate factor has the greatest influence of the two. This
#is a good question, because one factor is significant and the other is not
#if both were insignificant than it would not be a good question.

library("dplyr")
group_by(data, method, rate) %>%
  summarise(
    count = n(),
    mean = mean(hemoglobin, na.rm = TRUE),
    sd = sd(hemoglobin, na.rm = TRUE)
  )
#Rate 2 and method B leads to a mean of 10.1, which is the highest hemoglobin 
#yield when both factors are considered. When rate 3 and method A was used than
#the mean was eastimated to be 9.03.

library("dplyr")
group_by(data, rate) %>%
  summarise(
    count = n(),
    mean = mean(hemoglobin, na.rm = TRUE),
    sd = sd(hemoglobin, na.rm = TRUE)
  )

#When only the factor rate is considered than rate 2 gives the highest mean
#rate with a mean of 9.74 hemoglobin yield.

#4d
hemoglobinaov3 =lm(hemoglobin ~ rate, data = data)
anova(hemoglobinaov3)
#with the one way anova we see that the p<0.05 and therefore the rate factor
#gives significant different means for the hemoglobin. We can use the one way
#anova test on this data set because the factor method is insignficant and
#the interaction between the rate factor and the method factor is also
#insignficant