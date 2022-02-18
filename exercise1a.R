data = c(15.4, 17.9, 19.0, 0.5, 15.9, 2.7, 6.2, 2.5, 4.7, 6.9, 10.8, 24.3, 5.6, 23.0, 10.7)
#check normality
qqnorm(data)
hist(data)
shapiro.test(data)
#larger than confidence level: 0.05, therefore passed the test and can state 
#that there exists no significant departure from normality. 

#construct a 97%-CI for mu
mean_data = mean(data)
std_data = sd(data)
n = length(data)

#0.985 since 2-sided and confidence interval of 0.97,normal distribution
#since we can assume normality
error = qnorm(0.985)*std_data/sqrt(n)
left = mean_data-error
right = mean_data+error

#length of the 97%-CI is at most 2 what sample size should we use.
#2E=2 so E=1
E = 1
min_sample_size = (qnorm(0.985)^2*std_data^2)/ E^2
min_sample_size

##### bootstrap method #####
#If we have a (small) sample from an unknown distribution and the distribution
#of X¯ is not close to normal, we cannot rely on the above (asympt.) normal CI.


B=1000
Tstar=numeric(B)
for(i in 1:B) {
  Xstar=sample(data,replace=TRUE)
  Tstar[i]=mean(Xstar) }

Tstar15=quantile(Tstar,0.015)
Tstar985=quantile(Tstar,0.985)

#comparison t-test and bootstrap method
c(left, mean_data, right)
c(2*mean_data-Tstar985,mean(Tstar),2*mean_data-Tstar15)

