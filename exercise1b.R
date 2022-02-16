data = c(15.4, 17.9, 19.0, 0.5, 15.9, 2.7, 6.2, 2.5, 4.7, 6.9, 10.8, 24.3, 5.6, 23.0, 10.7)
mean(data)
mu0 = 15

#Assuming normality we can do the following t-test
t.test(data,mu=mu0,alt="less")
#since p<0.05 we reject H0, which was that H0 is equal or larger than 15,
#and therefore accept H1, H1 was that mean<15.

#You can see that the 95% confidence interval for the population mean is 
#[inf;14.59] which means that at a significance level alpha =0.05 we reject
#null hypothesis as long as the hypothesized value mu0 is below 14.59
#otherwise the null hypothesis cannot be rejected

#not sure about this one
larger_than_mu = sum(data>mu0)
samples = length(data)
binom.test(larger_than_mu,samples,p=0.5, alt='less')
#since p>0.05 we accept H0, we do not have sufficient evidence to say that
#the  patients are less likely to wait less than 15 min than more than 15 min

hist(data)
#Wilcoxon signed rank test can not be done since the distribution of the data
#is not symmetric