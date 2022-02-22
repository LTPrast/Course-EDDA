#2a # leads to more rain fall <- test for greater?
# plots
par(mfrow=c(2,2))
qqnorm(clouds$seeded);hist(clouds$seeded)
qqnorm(clouds$unseeded);hist(clouds$unseeded)

# normality test
shapiro.test(clouds$seeded)
shapiro.test(clouds$unseeded)

# equal variances
par(mfrow=c(1,2))
boxplot(clouds$seeded);boxplot(clouds$unseeded)
# f test significant so significant differences between variances
var.test(clouds$seeded, clouds$unseeded, alternative = "two.sided")
# two samples t-test
t.test(clouds$seeded,clouds$unseeded,alternative = "greater")
# Mann-Whitney test
wilcox.test(clouds$seeded,clouds$unseeded,alternative = "greater")
# Kolmogorov-Smirnov test
ks.test(clouds$unseeded,clouds$seeded,alternative = "greater")
# is data paired? 
# Chose clouds so independent/not paired,also clouds aren’t identical, 
# think twins for paired test or the same person/thing used twice.
# Are tests applicable?
# mann whitney and kolmogorov need the samples to come 
# from different populations, so be independent. This assumption can be withhold.
# T-test isnt applicable due to assumption of normality being violated. Variances
# also significantly differ so t.test cant be used with var.equal = true. 
# Comment on findings:
# Non parametric tests more significant due to outliers for which these 
# tests aren't as sensitive for, since mann whitney is a rank test (median)
# and kolmogorov checks the differences of the verticality of the histograms,
# since outliers, rare, verticality of histogram for outlier small.


#2b
# repeat with square root tranformation
sqrtseeded <- sqrt(clouds$seeded)
sqrtunseeded <- sqrt(clouds$unseeded)
# plots
par(mfrow=c(2,2))
qqnorm(sqrtseeded);hist(sqrtseeded)
qqnorm(sqrtunseeded);hist(sqrtunseeded)

# normality test
shapiro.test(sqrtseeded)
shapiro.test(sqrtunseeded)

# equal variances
par(mfrow=c(1,2))
boxplot(sqrtseeded)
boxplot(sqrtunseeded)
# f test significant if significant differences between variances
var.test(sqrtseeded, sqrtunseeded, alternative = "two.sided")
# two samples t-test: 
t.test(sqrtseeded,sqrtunseeded,alternative = "greater")
# Mann-Whitney test
wilcox.test(sqrtseeded,sqrtunseeded,alternative = "greater")
# Kolmogorov-Smirnov test
ks.test(sqrtunseeded,sqrtseeded,alternative = "greater")

# still outliers while less large, t-test assumptions still violated, 
# transformation doesnt affect ranked tests since ranks is still the same
# also doesnt affect kolmogorov since frequency data points also still same.

# repeat with square root of square root transoformation
sqrtsqrtseeded <- sqrt(sqrtseeded)
sqrtsqrtunseeded <- sqrt(sqrtunseeded)
# plots
par(mfrow=c(2,2))
qqnorm(sqrtsqrtseeded);hist(sqrtsqrtseeded)
qqnorm(sqrtsqrtunseeded);hist(sqrtsqrtunseeded)

# normality test
shapiro.test(sqrtsqrtseeded)
shapiro.test(sqrtsqrtunseeded)

# equal variances
par(mfrow=c(1,2))
boxplot(sqrtsqrtseeded)
boxplot(sqrtsqrtunseeded)
# f test significant so significant differences between variances
var.test(sqrtsqrtseeded, sqrtsqrtunseeded, alternative = "two.sided")
# two samples t-test:
t.test(sqrtsqrtseeded,sqrtsqrtunseeded, var.equal = TRUE,alternative = "greater")
# Mann-Whitney test
wilcox.test(sqrtsqrtseeded,sqrtsqrtunseeded,alternative = "greater")
# Kolmogorov-Smirnov test
ks.test(sqrtsqrtunseeded,sqrtsqrtseeded,alternative = "greater")
# are tests now applicable with transformations
# Mann whithey and kolmogorov were applicable the whole time, however now
# since only 1 outlier is left, t-test assumptions not violated. 
# P values similar, all around 0,01. (wouldn't it be expected t-test to be more,
# significant now assumptions are not violated? maybe due to outlier still having
# an influence)

#2c
c1 <- clouds$seeded
lambda_est <- 1/mean(c1)
n <- length(clouds$seeded)
sd <- 1/lambda_est
# CI
z <- qnorm(1-0.025)
CI_L <- (-z+sqrt(n))/(mean(c1)*sqrt(n));CI_L
CI_R <- (z+sqrt(n))/(mean(c1)*sqrt(n));CI_R

# bootstrap test
t=median(c1)
t
B=1000
tstar=numeric(B)
n=length(c1)
for (i in 1:B){
  xstar=rexp(n,lambda_est)
  tstar[i]=median(xstar)}
hist(tstar,prob=T)
pl=sum(tstar<t)/B; pr=sum(tstar>t)/B; p=2*min(pl,pr)
pl;pr;p

#install.packages("pracma")
#library("pracma")
cdf_x <- linspace(min(c1), max(c1), 101)
sorted_c1 <- sort(c1);
# Kolmogorov-Smirnov test
par(mfrow=c(1,1))
c1_quantiles  <- linspace(0,1,length(c1))
# plots actual values of clouds$seeded
plot(sorted_c1,c1_quantiles)
# line of expected values is exp dist with rate est lambda
lines(cdf_x,pexp(cdf_x,lambda_est),type="l",col="blue",lwd=2)
ks.test(clouds$seeded,"pexp", lambda_est)

# 2d
#is median precipitation for seeded clouds less than 300?
hist(clouds$seeded)
# not symmetric so cant use wilcox
x <- sum(clouds$seeded<300)
n <- length(clouds$seeded)
binom.test(x,n,p=0.5,alt='less')
# not significant (P = 0.9622) so h0 not rejected: which is medians are the same

# is fraction seeded clouds with precipitation less than 30 under 25%? 
x <- sum(clouds$seeded<30)
binom.test(x,n,p=0.25,alt='less')
# not significant (P=0.08019) h0 not rejected: which is medians are the same

