#2a
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
t.test(clouds$seeded,clouds$unseeded)
# Mann-Whitney test
wilcox.test(clouds$seeded,clouds$unseeded)
# Kolmogorov-Smirnov test
ks.test(clouds$seeded,clouds$unseeded)
# is data paired? 
# Chose clouds so independent/not paired,also clouds aren’t identical, 
# think twins for paired test or the same person/thing used twice.
# Are tests applicable?
# mann whitney and komogorov need the samples to come 
# from different populations, so be independent. This assumption can be withhold.
# T-test isnt applicable due to assumption of normality being violated. Variances
# also significantly differ so t.test cant be used with var.equal = true. 
# Comment on findings:
# Non parametric tests more significant due to outliers for which these 
# tests aren't as sensitive for, since mann whitney is a rank test (median)
# and komogorov checks the differences of the verticality of the histograms,
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
t.test(sqrtseeded,sqrtunseeded)
# Mann-Whitney test
wilcox.test(sqrtseeded,sqrtunseeded)
# Kolmogorov-Smirnov test
ks.test(sqrtseeded,sqrtunseeded)

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
t.test(sqrtsqrtseeded,sqrtsqrtunseeded, var.equal = TRUE)
# Mann-Whitney test
wilcox.test(sqrtsqrtseeded,sqrtsqrtunseeded)
# Kolmogorov-Smirnov test
ks.test(sqrtsqrtseeded,sqrtsqrtunseeded)
# are tests now applicable with transformations
# Mann whithey and kolmogorov were applicable the whole time, however now
# since only 1 outlier is left, t-test assumptions not violated. 
# P values similar, all around 0,01. (wouldn't it be expected t-test to be more,
# significant now assumptions are not violated? maybe due to outlier still having
# an influence)

#2c This code copied from lecture ppt however isn't correct
c1=clouds[,1] #   seeded
c2=clouds[,2] # unseeded
T1=mean(c1); T2=mean(c2)
T1
T2

B=1000
Tstar=numeric(B)
for(i in 1:B) {
  Xstar=sample(c1,replace=TRUE)
  Tstar[i]=mean(Xstar) }
Tstar25=quantile(Tstar,0.025)
Tstar975=quantile(Tstar,0.975)
sum(Tstar<Tstar25)
c(2*T1-Tstar975,2*T1-Tstar25)

# bootstrap test
hist(Tstar,prob=T)
hist(Tstar,prob=T,ylim=c(0,0.7))
x=seq(0,max(Tstar),length=1000)
lines(x,dexp(x),type="l",col="blue",lwd=2)
t=max(Tstar)
t
B=1000
tstar=numeric(B)
n=length(Tstar)
for (i in 1:B){
  xstar=rexp(n,1)
  tstar[i]=max(xstar)}
hist(tstar,prob=T)
pl=sum(tstar<t)/B; pr=sum(tstar>t)/B; p=2*min(pl,pr)
pl;pr;p

# Kolmogorov-Smirnov test
ks.test(c1,c2)

# 2d
#is median precipitation for seeded clouds less than 300?
x <- sum(clouds$seeded<300)
n <- length(clouds$seeded)
binom.test(x,n,p=0.5,alt='less')
# not significant (P = 0.9622) so h0 not rejected: which is medians are the same

# is fraction seeded clouds with precipertation less than 30 under 25%? 
x <- sum(clouds$seeded<30)
binom.test(x,n,p=0.25,alt='less')
# not significant (P=0.08019) h0 not rejected: which is medians are the same

