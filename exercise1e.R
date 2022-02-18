tab = matrix(c(3,2,4,6), ncol=2, byrow=TRUE)
tab
colnames(tab) = c('men', 'women')
rownames(tab) = c('more than 15.5', 'less than 15.5')

tab <- as.table(tab)

tab
chisq.test(tab) 


#As the p-value 0.8548 is greater than the .05 significance level, we do not 
#reject the null hypothesis that the wating time is below or above 15.5 min
#is independent of the fact that if you are a man or woman.
#so the claim of these researcher that the waiting time is different for men and
#women is not proven based on the chi squared test




