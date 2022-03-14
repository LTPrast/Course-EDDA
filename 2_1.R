#1a
#Discuss whether a contingency table test is appropriate here. 
#It is appropriate since the data can be considered categorical and frequencies
# and the condition of the chi-squared test are also met since more than 80% 
# of the expected counts are above 5.

data = read.table("nauseatable.txt", header=TRUE)
data

chisqdata =chisq.test(data)
chisqdata
chisqdata$residuals

# looking at residuals: Chlorpromazine is no neasua, 
# 100mg is naesea en 150mg a bit nausea

#pvalue = 0.03642 which is smaller than 0.05. Therefore H0 is rejected and
#H1 is accepted which means that the row variable and column variable are 
#dependent, which in this case means that it does matter which medicine works 
#best against nausea for the patient

#Where are the main inconsistencies?
#Dont know?

#1b
no_nausea_data = data$Incidence.of.no.nausea
nausea_data = data$Incidence.of.Nausea

a = as.logical(rep(1, times = nausea_data[1]))
b = as.logical(rep(0, times = no_nausea_data[1]))
a2 = as.logical(rep(1, times = nausea_data[2]))
b2 = as.logical(rep(0, times = no_nausea_data[2]))
a3 = as.logical(rep(1, times = nausea_data[3]))
b3 = as.logical(rep(0, times = no_nausea_data[3]))
nausea = c(a,b,a2,b2,a3,b3)

c = rep('Chlorpromazine', times = nausea_data[1])
d = rep('Chlorpromazine', times = no_nausea_data[1])
e = rep('Pentobarbital(100mg)', times = nausea_data[2])
f = rep('Pentobarbital(100mg)', times = no_nausea_data[2])
h = rep('Pentobarbital(150mg)', times = nausea_data[3])
i = rep('Pentobarbital(150mg)', times = no_nausea_data[3])
medicine = c(c,d,e,f,h,i)
table(medicine)
data
      
df <- data.frame(
  medicine = medicine,
  nausea = nausea
)

B=1000
Tstar = c()
for (i in 1:B){
  Xstar = df[sample(nrow(df),1000,replace=TRUE),]
  test=chisq.test(table(Xstar))
  Tstar =  c(Tstar, test$statistic)
  
}

hist(Tstar)
chisqdata$p.value
chisqdata$statistic

pl=sum(Tstar<chisqdata$statistic)/B
pr=sum(Tstar>chisqdata$statistic)/B
pl
pr
p_value = 2*min(pl,pr)
p_value

#pvalue = 0.026 which is smaller than 0.05. Therefore H0 is rejected and
#H1 is accepted which means that the row variable and column variable are 
#dependent, which in this case means that it does matter which medicine is given
#to reduce the nausea of the patient

#1c
#Both are significant so the outcome of both test is that which medicine is given
#matters to reduce the nausea of the patient. The difference between the tests 
#is that chisquare needs atleast 5 samples for 80% of the expected counts, but
#these data has that so we could just use the chisquare

# Equality between tests: use same test statistic, however the permutation test 
# resamples the data and it is still same p-value -> original sample has a good
# distribution
