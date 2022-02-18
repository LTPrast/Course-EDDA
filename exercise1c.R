data = c(15.4, 17.9, 19.0, 0.5, 15.9, 2.7, 6.2, 2.5, 4.7, 6.9, 10.8, 24.3, 5.6, 23.0, 10.7)
B=10000

#power test for sign test and t-test with mu0=14
mu0_1= 13
psign_13=numeric(B) ## will contain p-values of sign test
pttest_13=numeric(B) ## will contain p-values of t-test

for(i in 1:B) {
   x=sample(data,replace=TRUE)
   pttest_13[i]=t.test(x,mu=mu0_1,alt="less")[[3]] ## extract p-value
   psign_13[i]=binom.test(sum(x>mu0),length(data),p=0.5, alt="less")[[3]]} ## extract p-value

#percentage of t test/sign tests that are lower than 0.05. T tests have a lot
#larger percentage of tests that reject the H0.
sum(psign_13<0.05)/B
sum(pttest_13<0.05)/B

#power test for sign test and t-test with mu0=14
mu0_2 = 14
psign_14=numeric(B) ## will contain p-values of sign test
pttest_14=numeric(B) ## will contain p-values of t-test

for(i in 1:B) {
  x=sample(data,replace=TRUE)
  pttest_14[i]=t.test(x,mu=mu0_2,alt="less")[[3]] ## extract p-value
  psign_14[i]=binom.test(sum(x>mu0),length(data),p=0.5, alt="less")[[3]]} ## extract p-value

#percentage of t test/sign tests that are lower than 0.05. T tests have a lot
#larger percentage of test than reject the H0.
sum(psign_14<0.05)/B
sum(pttest_14<0.05)/B

#T test rejects the H0 more than the sign test for both mu0=13 and mu0=14
#which means that the wit the t-test the H1 is accepted more which was that 
#mean<15. What is also clearly seen for the t-test is the H1 is accepted more
#when mu0=14 compared to mu0=13. This is not the case for the sign test were
#H1 is accepted roughly the same amount
