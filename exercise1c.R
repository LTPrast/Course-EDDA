data = c(15.4, 17.9, 19.0, 0.5, 15.9, 2.7, 6.2, 2.5, 4.7, 6.9, 10.8, 24.3, 5.6, 23.0, 10.7)
B=10000


mu0 = 13
psign=numeric(B) ## will contain p-values of sign test
pttest=numeric(B) ## will contain p-values of t-test

for(i in 1:B) {
   x=sample(data,replace=TRUE)
   pttest[i]=t.test(x,mu=mu0,alt="less")[[3]] ## extract p-value
   psign[i]=binom.test(sum(x>mu0),length(data),p=0.5)[[3]] } ## extract p-value

sum(psign<0.05)/B
sum(pttest<0.05)/B