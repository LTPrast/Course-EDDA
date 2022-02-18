data = c(15.4, 17.9, 19.0, 0.5, 15.9, 2.7, 6.2, 2.5, 4.7, 6.9, 10.8, 24.3, 5.6, 23.0, 10.7)

s = sd(data)
n = length(data)

# z-score is probability
pr = 0.53 # right end confidence interval
x <- sum(data>15.5)/n # sum of values > 15.5 / total
m <- pr-x # cause pr = x+m, m is error margin
pl <- x-m;pl # left end confidence interval
cv <- (sqrt(n)*m)/s # cause m = cv*s/sqrt(n), is critical value (1.96 in ppt)
area <- 0.5398 # for cv 0.10 read from z-score table
alpha <- (1-area/2);alpha # don't actually need alpha cl is area/2
cl <- 1-alpha;cl # confidence level
