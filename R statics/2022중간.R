library(Rstat)
#1 
?pbinom
pbinom(15, size=25, prob=0.5, lower.tail =FALSE)
n = c(1:50)
pbinom(3*n, n, prob=0.5, lower.tail = FALSE)
pbinom(60, 100, prob = 0.65, lower.tail = TRUE)

#2
n = 35
1-pnorm(qnorm(0.95) + (30-32)*sqrt(n)/4)

mu = 33.395
1-pnorm(qnorm(0.95) + (30-mu)*sqrt(15)/4)
#3
s1pow = (15582.42 - 620.8^2/25)/24; s1pow
s1 = sqrt(s1pow); s1
s2pow = (16299.83-695.3^2/30)/29; s2pow
s2 = sqrt(s2pow); s2
m1 = 620.8/25
m2 = 695.3/30
n1 = 25
n2 = 30

sp = ((n1-1)*s1pow + (n2-1)*s2pow)/(n1+n2-2); sp
M = (m1-m2) + qt(0.975, n1+n2-2)*sp*sqrt((1/n1)+(1/n2)); M
m = (m1-m2) - qt(0.975, n1+n2-2)*sp*sqrt((1/n1)+(1/n2)); m

target = ((m1-m2)-0.5)/sp*sqrt((1/n1)+(1/n2)); target
pnorm(target, lower.tail=TRUE)
# 4.
p1 = 5472/9000
p2 = 6875/11000
n1 = 9000
n2 = 11000
M = (p1-p2) + qnorm(0.975)*sqrt( (p1*(1-p1)/n1)+(p2*(1-p2)/n2) ) ; M
m = (p1-p2) - qnorm(0.975)*sqrt( (p1*(1-p1)/n1)+(p2*(1-p2)/n2) ) ; m

(p1-p2)/sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))
pnorm(-2.45916)
