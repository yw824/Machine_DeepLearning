library(Rstat)

# ---------------------------------문제1----------------------------------------
# (1-1)
N = 3+4+3+4+3 ; r = 5
n1 = 3; n2 = 4; n3 = 3; n4 = 4; n5 = 3
sum1 = 266; sum2 = 363; sum3 = 275 ; sum4 = 375 ; sum5 = 279
mu1 = sum1 / n1; mu2 = sum2 / n2; mu3 = sum3 / n3 ; mu4 = sum4 / n4 ; mu5 = sum5 / n5
T = 1558 ; Tpow = 142868
y = ( sum1 + sum2 + sum3 + sum4 + sum5 ) / N
SST =  Tpow - T^2/N ; SST # 81.88235 , 자유도 = N-1
SSA =  sum1^2/n1 + sum2^2/n2 + sum3^2/n3 + sum4^2/n4 + sum5^2/n5 - T^2/N ; SSA 
# 53.04902 자유도 = r-1
SSE = SST - SSA ; SSE # 28.83333 자유도 = N-r\

N-1 # 16
r-1 # 4
N-r # 12

MSA = SSA / (r-1) ; MSA # 13.26225
MSE = SSE / (N-r) ; MSE # 2.402778

F0 = MSA / MSE ; F0 # 5.519551

target = qf(0.95, r-1, N-r); target # 3.259167

# (1-2)
sum4 # 375
mu4 # 93.75
n4 = 4

max = mu4 + qt(0.975, 12)*sqrt(MSE/n4); max
min = mu4 - qt(0.975, 12)*sqrt(MSE/n4); min

# (1-3)
ytemp = mu4 - mu5 ; ytemp
max = ytemp + qt(0.975, 12)*sqrt(MSE/n4+MSE/n5); max # 3.329498
min = ytemp - qt(0.975, 12)*sqrt(MSE/n4+MSE/n5); min # -1.829498




# ---------------------------------문제2----------------------------------------
# (2-1)
n = 3
r = 4
s = 3
N = n*r*s; N
T = 934 ; Tpow = 24960

t11=28+29+27;t21=27+25+26;t31=27+31+28;t41=19+22+24
t12=21+19+20;t22=30+31+27;t32=25+24+23;t42=31+30+32
t13=23+25+24;t23=32+34+33;t33=28+26+29;t43=19+17+18

t1. = (t11+t12+t13)
t2. = (t21+t22+t23)
t3. = (t31+t32+t33)
t4. = (t41+t42+t43)

y1. = (t11+t12+t13)/(s*n)
y2. = (t21+t22+t23)/(s*n)
y3. = (t31+t32+t33)/(s*n)
y4. = (t41+t42+t43)/(s*n)

t.1 = t11+t21+t31+t41
t.2 = t12+t22+t32+t42
t.3 = t13+t23+t33+t43

y.1 = (t11+t21+t31+t41)/(r*n); y.1
y.2 = (t12+t22+t32+t42)/(r*n); y.2
y.3 = (t13+t23+t33+t43)/(r*n); y.3

y = T/N ; y

SST = Tpow - T^2/N ; SST # 787.8889
SSA = (t1.^2+t2.^2+t3.^2+t4.^2)/(s*n) - T^2/N ; SSA # 201.8889
SSB = (t.1^2+t.2^2+t.3^2)/(r*n) - T^2/N ; SSB # 1.388889
SSAB = (t11^2+t21^2+t31^2+t41^2+t12^2+t22^2+t32^2+t42^2+t13^2+t23^2+t33^2+t43^2)/n - T^2/N 
SSAB # 677.2222
SSA_B = SSAB - SSA - SSB ; SSA_B # 473.9444
SSE = SST - SSAB ; SSE # 50.66667

MSA = SSA/(r-1); MSA # 67.2963
MSB = SSB/(s-1); MSB # 0.6944444
MSA_B = SSA_B/((r-1)*(s-1)); MSA_B # 78.99074
MSE = SSE/(r*s)*(n-1); MSE # 8.444444

MSA/MSE
MSB/MSE
MSA_B/MSE

ta = qf(0.975, r-1, r*s*(n-1)) ; ta
tb = qf(0.975, s-1, r*s*(n-1)) ; tb
ta_b = qf(0.975, (r-1)*(s-1), r*s*(n-1)) ; ta_b

r*s*(n-1)

# (2-2)
max = t43/3 + qt(0.975, 24)*sqrt(MSE/24); max
min = t43/3 - qt(0.975, 24)*sqrt(MSE/24); min

t41/3-t32/3 + qt(0.975, 24)*sqrt(MSE/24)
t41/3-t32/3 - qt(0.975, 24)*sqrt(MSE/24)

# 3
mtcars 데이터에서 am = 0인 경우 wt(x)와 mpg(y) 간의 상관관계가 유의한지
# ㅇㅎ의수준 5%에서 검정하시오.
