library(Rstat)


#-------------------------------Ex13-01--------------------------------------
# 한 화학공정 온도 100도, 150도, 200도, 250도 4수준에서 랜덤 순서로 반복 실험
# -> 수율, 제곱합 계산, 분산분석표 작성
# 온도의 변화가 수율에 영향을 미치는 지 유의수준 5%에서 검정

a1 = c(79, 83, 88, 78, 75)
a2 = c(81, 89, 91, 84, 86, 82)
a3 = c(86, 91, 93, 90, 89)
a4 = c(76, 81, 82, 79)
n1 = 5; n2 = 6; n3 = 5; n4 = 4
n = n1+n2+n3+n4; r = 4

sum1 = sum(a1) ; sum2 = sum(a2) ; sum3 = sum(a3) ; sum4 = sum(a4)
m1 = mean(a1) ; m2 = mean(a2) ; m3 = mean(a3) ; m4 = mean(a4)
T = sum1 + sum2 + sum3 + sum4 ; mean = sum / n

sst = sum(a1^2) + sum(a2^2) + sum(a3^2) + sum(a4^2) - T^2/n; sst
# sst = [1] 546.55

ssa = sum1^2/n1 + sum2^2/n2 + sum3^2/n3 + sum4^2/n4 - T^2/n ; ssa
# ssa = [1] 320.05

sse = sst - ssa ; sse
# sse = [1] 226.5

msa = ssa/(r-1)
mse = sse/(n-r)

target = msa/mse; target
# target = [1] 7.53613

?qf
limit = qf(0.95, r-1, n-r); limit
# [1] 3.238872

# target = 검정통계량이 기각역보다 크므로
# 유의수준 5%에서 귀무가설을 기각한다.
# 이 공정에서 4수준의 온도 변화는 수율에 영향을 미친다는 증거가 있다.
# 4수준 = 4개의 수준

#--------여기서부터 교수님 R풀이
data = exa13_1
y = exa13_1[[1]]
f = exa13_1[[2]]
?anova1
anova1(y, f, xl = "온도", yl = "수율", step=0:3)
#[Step 0] Mean of 수율 for each level of 온도 ---------
#    150  200  250  300 
#80.6 85.5 89.8 79.5 
#[Step 1] Box Plot of 수율 for each level of 온도 ---------
#    [Step 2] ANOVA Table of 수율 w.r.t. 온도 ---------
#    Sum Sq. df Mean Sq.     F0 P-value
#온도   320.05  3 106.6833 7.5361  0.0023
#Error   226.5 16  14.1562               
#Total  546.55 19                        
#[Step 3] ANOVA Diagnostic Plot of 수율 w.r.t. 온도 ---------

# 기본 R 함수 사용
af = as.factor(f)
an1 = aov(y~af)
summary(an1)


#-------------------------------Ex13-02--------------------------------------
# 예제 13-1에서 온도의 각 수준별로 수율의 모평균에 대한 95% 신뢰구간을 
# 구하고, 이를 그래프로 작성하시오.

max1 = m1 + qt(0.975, n-r)*sqrt(mse/n1); min1 = m1 - qt(0.975, n-r)*sqrt(mse/n1)
max1 ; min1
#[1] 84.16702
#[1] 77.03298

max2 = m2 + qt(0.975, n-r)*sqrt(mse/n2); min2 = m2 - qt(0.975, n-r)*sqrt(mse/n2)
max2 ; min2
#[1] 88.75623
#[1] 82.24377

max3 = m3 + qt(0.975, n-r)*sqrt(mse/n3); min3 = m3 - qt(0.975, n-r)*sqrt(mse/n3)
max3 ; min3
#[1] 93.36702
#[1] 86.23298

max4 = m4 + qt(0.975, n-r)*sqrt(mse/n4); min4 = m4 - qt(0.975, n-r)*sqrt(mse/n4)
max4 ; min4
#[1] 83.48805
#[1] 75.51195

# 그래프
anova1(y, f, xl="온도", yl="수율", step=4:5, alp=0.05, dig=4)
#[Step 4] 95% CI for the Mean of 수율 w.r.t. 온도 ---------
#    MSE = 14.1562 
#150     200    250    300
#ym  80.600 85.5000 89.800 79.500
#tol  3.567  3.2562  3.567  3.988
#lcl 77.033 82.2438 86.233 75.512
#ucl 84.167 88.7562 93.367 83.488
#[Step 5] 95% CI Plot for the Mean of 수율 w.r.t. 온도 ---------


#-------------------------------Ex13-03--------------------------------------
# 예제 13-1에서 온도의 두 수준에서 수율의 모평균 차이에 대한 
# 95% 신뢰구간을 구하고, 모평균 차이가 유의한지 검정하시오.

qt(0.975, n-r) # [1] 2.119905
mse # [1] 14.15625

# sum1-sum2
max=(m1-m2)+qt(0.975, n-r)*sqrt( mse*((1/n1)+(1/n2)) )
min=(m1-m2)-qt(0.975, n-r)*sqrt( mse*((1/n1)+(1/n2)) )
max; min
#[1] -0.0702318
#[1] -9.729768

# sum1-sum3
max=(m1-m3)+qt(0.975, n-r)*sqrt( mse*((1/n1)+(1/n3)) )
min=(m1-m3)-qt(0.975, n-r)*sqrt( mse*((1/n1)+(1/n3)) )
max; min
#[1] -4.155472
#[1] -14.24453

# sum1-sum4
max=(m1-m4)+qt(0.975, n-r)*sqrt( mse*((1/n1)+(1/n4)) )
min=(m1-m4)-qt(0.975, n-r)*sqrt( mse*((1/n1)+(1/n4)) )
max; min
#[1] 6.45053
#[1] -4.25053

# sum2-sum3
max=(m2-m3)+qt(0.975, n-r)*sqrt( mse*((1/n2)+(1/n3)) )
min=(m2-m3)-qt(0.975, n-r)*sqrt( mse*((1/n2)+(1/n3)) )
max; min
#[1] 0.5297682
#[1] -9.129768

# sum2-sum4
max=(m2-m4)+qt(0.975, n-r)*sqrt( mse*((1/n2)+(1/n4)) )
min=(m2-m4)-qt(0.975, n-r)*sqrt( mse*((1/n2)+(1/n4)) )
max; min
#[1] 11.14855
#[1] 0.8514498

# sum3-sum4
max=(m3-m4)+qt(0.975, n-r)*sqrt( mse*((1/n3)+(1/n4)) )
min=(m3-m4)-qt(0.975, n-r)*sqrt( mse*((1/n3)+(1/n4)) )
max; min
#[1] 15.65053
#[1] 4.94947

# Rstat 함수
anova1(y, f, xl="온도", yl="수율", step=6:7, alp=0.05, dig=4)
# step 6 : 두 수준의 모평균 신뢰구간 구하기

#[Step 6] 95% CI for the Mean Differences of 수율 w.r.t. 온도 ---------
#    A1-A2    A1-A3   A1-A4   A2-A3
#ym2  -4.9000  -9.2000  1.1000 -4.3000
#tol2  4.8298   5.0445  5.3505  4.8298
#lcl2 -9.7298 -14.2445 -4.2505 -9.1298
#ucl2 -0.0702  -4.1555  6.4505  0.5298
#A2-A4   A3-A4
#ym2   6.0000 10.3000
#tol2  5.1486  5.3505
#lcl2  0.8514  4.9495
#ucl2 11.1486 15.6505
#[Step 7] 95% CI Plot for the Mean Differences of 수율 w.r.t. 온도 ---------
# = 그래프

# A의 수준 R이라고 하면
# SST = sum(yij^2) - T^2/N -------자유도 N-1
# SSA = sum(y'i.^2/ni) - T^2/N ------자유도 r-1
# SSE = SST - SSA ----------------자유도 N-r

# 분산분석표
# 구분      제곱합      자유도   평균제곱        검정통계량    기각역
# 처리      SSA         r-1      MSA = SSA/(r-1) MSA/MSE       F(1-a;r-1,n-r)
# 오차      SSE         N-r      MSE = SSE/(N-r)
# 계        SST         N-1


# 모평균 범위 ( mse자유도)
# yi +- t(1-a/2; n-r)*sqrt(mse/ni)