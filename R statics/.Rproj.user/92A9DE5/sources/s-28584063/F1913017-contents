library(Rstat)
library(randomizeBE)

#---문제1-----------------------------------------------------------------------
# T = sum(yijk)
# 총제곱합 (SST) = sum(( yijk - y' )^2) = sum( (yijk)^2 ) - T^2/N
# 요인A제곱합(SSA) = sn*sum((y'i..-y')^2) = sum(Ti..^2)/sn - T^2/N
# 요인B제곱합(SSB) = rn*sum((y'.j.-y')^2) = sum(T.j.^2)/rn - T^2/N
# 교호작용제곱합(SSA*B) = SSAB - SSA - SSB
# 오차제곱합(SSE) = sum((yijk - y'ij.)^2) = SST-SSAB
# AB 총제곱합 = n*sum((y'ij.-y')^2) = sum((Tij.)^2/n) - T^2/N

# 아래의 분산분석표를 사용하여 세 가지 가설을 검정할 수 있다.
# 구분   제곱합  자유도      평균제곱    검정통계량   기각치
# A      SSA     r-1         MSA         MSA/MSE    > F(1-a/2)(φ(A),φ(E))
# B      SSB     s-1         MSB         MSB/MSE    > F(1-a/2)(φ(B),φ(E))
# A*B    SSA*B   (r-1)(s-1)  MS*B        MSA*B/MSE  > F(1-a/2)(φ(A*B),φ(E))
# E      SSE     rs(n-1)     MSE
# T      SST     rsn-1


# (y'ij.-y'i'j'. +- t(1-a/2;φ(E))*sqrt(MSE/n))
qt(0.975, 660)*sqrt(0.2090909/45) # 0.1338464

# (2) 교호작용이 의미 없는 경우
# 새로운 오차항(MSE)을 구해야 하는데, 
# 교호작용의 제곱합을 더한 오차항의 제곱합은 ( 새로운 SSE')
# SSE' = SSE + SSA_B 
# 교호작용의 제곱합을 더한 오차항의 자유도는 
# (r-1)*(s-1) + r*s*(n-1)  이다.
# 따라서 새로운 MSE' = (SSE+SSA_B) / ((r-1)*(s-1) + r*s*(n-1)) 이다,
# φ(E') = (r-1)*(s-1) + r*s*(n-1)

# 수준조합 AiBj에서의 모평균의 100(1-a)% 신뢰구간은 
# [ (y'i..+y'.j.-y') +- t(1-a/2;φ(E'))*sqrt(MSE'*(r+s-1)/N) ]

#---문제2-----------------------------------------------------------------------
meanx = 3.384
meany = 38.84
sumxpow = 289.74
sumypow = 38507
sumxy = 3317
n=25

# rxy = Sxy/sqrt(sxx*Syy)


# Sxx = sum((xi - x')^2) = sum(xi^2) - sum(xi)^2/n
# Syy = sum((yi - y')^2) = sum(yi^2) - sum(yi)^2/n=
# Sxy = sum((xi-x)*(yi-y)) = sum(xi*yi) - sum(xi)*sum(yi)/n

sxx = sumxpow - (meanx*n)^2/n ; sxx # 3.4536
syy = sumypow - (meany*n)^2/n ; syy # 793.36
sxy = sumxy - meanx*n*meany*n/n ; sxy # 31.136

rxy = sxy/sqrt(sxx*syy) ; rxy # 0.5948283
rxy^2
sqrt(23/)
T0 = rxy*sqrt((n-2)/(1-(rxy)^2)) ; T0
sqrt(23 / (1-rxy^2))
target = qt(0.95, n-2) ; target

ρ0
Z0 = sqrt(n-3)*( 1/2*ln( (1+rxy)/(1-rxy) ) - 1/2*ln( (1+ρ0)/(1-ρ0) ) )

# β1^ = SXY / SXX
# β0^ = y' - β1^*x'

# 총변동 : SST = (sum(yi-y')^2) = sum(yi^2) - (sum(yi))^2/n
# SST = SSR + SSE

# 회귀제곱합 : SSR = sum((yi^-y')^2) = (B1^)^2 * sum(xi-x')^2
#           = (B1^)^2 * SXX = SXY^2 / SXX

# 오차제곱합 : SSE = sum(yi - yi^)^2 = SST - SSR

# SST의 자유도 = n-1 , SSR의 자유도 = 1 ( x는 독립변수라서 변량으로 취급X )
# 변량은 기울기와 절편이다. -> ( 2 - 1 ) = 1이라서 SSR의 자유도는 1이다.
# SSE의 자유도 = n-2 ( 전체 n개에서 절편과 기울기 2개를 추정하였기 때문 )
# = 1 + (n-2) = n-1
(9.01552)^2 * 3.4536

# 단순회귀분석의 분산분석표
# 요인   제곱합(SS)  자유도  평균제곱(MS)      검정통계량       기각역
# 회귀   SSR         1       MSR = SSR/1       MSR/MSE          F(1-a;(1,n-2))
# 잔차   SSE         n-2     MSE - SSE/(n-2)
# 계     SST         n-1     

# (B0_+_B1_*x0) +- qt(1-a/2, n-2)*sqrt( MSE*(1 + 1/n + (x0-x')^2/sxx) )


a=8.33148+9.01532*3.5 # 39.8851
b=qt(0.975, 23) # 2.068658
c=sqrt(22.28925 * (1+1/25+(3.5-3.384)/3.4536)) # 4.891776
a + b*c
a - b*c
