library(Rstat)
#------------------------------ 기초 개론 -------------------------------------
# 인과관계 예측 -> 좋은 의사결정
# 상관분석과 회귀분석 : 변수 간의 관련성 분석
# 상관분석 : 두 변수 간의 선형관계를 계량적으로 분석
# 회귀분석 : 변수를 설명변수와 종속변수로 구분하여 , 종속변수를 
#               설명변수의 특정한 함수 형태로 설명할 수 있는 지를 분석

# -----------------------------14-1.1 상관계수의 측정 --------------------------
# 상관계수: 두 확률변수 X와 Y의 상관관계(선형관계)의 부호와 강약을 나타내는 척도
# ρxy = Corr(X, Y) = ( Cov(X, Y) / σxσy ) = ( σxy / σxσy )

# 1. 상관계수 ρxy는 -1 <= ρxy <= 1 
# 2. 두 변수가 서로 독립이면 두 변수 간에 상관관계가 있으며 , ρxy = 0
# 3. ρxy = 0이면 두 변수 간에 상관관계(선형관계)가 없다.
#   그러나 비선형관계는 있을 수 있기 때문에 두 변수가 서로 독립이라는 보장X
# 4. x와  y가 정규분포를 따르는 경우 ρxy = 0이면 x와 y는 독립이다.

# 구하는 공식 예시 1
X=c(1,2,3,4,5)
Y=c(1,2,4,4,5)
cor(X,Y) #[1] 0.9622504

# 구하는 공식 예시 2
df=data.frame(X=c(1,2,3,4,5),Y=c(1,1,2,3,4),Z=c(1,3,5,6,7))
cor(df) 
# 아래 표는 결과
# X         Y         Z
# X 1.0000000 0.9701425 0.9847982
# Y 0.9701425 1.0000000 0.9235481
# Z 0.9847982 0.9235481 1.0000000

# 표본상관계수 : 표본을 통하여 상관계수를 추정하는 통계량
# 두 확률변수 X와 Y의 모집단으로부터 n쌍의 확률표본
# (X1, Y1), (X2, Y2), ,,, (Xn, Yn)을 얻었을 때
# X와 Y의 표본상관계수는 다음과 같이 정의한다.

# rxy = Sxy/sqrt(sxx*Syy)


# Sxx = sum((xi - x')^2) = sum(xi^2) - sum(xi)^2/n
# Syy = sum((yi - y')^2) = sum(yi^2) - sum(yi)^2/n=
# Sxy = sum((xi-x)*(yi-y)) = sum(xi*yi) - sum(xi)*sum(yi)/n

# rxy의 범위는 -1<= rxy <= 1
# rxy의 값이 +1 또는 -1에 가까울 수록 산점도 상의 점들이 직선에 가깝게 위치
# 1 : 양의 기울기 직선 , -1 : 음의 기울기 직선에 해당



# --------------------------------[예제 14-1]----------------------------------
# 학생 20명의 1, 2학기 과목별 성적이 다음과 같을 때 , 여섯 가지 데이터 쌍의
# 산점도를 작성하고 상관계수를 구하여 비교

# 1학기와 2학기 데이터 간 상관계수를 구하는 것

data = data(exa14_1)
# str(exa14_1) : 데이터 출력하기
과목 = c('국어', '영어', '수학', '사회', '과학', '예체능')
xd = list()
for (k in 1:6) xd[[k]] = cbind(exa14_1[[2*k-1]], exa14_1[[2*k]])
corr.mplot(X=xd, item=과목, xl = "1학기", yl = "2학기", step = 1:2)

df = data.frame(exa14_1); 
n = 20
x1_ = sum(df['x1']); x1_ = x1_/n; x1_ # [1] 61.34
# mean 바로 사용하면 NA 값 나온다. 아마도 문자열이라서 그런 것 같다.

국어1 = sum(df['x1']); 국어1 = 국어1 / n ; 국어1
국어2 = sum(df['y1']); 국어2 = 국어2 / n ; 국어2
수학1 = sum(df['x3']); 수학1 = 수학1 / n ; 수학1
수학2 = sum(df['y3']); 수학2 = 수학2 / n ; 수학2
영어1 = sum(df['x2']); 영어1 = 영어1 / n ; 영어1
영어2 = sum(df['y2']); 영어2 = 영어2 / n ; 영어2
사회1 = sum(df['x4']); 사회1 = 사회1 / n ; 사회1
사회2 = sum(df['y4']); 사회2 = 사회2 / n ; 사회2
과학1 = sum(df['x5']); 과학1 = 과학1 / n ; 과학1
과학2 = sum(df['y5']); 과학2 = 과학2 / n ; 과학2
예체1 = sum(df['x6']); 예체1 = 예체1 / n ; 예체1
예체2 = sum(df['y6']); 예체2 = 예체2 / n ; 예체2

국어1 = sum((df['x1']-국어1)^2)
국어2 = sum((df['y1']-국어2)^2)

수학1 = sum((df['x2']-수학1)^2)
수학2 = sum((df['y2']-수학2)^2)




# ------------------------14-1.2 상관계수유무에 대한 검정 ----------------------
# H0 : ρxy = 0 | T0 = rxy * sqrt( (n-2) / (1-r^2xy) ) ~ t(n-2)
# 상관관계가 없는 경우에 t분포를 따른다.

# 대립가설 H1 : ρxy > 0 : 기각역 : T0 > t(1-a;n-2)
# 대립가설 H1 : ρxy < 0 : 기각역 : T0 < t(a;n-2) = -t(1-a;n-2)
# 대립가설 H1 : ρxy !=0 : 기각역 :|T0| >t(1-a/2;n-2)

# -----------------------------------------[예제 14-3]------------------------
# 한 수출기업에서 원-달러 환율과 수출액(억 원) 간의 관계를 분석하기 위하여
# 한 지점의 최근 10개월 간의 데이터를 수집한 결과가 다음과 같다.
# 유의수준 5%에서 원-달러 환율과 수출액 간의 상관관계가 있다고 할 수 있는지
# 검정하시오.

x = c(1095, 1110, 1086, 1074, 1098, 1105, 1163, 1124, 1088, 1064)
y = c(49, 52, 48, 49, 50, 51, 50, 51, 49, 48)
n = 10

tx = sum(x) ; mx = mean(x) 
ty = sum(y) ; my = mean(y)
SXX = sum(x^2) - sum(x)^2/n ; SXX # [1] 7006.1
SYY = sum(y^2) - sum(y)^2/n ; SYY # [1] 16.1
SXY = sum((mx-x)*(my-y)) ; SXY 
rxy = SXY / sqrt(SXX*SYY) ; rxy # 0.5779289

T0 = rxy * sqrt( (n-2)/ (1-rxy^2) ); T0 # [1] 2.003009

target = qt(0.975, n-2); target # 2.306004
# 상관관계가 있다는 충분한 근거가 없다.


# ------------------------14-1.3 상관계수에 대한 검정 -------------------------
# 14-1.3에서의 상관계수가 있다는 것과 달리 , 

# 두 변수 간의 상관계수가 특정한 값과 같은지 판단해야 하는 경우가 있다.
# 이때의 검정통계량은 다음 정리와 같이 귀무가설 하에서 근사적으로 
# 표준정규분포를 따른다.

# 귀무가설 : ρxy = ρ0
# 검정통계량 : 
#   Z0 = sqrt(n-3)*( 1/2*ln( (1+rxy)/(1-rxy) ) - 1/2*ln( (1+ρ0)/(1-ρ0) ) )
#       ~N(0,1)

# 대립가설 H1 : ρxy > ρ0 => 기각역 : Z0 > Z1-a
# 대립가설 H1 : ρxy < ρ0 => 기각역 : Z0 < -Z1-a
# 대립가설 H1 : ρxy !=ρ0 => 기각역 :|Z0|> Z1-a/2

# -----------------------------------------[예제 14-4]------------------------
# [예제 14-3]에서 환차손익을 배제하기 위해 수출애 단위를 
# 억원 이 아닌 10만 USD로 바꾸어 데이터를 분석하기로 하였다.

x = c(1095, 1110, 1086, 1074, 1098, 1105, 1163, 1124, 1088, 1064)
y2 = c(53.655, 57.72, 52.128, 52.626, 54.9, 56.355, 58.15, 57.324, 53.312, 51.072)
# y2 = y*x/1000
n = 10

tx = sum(x) ; mx = mean(x) 
ty = sum(y2) ; my = mean(y2)
SXX = sum(x^2) - sum(x)^2/n ; SXX # [1] 7006.1
SYY = sum(y2^2) - sum(y2)^2/n ; SYY # [1] 57.77906
SXY = sum((mx-x)*(my-y2)) ; SXY 
rxy = SXY / sqrt(SXX*SYY) ; rxy # 0.8811918

# (1)   유의수준 5%에서 원-달러 환율과 수출액 간에 상관관계가 있다고 
#       할 수 있는지 검정하시오.
T0 = rxy * sqrt( (n-2)/ (1-rxy^2) ); T0 # [1] 5.272004

target = qt(0.975, n-2); target # 2.306004
# 상관관계가 있다고 할 수 있다.

# (2)   유의수준 5%에서 원-달러 환율과 수출액 간에 상관계수가 0.9라고
#       할 수 있는지 검정하시오.
p0 = 0.9
temp1 = 1/2 * log((1+rxy)/(1-rxy)) ; temp1 # 1.381075
temp2 = 1/2 * log((1+p0)/(1-p0)) ; temp2 # 1.472219 - OK
Z0 = sqrt(n-3)*(temp1 - temp2) 
Z0 # -0.2411452


target = qnorm(0.975) ; target # [1] 1.959964
# abs(Z0) < target 이므로 , 귀무가설 채택 - 상관계수가 0.9라고 할 수 있다.



# ------------------------14-2 회귀분석 기본개념--------------------------------
# 단순회귀분석 : 하나의 독립변수로 하나의 종속변수를 설명하는 모형
# 예 ) 아버지의 키로 한 자녀의 키를 설명하는 경우에 해당

# 다중회귀분석 : 두 개 이상의 독립변수로 하나의 종속변수를 설명하는 모형
# 예 ) 아버지와 어머니의 키로 한 자녀의 키를 설명

# 곡선회귀분석 : 독립변수와 종속변수의 관계를 2차 이상의 함수로 설명
# 예 ) 2차 함수관계 -> 독립변수 = (x , x^2) ->  다중회귀분석 기법 사용
# -> 독립 변수 간의 종속성에 주의

# 다변량회귀분석 : 두 개 이상의 종속변수를 사용하는 모형
# 예 ) 아버지와 어머니의 키로 두 자녀의 키를 설명하는 모형

# -----------------------------------------[예제 14-5]------------------------
# 앞의 [예제 14-4]의 데이터를 사용하여 산점도를 작성하고 , 
# 직선 회귀식을 표시하시오. 
x = c(1095, 1110, 1086, 1074, 1098, 1105, 1163, 1124, 1088, 1064)
y2 = c(53.655, 57.72, 52.128, 52.626, 54.9, 56.355, 58.15, 57.324, 53.312, 51.072)
# y2 = y*x/1000
n = 10

tx = sum(x) ; mx = mean(x) 
ty = sum(y2) ; my = mean(y2)
SXX = sum(x^2) - sum(x)^2/n ; SXX # [1] 7006.1
SYY = sum(y2^2) - sum(y2)^2/n ; SYY # [1] 57.77906
SXY = sum((mx-x)*(my-y2)) ; SXY 
rxy = SXY / sqrt(SXX*SYY) ; rxy # 0.8811918

corr.reg1(x, y2, xl="환율", yl="수출액", step=6)
# y = -33.3577+0.08*x -> 그래프 출력문에 식이 나온다.

# ---------------------------14-3 단순회귀분석 ---------------------------------
# 단순선형회귀 모형 : yi = β0 + β1xi + εi , i = 1,2,,,n
# 회귀계수 >> β0 = 절편 // β1 = 기울기
# 오차항 εi ~ N(0, σ^2)

# 단순선형회귀 모형의 특성
# E(εi) = 0 -> E(yi) = E(β0 + β1xi + εi) = β0 + β1xi
# Var(yi) = Var(β0 + β1xi + εi) = Var(εi) = σ^2
# εi 는 독립적이다 -> 따라서 yi도 독립적이다.
# yi ~ N(β0 + β1xi , σ^2)

# 단순선형회귀 추정모형 
# yi = yi^ + ei = β0^ + β1^xi + + ei , i = 1,2,,,n
# ei = 잔차 -> 오차의 관측치

# 그래프를 보면 , x1, x2,,, xn을 지날 때마다 
# 그 좌표에서 정규분포의 값을 가진다


# ---------------------------14-3.1 회귀계수의 추정-----------------------------

# 최소제곱법 : 오차의 제곱합을 최소화하는 회귀계수의 값을 구하는 방법
# --- 중간에 편미분 들어가서 이해하기 어렵다.

# 정규 방정식( 중간 과정 - 필요 없음 )
# sum(yi) = n*β0^ + β1^ * sum(x)
# sum(xi*yi) = β0^*sum(x) + β1^ * sum(x^2)
# 두 식을 빼면 
# β1^ * ( n*sum(xi^2) - ( sum(xi) )^2  )

# 최소제곱추정량 (LSE)
# β1^ = SXY / SXX
# β0^ = y' - β1^*x'
#                                 이것만 외우자.

# [ 정리 14-5 ] 단순선형회귀 모형의 최소제곱추정
# 단순선형회귀 모형 : yi = B0 + B1xi + ei ( i = 1,2,,,n )
# 최소제곱추정치(LSE) : B1^ = Sxy / Sxx , B0^ = y' - B1^*x'
# 추정 회귀식 : y^ = B0^ + B1^x
# 잔차의 특성 : sum(ei) = 0 , sum(xi*ei) = 0 

# -----------------------------------------[예제 14-6]------------------------
# [예제 14-4]에서 환율을 독립변수 , 수출액을 종속변수로 놓고 , 단순선형회귀
# 모형 yi = β0 + β1*xi + εi , (i = 1,2,,,n) 에서
# 회귀계수의 최소제곱 추정치를 구하시오.

x = c(1095, 1110, 1086, 1074, 1098, 1105, 1163, 1124, 1088, 1064)
y2 = c(53.655, 57.72, 52.128, 52.626, 54.9, 56.355, 58.15, 57.324, 53.312, 51.072)
# y2 = y*x/1000
n = 10

tx = sum(x) ; tx
mx = mean(x); mx # [1] 1100.7
ty = sum(y2) ; ty
my = mean(y2) ; my # [1] 54.7242
SXX = sum(x^2) - sum(x)^2/n ; SXX # [1] 7006.1
SYY = sum(y2^2) - sum(y2)^2/n ; SYY # [1] 57.77906
SXY = sum((mx-x)*(my-y2)) ; SXY # [1] 560.6526
rxy = SXY / sqrt(SXX*SYY) ; rxy # 0.8811918

B1_ = SXY / SXX ; B1_ # [1] 0.08002349
B0_ = my - B1_*mx ; B0_ # [1] -33.35766
# 교수님 답안에서는 -33.332라는데 , 
# 반올림 오차로 인하여 절편 추정치에서 다소의 차이가 발생하였다.

# y^ = B0^ + B1^x = -33.332 + 0.080x

corr.reg1(x, y2, xl = "환율", yl = "수출액", step=7)

