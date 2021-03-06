library(Rstat)
ch13.man()
# ---------------------------13-2-1 이원분산분석-------------------------------

# 두 개 이상의 요인에 대해 반응치에 이상이 있는 지 분석하는 경우 = 이원 분산분석
# 세 개 이상의 요인을 다루는 경우를 다원 분산분석이라 한다.
# 두 개 이상의 요인이 있을 경우, "교호작용"을 고려한다.
# -> 두 개 이상의 요인이 서로 간섭작용을 일으키는 상호작용

# 두 요인 A , B가 있을 때 , A의 효과가 B의 수준에 따라 다르게 나타나는 경우에
# "교호작용"이 있다고 표현한다.

# 그래프를 보았을 때, 
# 그래프가 평행하면 교호작용이 없을 가능성이 높다.
# 그래프가 교차하면 교호작용이 있을 가능성이 높다.
# 그래프가 기울기가 다르면 교호작용이 있을 수도 있다. -> 확인해보아야 한다.

# ---------------------------13-2-2 그래프 분석-------------------------------
# 요인 A는 r수준, B는 s수준, 반복 n회로 하여 N = r*s*n 개의 반응치를 랜덤한 
# 순서로 구하였다면 데이터는 다음의 표와 같이 정의된다.
# A1B1 : y111 + y112 + ,,, + y11n = T11. / n = y'11.
# A2B1 : y211 + y212 + ,,, + y21n = T21. / n = y'21.
# ...
# ArB1 : yr11 + yr12 + ,,, + yr1n = Tr1. / n = y'r1.
# ==> 총합 : T11. + T21. + ,,, + Tr1. = T.1. / (r*n) = y'.1.

# A1B1 : y111 + y112 + ,,, + y11n = T11. / n = y'11.
# A1B2 : y121 + y122 + ,,, + y12n = T12. / n = y'12.
# ,,,
# A1Bs : y1s1 + y1s2 + ,,, + y1sn = T1s. / n = y'1s.
# ==> 총합 : T11. + T12. + ,,, + T1s. = T1.. / (s*n) = y'1..

# ==> 전체총합 : 
# T.1. + T.2. + ,,, + T.s. = T1.. + T2.. + ,,, + Tr.. = T(전체합)
# T / (n*s*r) = y'

# 

# 

# ---------------------------13-2-3 제곱합의 분해-------------------------------
# 1. 총편차 = y(ijk) - y' : 각 반응치와 전체평균의 차이
# --> 이를 4개의 아래 수치로 분해한다.

# 2. 요인 A의 수준간 편차 = y'i.. - y' 
#   ( A의 수준은 B의 수준 개수=s와 동일 )

# 3. 요인 B의 수준간 편차 = y'.j. - y' )
#   ( B의 수준은 A의 수준 개수=r과 동일 )

# 4. 교호작용에 의한 편차 = y'ij. - y'i.. - y'.j. + y' 
#   각 수준조합별 평균과 전체평균과의 차이에서 
#   요인 A의 수준간편차와 요인 B의 수준간편차를 제외한 나머지 편차
#   두 요인에서 겹치는 전체편차를 2번 뺐으니 한번 더해준다.

# 5. 수준 내 편차 = yijk - y'ij.
#   각 반응치와 수준조합별 평균의 차 = 간차 = 오차

# 교호작용에 의한 편차는 아래와 같이 AiBj 수준에서 발생하는 
# 편차 중 요인 A의 효과와 요인 B의 효과로 설명되지 않는 나머지 편차를 의미

# (y'ij. - y') - (y'i..-y') - (y'.j. - y') = (y'ij.-y'i.. - y'.j. + y')

# 그러면 다음과 같이 (1)총편차는 (2)요인 A의 수준편차, 
# (3) 요인 B의 수준간편차, (4) 요인 A와 B간의 교호작용에 의한 편차 , 
# (5) 수준내편차(잔차)의 합으로 분해된다. 

# yijk - y' = (y'i..-y') + (y'.j.-y') + (y'ij.-y'i.. - y'.j. + y') 
#               + (yijk - y'ij.)


# 다음으로 총제곱합을 분해하여 정리하면 다음과 같다.

# T = sum(yijk)
# 총제곱합 (SST) = sum(( yijk - y' )^2) = sum( (yijk)^2 ) - T^2/N
# 요인A제곱합(SSA) = sn*sum((y'i..-y')^2) = sum(Ti..^2)/sn - T^2/N
# 요인B제곱합(SSB) = rn*sum((y'.j.-y')^2) = sum(T.j.^2)/rn - T^2/N
# 교호작용제곱합(SSA*B) = SSAB - SSA - SSB
# 오차제곱합(SSE) = sum((yijk - y'ij.)^2) = SST-SSAB
# AB 총제곱합 = n*sum((y'ij.-y')^2) = sum((Tij.)^2/n) - T^2/N

# 다음으로 제곱합에 속한 각 항의 개수에서 제약식의 개수를 제하여
# 각 변동의 자유도를 구한다. 
# φ(T) = N-1 = rsn-1
# φ(A) = r-1 
# φ(B) = s-1
# φ(A*B) = (r-1)(s-1)
# φ(E) = rs(n-1)
# 교호작용의 자유도( φ(A*B) ) 는 φ(A*B) = φ(AB) - φ(A) - φ(B) 로 구한다.
# φ(AB) = rs-1
#  φ(A*B) = rs-1 - (r-1) - (s-1) = rs - r - s + 1 = (r-1)(s-1)

# 이상을 종합하면 다음과 같다.
# φ(T) = φ(AB) + φ(E) = ( φ(A) + φ(B) + φ(A+B) ) + φ(E)


# ---------------------------13-2-4 가설 검정---------------------------------
# 이원 분산분석에서는 다음과 같은 세 가지 가설에 대한 검정을 각각 실시한다.
# 귀무가설 : 요인 A의 수준 변화가 반응치에 미치는 변화가 없다.
# 귀무가설 : 요인 B의 수준 변화가 반응치에 미치는 변화가 없다.
# 귀무가설 : 요인 A와 B의 교호작용이 없다.

# 아래의 분산분석표를 사용하여 세 가지 가설을 검정할 수 있다.
# 구분   제곱합  자유도      평균제곱    검정통계량   기각치
# A      SSA     r-1         MSA         MSA/MSE    > F(1-a/2)(φ(A),φ(E))
# B      SSB     s-1         MSB         MSB/MSE    > F(1-a/2)(φ(B),φ(E))
# A*B    SSA*B   (r-1)(s-1)  MS*B        MSA*B/MSE  > F(1-a/2)(φ(A*B),φ(E))
# E      SSE     rs(n-1)     MSE
# T      SST     rsn-1

# ---------------------------예제 13-4---------------------------------
# 한 화학공정에서 온도 100도, 150도, 200도, 250도 4수준과 압력 1기압,
# 2기압, 3기압에서 랜덤한 순서로 2회 반복 실험하여 얻은 수율 데이터가
# 데이터 : exa13_4 -> 다음과 같다.
# 제곱합을 계산하여 분산분석표를 작성하고, 이 공정에서 온도와 압력의 
# 변화가 수율에 영향을 미치는 지 유의수준 5%에서 검정하시오.

data(exa13_4) ; str(exa13_4) ; attach(exa13_4)
n = 2; r=4; s=3 ; N = n*r*s
x111=76; x112=79; x211=79; x212=81; x311=87; x312=91; x411=79; x412=82
T11.=x111+x112; T21.=x211+x212; T31.=x311+x312; T41.=x411+x412
x121=81; x122=79; x221=84; x222=86; x321=91; x322=94; x421=85; x422=84
T12.=x121+x122; T22.=x221+x222; T32.=x321+x322; T42.=x421+x422
x131=83; x132=85; x231=89; x232=88; x331=88; x332=86; x431=77; x432=76
T13.=x131+x132; T23.=x231+x232; T33.=x331+x332; T43.=x431+x432

# 요인 A에 대한 수준별평균
T1.. = (T11. + T12. + T13.) ; y1.. = T1../(s*n)
T2.. = (T21. + T22. + T23.) ; y2.. = T2../(s*n)
T3.. = (T31. + T32. + T33.) ; y3.. = T3../(s*n)
T4.. = (T41. + T42. + T43.) ; y4.. = T4../(s*n)

# 요인 B에 대한 수준별평균
T.1. = (T11.+T21.+T31.+T41.) ; y.1. = T.1./(r*n)
T.2. = (T12.+T22.+T32.+T42.) ; y.2. = T.2./(r*n)
T.3. = (T13.+T23.+T33.+T43.) ; y.3. = T.3./(r*n)

# 총합
T1..+T2..+T3..+T4.. ; T.1.+T.2.+T.3.
T = T1..+T2..+T3..+T4.. ; y = T/(r*s*n)
T; y # T = 2010 , y = 83.75

Tots = ((x111^2)+(x112^2)+(x211^2)+(x212^2)+(x311^2)+(x312^2)+(x411^2)+(x412^2)+(x121^2)+(x122^2)+(x221^2)+(x222^2)+(x321^2)+(x322^2)+(x421^2)+(x422^2)+(x131^2)+(x132^2)+(x231^2)+(x232^2)+(x331^2)+(x332^2)+(x431^2)+(x432^2)); Tots # [1] 168910
SST = Tots - T^2/(N); SST # [1] 572.5
SSA = sum(T1..^2+T2..^2+T3..^2+T4..^2)/(s*n) - T^2/N; SSA # 328.5
SSB = sum(T.1.^2+T.2.^2+T.3.^2)/(r*n) - T^2/N ; SSB # 57
SSAB = sum(T11.^2+T12.^2+T13.^2+T21.^2+T22.^2+T23.^2+T31.^2+T32.^2+T33.^2+T41.^2+T42.^2+T43.^2)/(n) - T^2/N ; SSAB  # 539.5
SSA_B = SSAB-SSA-SSB; SSA_B # 154
SSE = SST-SSAB ; SSE # 33

MSA = SSA/(r-1); MSA # 109.5
MSB = SSB/(s-1); MSB # 28.5
MSE = SSE/(r*s*(n-1)); MSE # 2.75
MSA_B = SSA_B/((r-1)*(s-1)); MSA_B # 25.66667

FA = MSA/MSE; FA # 39.81818
FB = MSB/MSE; FB # 10.36364
FA_B = MSA_B/MSE; FA_B # 9.333333

?qf
qa = qf(0.95, r-1, r*s*(n-1)); qa # qa 3.490295
qb = qf(0.95, s-1, r*s*(n-1)); qb # qb 3.885294
qa_b = qf(0.95, (r-1)*(s-1), r*s*(n-1)); qa_b # qa_b 2.99612

