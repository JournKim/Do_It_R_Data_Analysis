
# 기술 통계 : 데이터를 요약해 설명하는 통계 기법
# 추론 통계 : 숫자 요약을 넘어 어떤 값이 발생할 확률을 계산하는 통계 기법
# 
# ex) 성별에 따라 월급 차이가 있는 것으로 나타났을 때, 이런 차이가 발생할 확률을 계산.
#     이런 차이가 우연히 나타날 확률이 작다면 통계적으로 유의하다고 결론.

# 통계적 가설 검정(Statistical Hypothesis) : 유의 확률을 이용해 가설을 검정하는 방법
# 유의확률 (Significance probability, p-value) : 실제로 차이가 없는데 우연히 차이가 있는 데이터가 추출될 확률


# 13-2 t-검정: 두 집단의 평균 비교 - 두 집단의 평균에 통계적으로 유의미한 차이가 있는지 알아볼 때 사용
# R 내장 t.test() 이용.
library(dplyr)
library(ggplot2)

mpg <- as.data.frame(ggplot2::mpg)
mpg_diff <- mpg %>%
  select(class, cty) %>% 
  filter(class %in% c('compact', 'suv'))

head(mpg_diff)

table(mpg_diff$ class)

# 비교할 값 cty ~ 비교할 집단 class
# 비교할 집단의 분산이 같은지 여부에 따라 공식이 달라짐. 여기서는 두 집단의 분산이 같다고 가정
t.test(data = mpg_diff, cty ~ class, var.equal = T) # p-value = 2.2e-16 : 일반적으로 0.05 미만이면 유의미하다고 판단

# sample estimates:    ##################### 'suv'보다 'compact'의 도시 연비가 더 높다.
#   mean in group compact     mean in group suv
#               20.12766              13.50000


# 일반 휘발유와 고급 휘발유의 도시 연비 t-test

mpg_diff2 <- mpg %>% 
  select(fl, cty) %>% 
  filter(fl %in% c('r', 'p')) # r : regular, p: premium

table(mpg_diff2$fl) # p: 52, r:168

t.test(data = mpg_diff2, cty ~ fl, var.equal = T)

# data:  cty by fl
#   t = 1.0662, df = 218, p-value = 0.2875   -> p-value가 0.05보다 크다. 통계적으로 유의미하지 않다.
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.5322946  1.7868733
# sample estimates:
#   mean in group p   mean in group r 
#         17.36538          16.73810 



# 13-3 상관분석 (Corellation Analysis) : 두 변수의 관계성 분석 - 두 연속 변수가 서로 관련이 있는지 검정
# 상관분석을 통해 상관계수 도출. 0~1의 값을 지니고, 1에 가까울 수록 관련성이 크다. 양수이면 비례, 음수이면 반비례

# economic 데이터를 이용해 unemploy와 pce(개인 소비 지출) 간의 관계 분석.

economics <- as.data.frame(ggplot2::economics)
cor.test(economics$unemploy, economics$pce)

################################################################
# Pearson's product-moment correlation
# 
# data:  economics$unemploy and economics$pce
# t = 18.605, df = 572, p-value < 2.2e-16                       p-value <<<<<< 0.05 : 유의미한 관계.
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.5603164 0.6625460
# sample estimates:
# cor 
# 0.6139997                   # 상관계수가 0.614 : 정비례 관계.


# 상관행렬 히트맵 만들기 : 여러 변수의 관련성을 한 번에 알아볼 때.
# R 내장 mtcars데이터 이용 (자동차 32종의 11개 속성에 대한 정보)

#cor()를 이용하면 상관행렬을 만들 수 있다.

head(mtcars)

car_cor <- cor(mtcars) # 상관행렬 생성
round(car_cor, 2) # 소수점 2째 자리까지 출력
########### Result ######################################################
#        mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
# mpg   1.00 -0.85 -0.85 -0.78  0.68 -0.87  0.42  0.66  0.60  0.48 -0.55
# cyl  -0.85  1.00  0.90  0.83 -0.70  0.78 -0.59 -0.81 -0.52 -0.49  0.53
# disp -0.85  0.90  1.00  0.79 -0.71  0.89 -0.43 -0.71 -0.59 -0.56  0.39
# hp   -0.78  0.83  0.79  1.00 -0.45  0.66 -0.71 -0.72 -0.24 -0.13  0.75
# drat  0.68 -0.70 -0.71 -0.45  1.00 -0.71  0.09  0.44  0.71  0.70 -0.09
# wt   -0.87  0.78  0.89  0.66 -0.71  1.00 -0.17 -0.55 -0.69 -0.58  0.43
# qsec  0.42 -0.59 -0.43 -0.71  0.09 -0.17  1.00  0.74 -0.23 -0.21 -0.66
# vs    0.66 -0.81 -0.71 -0.72  0.44 -0.55  0.74  1.00  0.17  0.21 -0.57
# am    0.60 -0.52 -0.59 -0.24  0.71 -0.69 -0.23  0.17  1.00  0.79  0.06
# gear  0.48 -0.49 -0.56 -0.13  0.70 -0.58 -0.21  0.21  0.79  1.00  0.27
# carb -0.55  0.53  0.39  0.75 -0.09  0.43 -0.66 -0.57  0.06  0.27  1.00

# mpg-cyl -0.85 : 연비가 높을수록 실린더 수가 적은 경향.
# cyl-wt 0.78 : 실린더가 많을수록 무게가 무거운 경향.

# corrrplot 패키지의 corrplot()을 이용해 상관행렬을 히트맵으로 만들면 관계를 쉽게 파악 가능.
# 상관계수가 클 수록 원의 크기가 크로 색이 진하다. 양수면 파란색, 음수면 빨간색 계열.
install.packages('corrplot')
library(corrplot)

corrplot(car_cor)

corrplot(car_cor, method = 'number') # 숫자로 표시

col <-colorRampPalette(c('#BB4444', '#EE9988', '#FFFFFF', '#77AADD', '#4477AA'))

corrplot(car_cor,
         method = 'color',       # 색깔로 표현
         col = col(200),         # 색상 200개 선정
         type = 'lower',         # 왼쪽 아래만 표시
         order = 'hclust',       # 유사한 상관계수끼리 군집화
         addCoef.col = 'black',  # 상관계수 색깔
         tl.col = 'black',       # 변수명 색깔
         tl.srt = 45,            # 변수명 45도 기울임
         diag = F)               # 대각 행렬 제외
