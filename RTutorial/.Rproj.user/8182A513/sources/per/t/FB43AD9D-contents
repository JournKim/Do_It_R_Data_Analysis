# dpylr를 이용하지 않고 r 내장 함수로도 데이터 추출을 할 수 있다.
# dplyr이 편하고 빠르지만 R 내장 함수들도 알아두어야 다른 함수들과 조합하여 이용하거나 다른 사용자의 코드를 이해,활용할 수 있다.

exam <- read.csv('csv_exam.csv')

exam[] # 조건없이 전체 데이터 출력 

exam[1,] # 1행 출력

exam[,2] # 2열 출력

exam[exam$class == 1, ] # class가 1인 행 추출

exam[exam$math >= 80, ]

exam[exam$class == 1 & exam$math >= 50, ]

exam[, 'class'] # 내장 함수로 추출할 때 추출 변수가 1개인 경우, 벡터로 나온다. 2개 이상인 경우, 데이터 프레임
                # dplyr로 하면 1개라도 데이터프레임

exam[exam$class == 1, c('class', 'math', 'english')]

# dplyr와 내장 함수의 차이

# 수학점수 50 이상, 영어점수 80 이상인 학생들을 대상으로 각 반의 전 과목 총 평균?

# 내장함수
exam$tot <- (exam$math + exam$english + exam$science)/3
aggregate(data = exam[exam$math >= 50 & exam$english >= 80,], tot~class, mean) # aggregate : 범주별 요약 통계량 구하는 함수
# tot~class, mean : class마다 tot의 평균.

# dplyr
exam %>%
  filter(math>=50 & english >= 80) %>% 
  mutate(tot = (math+english+science)/3) %>% 
  group_by(class) %>% 
  summarise(mean = mean(tot))

# Do It Alone!

# compact와 suv의 도시 및 고속도로 통합 연비 평균

mpg <- as.data.frame(ggplot2::mpg)

mpg$tot <- (mpg$cty + mpg$hwy) / 2
aggregate(data = mpg[mpg$class == 'suv' | mpg$class == 'compact',], tot~class, mean)


# 변수의 종류

#1 Numeric 타입 : 숫자. 크기를 의미, 산술 연산 가능
#2 Factor 타입, 명목 변수(Nominal Variable) : 값이 분류하는 의미를 지니는 변수. ex) 남자-1, 여자-2, 지역 등등

var1 <- c(1,2,3,1,2)
var2 <- factor(c(1,2,3,1,2))

var1
#[1] 1 2 3 1 2

var2
# [1] 1 2 3 1 2
# Levels: 1 2 3

var1+2
# [1] 3 4 5 3 4

var2+2
# [1] NA NA NA NA NA
# Warning message:
#   In Ops.factor(var2, 2) : 요인(factors)에 대하여 의미있는 ‘+’가 아닙니다.

levels(var2)

var3 <- c('a','b','b','c')
var4 <- factor(c('a','b','c','d'))

class(var3) # character
class(var4) # factor

# as.numeric(), as.factor(), as.character(), as.Date(), as.data.frame() : 변환 함수.

# numeric : 실수
# integer : 정수
# complex : 복소수  3 + 2i
# character : 문자
# logical : 논리
# factor : 범주
# Date : 날짜

# 혼자 해 보기

class(mpg$drv) # character
mpg$drv <- as.factor(mpg$drv)
class(mpg$drv) # factor
levels(mpg$drv)


# 15-3 데이터 구조

# Vector : 1차원, 한가지 변수 타입
# Data Frame : 2차원, 다양한 변수 타입
# Matrix : 2차원, 한가지 변수 타입
# Array : 다차원, 2차원 이상의 매트릭스
# List : 다차원, 서로 다른 데이터 구조 포함
