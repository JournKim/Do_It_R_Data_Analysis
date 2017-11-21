exam <- df_exam

head(exam) # 6행까지 출력
head(exam, 10) # 지정한 행 까지 출력

tail(exam) # 뒤의 6행 출력
tail(exam, 10) # 지정한 만큼 출력

View(exam) # 뷰어에서 데이터 확인

dim(exam) # dimensions 약어 : 행, 열 출력

str(exam) #데이터의 변수들의 속성 출력

summary(exam) # 요약 통계량 출력 (Min, 1st Qu(1사분위수, 하위 25%지점), Median, Mean, 3rd Qu(3사분위수, 상위25% 지점), Max)


mpg <- as.data.frame(ggplot2::mpg) # what is difference between as.data.frame and data.frame?

dim(mpg)
head(mpg)
str(mpg)
summary(mpg)

#dplyr : 데이터 가공시 사용하는 패키지
library(dplyr)

df_raw <- data.frame(var1 = c(1,2,1),
                     var2 = c(2,3,2))

df_new <- df_raw

df_new <- rename(df_new, v2 = var2)

mpg_raw <- as.data.frame(ggplot2::mpg)

mpg <- mpg_raw

mpg <- rename(mpg, city = cty, highway = hwy)

head(mpg)



# 파생변수 생성

df <- data.frame(var1 = c(4,3,8),
                 var2 = c(2,6,1))
df$var_sum <- df$var1 + df$var2

df$var_mean <- (df$var_sum)/2


# mpg 통합연비 변수 생성
mpg$total <- (mpg$city + mpg$highway) / 2

mean(mpg$total)


# 조건문 활용 파생변수 생성 : 고연비 합격 자동차

summary(mpg$total)
hist(mpg$total) # 20~25가 많고, 25 이상은 별로 없음. -> 20을 합격 기준으로 설정

mpg$test <- ifelse(mpg$total>=20, "pass", "fail")

#합불 빈도 보기
table(mpg$test)

#등급 나누기

mpg$grade <- ifelse(mpg$total >= 30, 'A', ifelse(mpg$total>=20, 'B', 'C'))

table(mpg$test, mpg$grade)

qplot(mpg$grade)


#practice!!

#ggplot2::midwest : 미국 동북중부 지역의 인구통계정보.

#1. 데이터를 불러와서 특징 파악
#2 poptotal -> total, popasian->asian
#3. total, asian -> 전체 중 아시아인 백분율, 히스토그램 확인
#4. 아시아인 백분율 전체 평균을 구하고, 평균 초과->large, 이하->small
#5. large, small 빈도표와 막대그래프

midwest_raw <- as.data.frame(ggplot2::midwest)
midwest <- midwest_raw
head(midwest)
str(midwest)

midwest <- rename(midwest, total = poptotal, asian = popasian)
str(midwest)

midwest$aisan_ratio <- (midwest$asian / midwest$total)*100
midwest$aisan_ratio

midwest <- rename(midwest, asian_ratio = aisan_ratio)
hist(midwest$asian_ratio)

mean_asian <- mean(midwest$asian_ratio)

midwest$over_mean <- ifelse(midwest$asian_ratio>mean_asian, "large", "small")
View(midwest)

table(midwest$over_mean)
qplot(midwest$over_mean)

     