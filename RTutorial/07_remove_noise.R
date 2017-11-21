
df <- data.frame(sex = c('M','F',NA,'M','F'),
                 score = c(5,4,3,4,NA))

is.na(df)

table(is.na(df))

table(is.na(df$sex))
table(is.na(df$score))

mean(df$score) # NA가 포함되어 결과도 NA
mean(df$score, na.rm = TRUE) # na.rm : 계산 결과에 NA를 제외하고 함수를 적용
                            #함수가 na.rm을 지원하지 않는 경우 filter를 이용해야 한다.

# NA 제거

df_nomiss <- df %>% filter(!is.na(sex) & !is.na(score))

df_nomiss2 <- na.omit(df) # 모든 변수에 NA가 없는 데이터 추출


exam <- read.csv('csv_exam.csv')
exam[c(3,8,15), 'math'] <- NA # math의 3,8,15번째 데이터를 NA로 바꿈.

exam %>% summarise(mean_math = mean(math, na.rm = T))

# 결측치(NA) 대체하기

mean_math = mean(exam$math, na.rm = T)

exam$math <- ifelse(is.na(exam$math), mean_math, exam$math)

table(is.na(exam$math))

# 혼자서 해 보기

mpg <- mpg_raw
mpg[c(65, 124, 131, 153, 212), "hwy"] <- NA

#1 drv와 hwy 변수에 결측치가 몇 개 있는지 확인

table(is.na(mpg$drv))
table(is.na(mpg$hwy))

#2 filter를 이용해 hwy의 결측치를 제외하고, 어떤 구동 방식의 평균이 높은지 확인.

mpg %>% 
  filter(!is.na(hwy)) %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(desc(mean_hwy))


# 07-2 remove Outlier

outlier <- data.frame(sex = c(1,2,1,3,2,1), # sex : 1, 2
                      score = c(5,4,3,4,2,6)) # score : 1~5

outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)

outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)

outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))

    # 극단값 제거

boxplot(mpg$hwy)
# 상자 아래 세로 점선 : 하위 0~25%
# 상자 밑면 : 1사분위수 (Q1)
# 상자 안 굵은 선 : 중앙값 ( 2사분위수)
# 상자 윗면 : 3사분위수 (Q3)
# 상자 위 세로 점선 : 하위 75~100%
# 상자 밖 가로선 : 극단값 경계 , Q1, Q3로부터 1.5IQR 내 최대값. (IQR = Q3-Q1 )

boxplot(mpg$hwy)$stats # 아래 극단값 경계, Q1, Q2, Q3, 위 극단값 경계 순으로 출력.

# 아래 위 극단값이 12, 37 이므로 이 범위를 벗어난 값을 결측 처리
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>%
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))


# 혼자서 해보기
mpg <- mpg_raw
mpg[c(10,14,58,93), 'drv'] <- 'k'
mpg[c(29,43,129,203), 'cty'] <- c(3,4,39,42)

#1 drv에 이상치가 있는지 확인. 이상치를 결측처리한 후 이상치가 사라졌는지 확인. 결측처리시 %in% 기호 활용

mpg$drv <- ifelse(mpg$drv %in% c('4','f','r'), mpg$drv, NA)
table(is.na(mpg$drv))

#2 상자 그림을 이용해 cty에 이상치가 있는지 확인. 정상 범위를 벗어난 값을 결측처리 후 다시 상자그림으로 이상치가 사라졌는지 확인

boxplot(mpg$cty)$stat
mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)
boxplot(mpg$cty)$stat

#3 이상치를 제외한 후 drv별로 cty평균이 어떻게 다른지 확인

mpg %>% 
  filter(!is.na(drv) & !is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(mean_cty = mean(cty))
