
library(dplyr)

#filter : 행추출
#select : 열 추출
# arrange : 정렬
# mutate : 변수 추가
# summarise : 통계치 산출
# group_by : 집단별로 나누기
# left_join : 데이터 합치기(열)
# bind_rows : 데이터 합치기(행)

#####################  filter

exam_raw <- read.csv("csv_exam.csv")

exam <- exam_raw

# dplyr 패키지는 %>% (파이프 연산자) 를 이용해 함수들을 나열. Ctrl + Shift + M

exam %>% filter(class == 1) # 1반 학생들만 나열

exam %>% filter(class == 1 & math >= 50)
exam %>% filter(class ==1 | class == 2 | class == 3)
exam %>% filter(class %in% c(1,2,3))

# ^ or ** : 거듭제곱
# %/% : 나눗셈의 몫
# %% : 나눗셈의 나머지

##################### select

exam %>% select(math)

exam %>% select(class, math, english)

exam %>% select(-math, -english)

#################### combination

exam %>% filter(class == 1) %>% select(english)

exam %>% 
  filter(class == 1) %>% 
  select(english)

exam %>%
  select(id, math) %>% 
  head # 6행까지 추출

exam %>% 
  select(id, math) %>% 
  head(10) # 10행까지 추출

#################### arrange

exam %>% arrange(math)

exam %>% arrange(desc(math))

exam %>% arrange(class, math)

exam %>% mutate(total = math+english+science) %>% head

exam %>% mutate(total = math + english + science,
                mean = total/3) %>% head

exam %>% mutate(test = ifelse(science >= 60 , "pass", "fail")) %>% head

exam %>% mutate(total = math+english+science) %>% arrange(total) %>% head

exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math))

exam %>%
  group_by(class) %>% 
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math), # sd(x) : 표준편차
            n = n())

test1 <- data.frame(id = c(1,2,3,4,5),
                    midterm = c(60,80,70,90,85))

test2 <- data.frame(id = c(1,2,3,4,5),
                    final = c(70, 83, 65, 95, 80))

total <- left_join(test1, test2, by="id")

name = data.frame(class = c(1,2,3,4,5),
                  teacher = c('kim','lee','park','choi', 'jung'))

exam_new <- left_join(exam,name,by='class')

group_a <- data.frame(id= c(1,2,3,4,5),
                      test = c(60,80,70,90,85))

group_b <- data.frame(id = c(6,7,8,9,10),
                      test = c(70, 83, 65, 95, 80))

group_all <- bind_rows(group_a, group_b)


a
library(ggplot2)


mpg_raw <- as.data.frame(ggplot2::mpg)
mpg <- mpg_raw

low <- mpg %>% filter(displ<=4)
high <- mpg %>% filter(displ>=5)

mean_low <- mean(low$hwy)
mean_high <- mean(high$hwy)

audi <- mpg %>% filter(manufacturer == 'audi')
toyota <- mpg %>% filter(manufacturer == 'toyota')

mean_cty_audi <- mean(audi$cty)
mean_cty_toyota <- mean(toyota$cty)

cfh <- mpg %>% filter(manufacturer %in% c('chevrolet', 'ford', 'honda'))
mean_hwy_cfh <- mean(cfh$hwy)

d <- mpg %>% select(class, cty)
suv <- d %>% filter(class == "suv")
compact <- d %>% filter(class == "compact")
mean_suv <- mean(suv$cty)
mean_compact <- mean(compact$cty)

audi %>% arrange(desc(hwy)) %>% head(5)

mpg_cpy <- mpg
mpg_cpy <- mpg_cpy %>% mutate(tot = hwy + cty, avg = tot/2)

mpg_cpy %>% arrange(desc(avg)) %>% head(3)

mpg %>% mutate(tot = hwy+cty, avg = tot/2) %>% arrange(desc(avg)) %>% head(3)

mpg %>%
  group_by(manufacturer, drv) %>%  # drv -> 4:사륜구동, f : 전륜구동, r : 후륜구동
  summarise(mean_cty = mean(cty)) %>% 
  head(10)

mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == "suv") %>% 
  mutate(tot = (cty+hwy)/2) %>% 
  summarise(mean_tot = mean(tot)) %>% 
  arrange(desc(mean_tot)) %>% 
  head(5)

mpg %>%
  group_by(class) %>% 
  summarise(mean_cty = mean(cty))

mpg %>%
  group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty))

mpg %>%
  group_by(manufacturer) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(mean_hwy) %>% 
  head(3)

mpg %>%
  group_by(manufacturer) %>% 
  filter(class == "compact") %>% 
  summarise(n_compact = n()) %>% 
  arrange(desc(n_compact))
 
fuel <- data.frame(fl=c('c', 'd', 'e', 'p', 'r'),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F) # T일 경우 문자를 factor type으로 자동 변환한다. mpg와 동일하게 chr타입으로 만들기 위해 사용.

mpg <- left_join(mpg, fuel, by='fl')
mpg %>% select(model, fl, price_fl) %>% head(5)



################### practice

midwest <- as.data.frame(ggplot2::midwest)

midwest <- midwest %>% mutate(nonadults = (poptotal - popadults)/poptotal * 100)

head(midwest)

midwest %>% select(county, nonadults) %>% arrange(desc(nonadults)) %>% head(5)

midwest <- midwest %>% mutate(grade = ifelse(nonadults>=40, 'large', ifelse(nonadults >= 30, 'middle', 'small')))

midwest %>% group_by(grade) %>% summarise(n_grade = n())

midwest <- midwest %>% mutate(asian_ratio = popasian/poptotal*100)

midwest %>% select(state, county, asian_ratio) %>% arrange(asian_ratio) %>% head(10)
