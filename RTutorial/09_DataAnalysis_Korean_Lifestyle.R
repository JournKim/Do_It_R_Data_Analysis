# bit.ly/doit_rb
#데이터는 SPSS 전용 파일로 되어있다. (sav 파일)
# foreign 패키지를 이용하면 불러올 수 있다.
install.packages('foreign')
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

str_filename = 'Koweps_hpc10_2015_beta1.sav' # 한국복지패널데이터 

raw_welfare <- read.spss(file = 'Koweps_hpc10_2015_beta1.sav',
                         to.data.frame = T)

welfare <- raw_welfare

head(welfare)
dim(welfare)
str(welfare)
summary(welfare)

welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)

# 분석 절차
#   1. 변수 검토 및 전처리
#   2. 변수 간 관계 분석.


# 9-2 성별에 따른 월급 차이

class(welfare$sex) # 타입 파악
table(welfare$sex) # -> 1 또는 2의 값을 가지는 것을 확인 (코드북에 따르면 1 : 남, 2 : 여)
# 이상치가 있다면 처리하는 과정을 거쳐야 한다.
#ex) welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex) ...

# 구분하기 쉽게 male 과 female로 바꾸어 준다.
welfare$sex <- ifelse(welfare$sex == 1, 'male', 'female')
table(welfare$sex)

#코드북에 따르면 income : 일한 달의 월 평균 임금 (단위 만원, 범위 1~9998, 모름/무응답 9999)
class(welfare$income)
summary(welfare$income) # 최소 0, 최대 2400. NA가 많다.(12030개) : 월급을 받지 않는 사람들.

# 값이 0이거나 9999이면 결측 처리.
welfare$income <- ifelse(welfare$income %in% c(0,9999), NA, welfare$income)

table(is.na(welfare$income)) # 4620개의 월급 데이터만 남음.

# 성별에 따른 월급 평균표 만들기.

sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))
sex_income # 여자 163.247, 남자 312.293.

#그래프로 표현
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()


# 9-3 나이와 월급의 관계

#나이 변수 검토 및 전처리

class(welfare$birth)
summary(welfare$birth) # 1907~2014, NA 없음. 9999없음(무응답/모름)

qplot(welfare$birth)

#파생 변수 만들기 (나이)

#2015년 기준 조사이므로 2015-birth + 1

welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)

# 나이에 따른 평균 월급 만들기

age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))

ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()


# 9-4 연령대에 따른 월급의 차이

# <30 : young
# <=59 : middle
# >60 : old

#연령대 변수 추가
welfare <- welfare %>% 
  mutate(ageg = ifelse(age < 30, 'young', 
                       ifelse(age <= 59, 'middle', 'old')))

table(welfare$ageg) # young-4334, middle-6049, old-6281


#연령대에 따른 월급

ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))

ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) +
  geom_col() +
  scale_x_discrete(limits = c('young', 'middle', 'old')) # x축 순서 지


# 9-5 연령대 및 성별 월급 차이

# 변수 : 연령대, 성별, 월급

sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg,sex) %>% 
  summarise(mean_income = mean(income))

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) + # 한 막대 안에서 sex로 구분지어짐
  geom_col()+
  scale_x_discrete(limits = c('young', 'middle', 'old'))

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = 'dodge')+ # 연령대별로 성별 막대가 각각 생김. ( position 기본값이 stack임.)
  scale_x_discrete(limits = c('young', 'middle', 'old'))


# 직접 해 보기 : 나이 및 성별 월급 평균표, 선그래프

sex_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))

ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex))+ # 성별간 색이 구분됨.
  geom_line()

# 9-6 직업별 월급 차이

#변수 : 직업, 월급

class(welfare$code_job)

table(welfare$code_job)

# 직업 코드별 이름을 엑셀 파일에서 가져 온다.

list_job <- read_excel('Koweps_Codebook.xlsx', col_names = T, sheet = 2)

head(list_job)

welfare <- left_join(welfare, list_job, by='code_job')

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

job_income <- welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))

head(job_income)

top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income))+
  geom_col()+
  coord_flip() # x,y축 바꾼다.

bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)

ggplot(data = bottom10, aes(x = reorder(job, -mean_income),
                            y = mean_income)) +
  geom_col()+
  coord_flip()+
  ylim(0,850) # top10과 비교를 위해 범위를 850까지 설정


# 9-7 성별 직업 빈도

#변수 : 성별, 직업

job_male <- welfare %>% 
  filter(!is.na(job) & sex == 'male') %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

job_female <- welfare %>% 
  filter(!is.na(job) & sex == 'female') %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

ggplot(data = job_male, aes(x = reorder(job, n), y = n))+
  geom_col()+
  coord_flip()

ggplot(data = job_female, aes(x = reorder(job, n), y = n)) +
  geom_col()+
  coord_flip()

# 9-8 종교 유무에 따른 이혼율

# 변수 : 종교(1 : 있음, 2 : 없음), 혼인상태(0,5,9를 제외한 데이터 중 3의 비율)

table(welfare$religion) # 이상치 없음, 결측 없음.

welfare$religion <- ifelse(welfare$religion == 1, 'yes', 'no')

table(welfare$marriage) # 9 없음, 1: 배우자 있음. 3 : 이혼

welfare$group_marriage <- ifelse(welfare$marriage == 1, 'marriage',
                                 ifelse(welfare$marriage == 3, 'divorce', NA))

table(welfare$group_marriage)

religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))

religion_marriage2 <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(religion, group_marriage) %>%  # count를 통해 n을 한 번에 구함.
  group_by(religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1)) # 결과는 위의 것과 같음.

divorce <- religion_marriage %>% 
  filter(group_marriage == 'divorce') %>% 
  select(religion, pct)

ggplot(data = divorce, aes(x = religion, y = pct))+
  geom_col()

# 직접 해보세요 : 연령대 및 종교 유무에 따른 이혼율

ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg,group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))

# 중년과 노년의 이혼을 나타내는 값만 추출해 그래프 만들기
ageg_divorce <- ageg_marriage %>% 
  filter(group_marriage == 'divorce') %>% 
  select(ageg, pct)

ggplot(data = ageg_divorce, aes(x = ageg, y = pct))+
  geom_col()+
  scale_x_discrete(limit = c('young', 'middle', 'old'))


# 연령대 및 종교 유무에 따른 이혼율

ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))

df_divorce <- ageg_religion_marriage %>% 
  filter(group_marriage == 'divorce') %>% 
  select(ageg, religion, pct)

ggplot(data = df_divorce, aes(x = ageg, y = pct, fill = religion))+
  geom_col(position = 'dodge')


#9-9 지역별 연령대 비율 - 노년층이 많은 지역은 어디일까?

#변수 : 지역, 연령대

list_region <- data.frame(code_region = c(1:7),
                          region = c('서울',
                                     '수도권(인천/경기)',
                                     '부산/경남/울산',
                                     '대구/경북',
                                     '대전/충남',
                                     '강원/충북',
                                     '광주/전남/전북/제주도'))
# 지역 코드와 지역명 조인.
welfare <- left_join(welfare, list_region, by='code_region')

region_ageg <- welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 2))

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg))+
  geom_col()+
  coord_flip()

# 노년층 비율 높은 순으로 막대 정렬하기

list_order_old <- region_ageg %>% 
  filter(ageg == 'old') %>% 
  arrange(pct)

order <- list_order_old$region

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg))+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limits = order)

# 연령대 순으로 막대 색깔 나열하기

class(region_ageg$ageg) # 현재 class가 character 타입이기 때문에 level이 없어 순서가 없는 것 같다.
levels(region_ageg$ageg) # NULL

region_ageg$ageg <- factor(region_ageg$ageg, level = c('old','middle','young'))
class(region_ageg$ageg) # factor
levels(region_ageg$ageg) # old - middle - young

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg))+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limits = order)

