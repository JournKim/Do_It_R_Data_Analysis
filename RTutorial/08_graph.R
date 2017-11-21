
library(ggplot2)
#step1 배경 설정(축)
#step2 그래프 추가 (점, 막대, 선)
#step3 설정 추가 (축 범위, 색, 표식)


#산점도 (Scater Plot)

mpg = as.data.frame(ggplot2::mpg)

ggplot(data = mpg, aes(x = displ, y = hwy)) # step1

ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() # step1 + step2

ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  xlim(3,6) + ylim(10,30) # step1 + step2 + step3

#혼자 해보기

#1 mpg데이터의 cty와 hwy 간의 관계. x축 cty, y축 hwy로 된 산점도

ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_point()

#2 midwest데이터를 이용, 전체 인구와 아시아인 인구 관계 확인. x축 poptotal, y축 popasian, 전체 인구 50만 이하, 아시아인 1만 이하 지역만 표시

midwest <- as.data.frame(ggplot2::midwest)

ggplot(data = midwest, aes(x = poptotal, y = popasian)) +
  geom_point() +
  xlim(0,500000) +
  ylim(0,10000) # r에서는 10만 단위가 넘는 수는 지수 표기법으로 표시. 정수로 표시 하려면 options(scipen = 99) 하면 됨. scipen = 0 하면 다시 지수표기법 가능. 재부팅시 원상복구

# 막대그래프 (Bar Chart)

library(dplyr)

df_mpg <- mpg %>% #구동 방식별 평균 고속도로 연비
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))

ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col() # 평균막대그래프
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col() # 크기순 정렬 (내림차순, -가 없으면 오름차순)

# drv 빈도 그래프

ggplot(data = mpg, aes(x = drv)) + geom_bar() # 빈도 막대그래프. y축 값이 없다.
ggplot(data = mpg, aes(x = hwy)) + geom_bar() # hwy 빈도 그래프

#혼자 해 보기

#1 어떤 회사의 suv가 cty가 높은지 확인. suv의 평균 cty가 높은 5개의 회사 막대그래프. 연비 높은 순 정렬

df_suv_cty <- mpg %>% 
  filter(class == 'suv') %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)

ggplot(data = df_suv_cty, aes(x = reorder(manufacturer, -mean_cty), y = mean_cty)) + geom_col()

#2 어떤 class(차종)이 많은지 빈도그래프.

ggplot(data = mpg, aes(x = class)) + geom_bar()


# 선 그래프 (Line Chart) - 시간에 따라 달라지는 데이터 표현 ( Time Series Chart )

economics <- as.data.frame(ggplot2::economics)

ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()

# do it alone

#1 psavert의 시간에 따라 어떻게 변해 왔는지

ggplot(data = economics, aes(x = date, y = psavert)) + geom_line()

 # 상자 그림 ( Box Plot ) : 데이터 분포를 상자를 이용해 표현

ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

### 치트 시트 : Help - Cheatsheets -> Data Visualization with ggplot2 에서 자주 사용하는 기능들 확인 가능
### 다른 사용자들의 그래프 및 코드 : bit.ly/2s5cmdc
### ggplot2 확장 패키지 : bit.ly/2qpzga

# do it alone

#1 class가 compact, subcompact, suv인 차들의 cty가 어떻게 다른지 비교. 상자그래프 ㄱㄱ

df_css <- mpg %>% filter(class %in% c('compact', 'subcompact', 'suv'))

ggplot(data = df_css, aes(x = class, y = cty)) + geom_boxplot()


