
#지역별 통계치를 색깔의 차이로 표현한 지도 : 단계 구분도 ( Choropleth Map )

install.packages('ggiraphExtra')
library(ggiraphExtra)

# USArrest : 1973년 미국 주별 강력범죄율 정보, R 내장

str(USArrests)

# 지역명 변수가 따로 없고, 행 이름이 지역명.
library(tibble)

crime <- rownames_to_column(USArrests, var = 'state')
head(crime)
crime$state <- tolower(crime$state) # 소문자로 저장 : maps 내의 state가 주 이름을 소문자로 저장.

str(crime)

# 지도 데이터 준비

# R 내장 maps 패키지에 주별 위경도를 나타낸 state 데이터가 있다.

library(ggplot2)
states_map <- map_data('state')

#단계 구분도 만들기

ggChoropleth(data = crime,
             aes(fill = Murder,
                 map_id = state),
             map = states_map,
             interactive = T)



# 11-2 대한민국 시도별 인구, 결핵 환자 수 단계 구분도.

#kormaps2014 패키지에 필요한 stringi 패키지 설치
install.packages('stringi')
# devtool의 install_github를 이용해 kormaps2014 설치
install.packages('devtools')
devtools::install_github('cardiomoon/kormaps2014')

library(kormaps2014)
# korpop1 : 2015 센서스 데이터(시도별)
# korpop2 : (시군구별)
# korpop3 : 읍면동별

# korpop1 데이터의 인코딩이 utf-8 이어서 한글이 깨져 보인다.
# kormaps2014 패키지의 changeCode()로 cp949로 변환 후 str() 하면 한글이 깨지지 않고 출력된다
# str(korpop1)
str(changeCode(kormaps2014::korpop1))

korpop <- kormaps2014::korpop1

# 변수명이 한글이면 오류 발생 가능성. 
library(dplyr)
korpop <- rename(korpop,
                  pop = 총인구_명,
                  name = 행정구역별_읍면동)
# kormap1 : 2014 한국 행정 지도(시도별)
# kormap2 : 시군구별
# kormap3 : 읍면동별


library(ggiraphExtra)
library(ggplot2)

# korpop의 name이 인코딩이 utf-8이라는데, 그래서 지도에서 깨지는듯.
# CP949로 바꾸면 지도를 못만듦. 어떻게 해야되죠?
# p287


ggChoropleth(data = korpop,        # 지도에 표현할 데이터
             aes(fill = pop,        # 색깔로 표현할 변수
                 map_id = code,     # 지역 기준 변수
                 tooltip = name),   # 지도 위에 표시할 지역명
             map = kormap1,         # 지도 데이터
             interactive = T)       # 인터랙티브


# 시도별 결핵환자수 단계 구분도
# tbc : 지역별 결핵 환자수 포함. tbc$NewPts : 결핵 환자수

str(tbc)

ggChoropleth(data = tbc,
             aes(fill = NewPts,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)
