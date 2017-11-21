
# 12-1 plotly 패키지로 인터랙티브 그래프 만들기

install.packages('plotly')
library(plotly)

# 경고 메시지에 따라
library(devtools)
install_github('hadley/ggplot2')

library(ggplot2)

# mpg 데이터를 이용해 x축에 displ, y축에 hwy를 지정해 산점도 생성, col = drv (구동방식에 따라 다른 색)
p <- ggplot(data = mpg, aes(x = displ, y = hwy, col = drv)) + geom_point()

ggplotly(p)

# Viewer에서 HTML로 저장 가능.

# 막대 그래프 만들기 : ggplot2::diamonds 데이터를 이용.

p2 <- ggplot(data = diamonds, aes(x = cut, fill = clarity))+
  geom_bar(position = 'dodge')
ggplotly(p2)


# 12-2 dygraphs 패키지로 인터랙티브 시계열 그래프 만들기

install.packages('dygraphs')
library(dygraphs)

economics <- ggplot2::economics
head(economics)

#dygraphs를 이용해 시계열 그래프를 만드려면 데이터가 시간 순서 속성을 지니는 xts 타입으로 되어있어야 한다.
library(xts)
eco <- xts(economics$unemploy, order.by = economics$date)

dygraph(eco)

# %>% 를 이용해 dyRangeSelector()를 추가하면 날짜 범위 선택 기능이 추가 된다.
dygraph(eco) %>% dyRangeSelector()

# 여러 값 표현하기

eco_a <- xts(economics$psavert, order.by = economics$date) # 저축률
eco_b <- xts(economics$unemploy/1000, order.by = economics$date) # 실업자 수

# cbind()를 이용하여 두 데이터를 가로로 결합

eco2 <- cbind(eco_a, eco_b)
colnames(eco2) <- c('psavert', 'unemploy')
head(eco2)

dygraph(eco2) %>% dyRangeSelector()
