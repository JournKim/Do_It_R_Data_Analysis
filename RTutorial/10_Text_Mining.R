#10-1 힙합가사 텍스트 마이닝

# KoNLP
install.packages('rJava')
install.packages('memoise')
install.packages('KoNLP')

library(KoNLP)
library(dplyr)

library(readxl)

# NIA 사전 : 98만여개의 단어 포함.
useNIADic()

txt <- readLines('hiphop.txt')
head(txt)

SongList <- read_excel('SongList.xlsx')
head(SongList)

# 문자처리 패키지 stringr
install.packages('stringr')
library(stringr)

#특수문자 제거
txt <- str_replace_all(txt, "\\W", " ") # '\\W' : 특수문자를 의미하는 정규 표현식
head(txt)


#명사 추출

extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다.")

nouns <- extractNoun(txt)

unlist(nouns) # extractNoun은 결과를 list로 반환. unlist()로 리스트를 벡터로 바꿔줄 수 있다.
wordcount <- table(unlist(nouns)) # table()하여 각 단어의 개수를 센다.

#데이터 프레임으로 변환
df_word <- as.data.frame(wordcount, stringsAsFactors = F)

# 변수명 수정
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)
head(df_word)

# 한 글자로 된 단어는 의미 없는 경우가 많기 때문에 제거.

df_word <- filter(df_word, nchar(word)>=2)

# 가장 많이 사용된 20가지 가사
top20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)


# 워드 클라우드 만들기

install.packages('wordcloud')

library(wordcloud)
library(RColorBrewer) # R 내장, 글자 색깔을 표현하는 데 사용

# Dark2 색상 목록에서 8개 색상 추출
pal <- brewer.pal(8, 'Dark2')

# wordcloud는 함수를 실행할 때마다 난수를 이용해 매번 다른 모양의 워드 클라우드를 생성.
# 항상 같은 클라우드를 생성하도록 난수를 고정
set.seed(1234)

wordcloud(words = df_word$word, # 단어
          freq = df_word$freq,  # 빈도
          min.freq = 2,         # 최소 단어 빈도
          max.words = 200,      # 표현 단어 수
          random.order = F,     # 고빈도 단어 중앙 배치
          rot.per = .1,         # 회전 단어 비율
          scale = c(4, 0.3),    # 단어 크기 범위록
          colors = pal)         # 색상 목록

# 단어 색상 바꾸기
# 빈도가 높을 수록 진한 파란색으로 표현.
pal <- brewer.pal(9, "Blues")[5:9]
set.seed(4321)

wordcloud(words = df_word$word, # 단어
          freq = df_word$freq,  # 빈도
          min.freq = 2,         # 최소 단어 빈도
          max.words = 200,      # 표현 단어 수
          random.order = F,     # 고빈도 단어 중앙 배치
          rot.per = .1,         # 회전 단어 비율
          scale = c(4, 0.3),    # 단어 크기 범위록
          colors = pal)         # 색상 목록


# 10-2 국정원 트윗 텍스트 마이닝

twitter <- read.csv('twitter.csv',
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = 'UTF-8')
head(twitter) # x, 번호, 계정이름, 작성일, 내용

# 변수명 수정
twitter <- rename(twitter,
                  no = 번호,
                  id = 계정이름,
                  date = 작성일,
                  tw = 내용)

# 특수문자 제거
twitter$tw <- str_replace_all(twitter$tw, '\\W', ' ')

# 단어 빈도표 만들기

nouns <-extractNoun(twitter$tw)

wordcount <- table(unlist(nouns))

df_word <- as.data.frame(wordcount,
                         stringsAsFactors = F)

df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

df_word <- filter(df_word, nchar(word)>=2)

top20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)

#막대그래프 만들기

library(ggplot2)

order <- arrange(top20, freq)$word
ggplot(data = top20, aes(x = word, y = freq))+
  geom_col()+
  scale_x_discrete(limit = order)+
  coord_flip()+
  geom_text(aes(label = freq), hjust = -0.3)+
  ylim(0,2500)

pal <- brewer.pal(8, 'Dark2')
set.seed(1234)

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 10,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(6,0.2),
          colors = pal)
