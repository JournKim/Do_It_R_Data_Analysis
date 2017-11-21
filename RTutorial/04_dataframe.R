english <- c(90, 80, 60, 70)

math <- c(50, 60, 100, 20)



df_midterm <- data.frame(english, math)

class <- c(1,1,2,2)

df_midterm <- data.frame(english, math, class)

mean_english <- mean(df_midterm $ english)

mean_math <- mean(df_midterm $ math)

df_midterm <- data.frame(english = c(90,80,60,70),
                         math = c(50,60,100,20),
                         class = c(1,1,2,2))
df <- data.frame(제품 = c("사과", "딸기", "수박"),
                   가격 = c(1800, 1500, 3000),
                   판매량 = c(24, 38, 13))

mean(df$가격)
mean(df$판매량)

#read from excel file
library(readxl)
df_exam <- read_excel("excel_exam.xlsx")
#df_exam <- read_excel("excel_exam.xlsx", col_names = F) : 첫 행이 변수명이 아닐 때
#df_exam <- read_excel("excel_exam.xlsx", sheet = 3) : 여러 시트가 있을 때

df_csv_exam <- read.csv("csv_exam.csv")
#df_csv_exam <- read.csv("csv_exam.csv", stringsAsFactors = F) : 데이터에 문자가 들어 있을 때 사용.

##### dataframe to csv file

write.csv(df_midterm, "df_midterm.csv")

#### dataframe to RData
save(df_midterm, file="df_midterm.rda")

rm(df_midterm) # remove data

load("df_midterm.rda") # load rda file


#### Practice

save(df, df_csv_exam, df_exam, df_midterm, file="saveTest.rda")
rm(df)
rm(df_csv_exam)
rm(df_exam)
rm(df_midterm)

load("saveTest.rda")
