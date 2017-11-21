a <- 1
b <- 2
b
b<-c(1,2)

str1 <- c("Hello", "World", "is", "Good!")
str2 <- paste(str1, collapse = " ")

x <- c(1,2,3) # Alt + '-' 하면 <- 입력됨.
x_mean <- mean(x) 

library(ggplot2)

x <- c('a','a','b','c')

qplot(x)
qplot(data = mpg, x = hwy)

qplot(data=mpg, x=drv, y=hwy, geom = 'boxplot', colour = drv)

scores = c(80,60,70,50,90)
avg = mean(scores)
avg
