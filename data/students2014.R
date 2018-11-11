#Name: Donggyu Kam,  Date:11.11.2018

students2014<-read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/learning2014.txt", sep=",", header=TRUE )
dim(students2014)
str(students2014)

the data "students2014" is about the students attitude toward statistics. And the data consists of information about student ages, attitude toward statistics, points from exam and the students'' gender so it has 7 variables and 166 observations.

library(GGally)
library(ggplot2)


pairs(students2014[-1], col=(students2014$gender))
p <- ggpairs(students2014, mapping = aes(col=gender, alpha=0.3), lower = list(combo = wrap("facethist", bins = 20)))
p



model1<-lm(points ~ attitude+stra+surf, data=students2014)
summary(model1)
par(mfrow=c(2,2))
plot(model1, which=c(1,2,5))

model2<-lm(points ~ attitude, data=students2014)
summary(model2)

par(mfrow=c(2,2))
plot(model2, which = c(1,2,5))



