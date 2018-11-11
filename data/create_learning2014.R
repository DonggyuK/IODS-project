#name:Donggyu Kam, Date:06.11.2018

lrn14<-read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header = TRUE)
dim(lrn14)
str(lrn14)

#dim is to look at how many rows and column data have, and str is to look into the structure of data. In this read data, 180 rows and 60 columns are there. 

library(dplyr)
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")
deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep<-rowMeans(deep_columns)
surface_columns<-select(lrn14, one_of(surface_questions))
lrn14$surf<-rowMeans(surface_columns)
strategic_columns<-select(lrn14, one_of(strategic_questions))
lrn14$stra<-rowMeans(strategic_columns)
keep_columns<-c("gender","Age","Attitude","deep","stra","surf","Points")
learning2014<-select(lrn14, one_of(keep_columns))
str(learning2014)

learning2014 <- filter(learning2014, Points > 0)

setwd("//ad.helsinki.fi/home/d/donggyu/Documents/GitHub/IODS-project/data")
write.csv(learning2014, file="learning2014.csv")
read.csv("learning2014.csv")

