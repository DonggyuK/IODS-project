
##Data wrangling
# read the human data
human <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt", sep  =",", header = T)

# look at the (column) names of human
names(human)

# look at the structure of human
str(human)

# print out summaries of the variables
summary(human)

#tidyr package and human are available
library(tidyr)
#access the stringr package
library(stringr)
#look at the structure of the GNI column in 'human'
str(human$GNI)
#Remove the commnas from GNI and print out a numeric version of it
str_replace(human$GNI, pattern=",", replace="")%>%as.numeric

library(dplyr)
##dealing with not available values (NA)
#column to keep
keep<-c("Country","Edu2.FM","Labo.FM", "Life.Exp", "Edu.Exp", "GNI", "Mat.Mor","Ado.Birth","Parli.F")
#select the 'keep' columns
human<-select(human, one_of(keep))
#print out a completeness indicator of the 'human' data
complete.cases(human)
#print out the data along with a completeness indicator as the last column
data.frame(human[-1], comp=complete.cases(human))
#filter out all rows with NA values
human_<-filter(human, complete.cases(human))
human_

##exclude observations
#look at the last 10 obs. of human
tail(human, 10)
#define the last indice we want to keep
last<-nrow(human)-7
last
#choose everthing until the last 7 obs. 
human_<-human[1:last,]
human_
#add countries as rownames
rownames(human_)<-human_$Country
human_

##explore thec countries
#remove the Country variable
human_<-select(human, -Country)
library(GGally)
#visulalize the 'human_' variables
ggpairs(human_)
cor(human_)


