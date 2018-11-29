human<-read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human2.txt", sep=',', header= TRUE)

##overview and summary of the data
ggpairs(human)
summary(human)

It seems that Edu.Exp is positvely correlated to Life.Exp and negatively to Mat.Mor.
Mat.Mor seems negatively correlated to Life.Exp. 

##Try to find out the most essential variabls through dimentionality reduction with PCA. 
#the unstandardized variables
pca_human_unstd<-prcomp(human)
us<-summary(human)
biplot(pca_human_unstd, cex=c(0.8,1), col=c("grey40","deeppink2"), xlab="PC1", ylab="PC2")

#the standized variables
human_std<-scale(human)
#print out summaries of human_std
summary(human_std)
#perform principal component analysis(with the SVD method)
pca_human<-prcomp(human_std)
pca_human
##draw a biplot of the principan component representation and the original variables
s<-summary(pca_human)
#rounded percentages of variance captured by each PC
pca_pr<-round(1*s$importance[2,], digits=1)
#print out the percentages of variance
pca_pr
#create object pc_lab to be used as axis labels
pc_lab<-paste0(names(pca_pr), "(", pca_pr, "%)")
#draw a biplot

biplot(pca_human, cex=c(0.8,1), col=c("grey40", "deeppink2"), xlab=pc_lab[1], ylab=pc_lab[2])

Unstandaized variables looks out of ranges because PCA is sensitive to the relatives scallings of the orginal variables and assumes that feature with larger varianace are more important than variables with smaller variabnce. 

##결과에 대해서 설명 넣기 


##Multiple Correspondence Analysis with Tea dataset
library(FactoMineR)
library(ggplot2)
library(dplyr)
library(tidyr)
data("tea")

#keep column names in the dataset
keep_columns<-c("Tea", "How","how","sugar","where","lunch")
#select the 'keep_columns' to create a new dataset
tea_time<-select(tea, one_of(keep_columns))
#summaries and structure of the data
summary(tea_time)
str(tea_time)
#visualize the dataset
gather(tea_time)%>%ggplot(aes(value))+facet_wrap("key",scales="free")+geom_bar()

#Multiple Corresondence Analysis
mca<-MCA(tea_time, graph=FALSE)
#summary of the model
summary(mca)
#visualize MCA
plot(mca, invisible=c("ind"), habillage="quali")
#biplot
biplot()