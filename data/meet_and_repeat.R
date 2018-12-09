BPRS<-read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep="", header=TRUE)
RATS<-read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", sep="\t", header=T)
#explore data
str(BPRS)
summary(BPRS)
str(RATS)
summary(RATS)
library(dplyr)
library(tidyr)

##BPRS
# Factor treatment & subject
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)

# Convert to long form
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)

# Extract the week number
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks, 5,5)))

# Take a glimpse at the BPRSL data
glimpse(BPRSL)

#graphical displays of individuals
library(ggplot2)
#draw the plot
ggplot(BPRSL, aes(x=week, y=bprs, linetype=subject))+
  geom_line()+
  scale_linetype_manual(values=rep(1:10, times=4))+
  facet_grid(.~treatment, labeller = label_both)+
  theme(legend.position = "top")+
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)), name=("BPRS"))

#standardise the variable bprs
BPRSL<- BPRSL%>%
  group_by(week)%>%
  mutate(stdbprs = ((bprs-mean(bprs))/sd(bprs))) %>%
  ungroup()
#glimps at the data
glimpse(BPRSL)

#plot again with the standardized bprs
ggplot(BPRSL, aes(x=week, y=bprs, linetype=subject))+
  geom_line()+
  scale_linetype_manual(values=rep(1:10, time = 4))+
  facet_grid(.~treatment, labeller = label_both)+
  scale_y_continuous(name = "standardized bprs")
#summary graphs
#Number of weeks, baseline (week 0) included
n <- BPRSL%>%unique()%>% length()
#summary data with and standard error of bprs by treatment and week
BPRSS<-BPRSL%>%
  group_by(treatment, week)%>%
  summarise( mean = mean(bprs), se =sd(bprs)/sqrt(n))%>%
  ungroup()
#glimpse at the data
glimpse(BPRSS)
#plot the mean profiles
ggplot(BPRSS, aes(x=week, y=mean, linetype=treatment, shape=treatment))+
  geom_line()+
  scale_linetype_manual(values= c(1,2) )+
  geom_point(size=3)+
  scale_shape_manual(values=c(1,2))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, linetype="1"), width=0.3)+
  scale_y_continuous(name= "mean(bprs)+/- se(bprs)")

#Find outliers
#create a summary data by treatment and subject with mean as the summary variable
BPRSL8S<-BPRSL%>%
  filter(week >0 )%>%
  group_by(treatment, subject)%>%
  summarise(mean = mean(bprs))%>%
  ungroup
#glipse at the data
glimpse(BPRSL8S)
#draw a boxplot of the mean versus treatment
ggplot(BPRSL8S, aes(x = treatment, y= mean))+
  geom_boxplot()+
  stat_summary(fun.y="mean", geom="point", shape=23, size =4, fill= "white")+
  scale_y_continuous(name="mean(bprs), weeks 1-8")
#create a new data by filtering th outlier
BPRSL8S1<-BPRSL8S%>%
  filter(mean<60)
#re-plot with the new data
ggplot(BPRSL8S1,aes(x = treatment, y= mean))+
  geom_boxplot()+
  stat_summary(fun.y="mean", geom="point", shape=23, size =4, fill= "white")+
  scale_y_continuous(name="mean(bprs), weeks 1-8")

#T-test and Anova analysis
#perform a two-sample t-test
t.test(mean~treatment, data = BPRSL8S1, var.equal =TRUE)
#add the baseline from the original data as a new variable to the summary data
BPRSL8S2<-BPRSL8S%>%
  mutate(baseline=BPRS$week0)
#fit the linear model with the mean as the response
fit <-lm(mean~baseline+treatment, data=BPRSL8S2)
#anova
anova(fit)

##RATS
#creating factors with ID and Group
glimpse(RATS)
RATS$ID<-factor(RATS$ID)
RATS$Group<-factor(RATS$Group)
glimpse(RATS)
#linear mixed effects model
#convert data to long form
RATSL<-RATS%>%
  gather(key=WD, value=Weight, -ID, -Group)%>%
  mutate(Time=as.integer(substr(WD,3,4)))
glimpse(RATSL)
#plot the RATSL data
dim(RATSL)
#plot the RATSL data
ggplot(RATSL, aes(x= Time, y= Weight, group = ID, linetype=Group))+
  geom_line()+
  scale_x_continuous(name="Time(days)", breaks = seq(0, 60, 10))+
  scale_y_continuous(name=("Weight(grams)"))+
  theme(legend.position = "top")
#Linear model
RATS_reg<-lm(Weight~Time + Group, data= RATSL)
summary(RATS_reg)
#Randome intercept model(lme4)
library(lme4)
#create a random intercept model
RATS_ref<-lmer(Weight~Time + Group + (1|ID), data = RATSL, REML = TRUE)
summary(RATSL_ref)
#create a random intercept and random slope model 
RATS_ref1<-lmer(Weight~Time + Group + (Time|ID), data = RATSL, REML = FALSE)
summary(RATS_ref1)
anova(RATS_ref1, RATS_ref)
#interaction between TIme and Group
RATS_ref2<-lmer(Weight~Time*Group+(Time|ID), data=RATSL, REML=FALSE)
summary(RATS_ref2)
anova(RATS_ref2, RATS_ref1)
#draw the plot of RATSL with the observed Weigth values
ggplot(RATSL, aes(x=Time, y= Weight, group=ID))+
  geom_line(aes(linetype=Group))+
  scale_x_continuous(name="Time(days)", breaks=seq(0,60,10))+
  scale_y_continuous(name="Observed Weight(grams)")+
  theme(legend.position="top")
#creat a vector of the fitted values
Fitted<-fitted(RATS_ref2)
#create a new column fitted RATSL
RATSL<-RATSL%>%
  mutate(Fitted)
#draw the plot of RATSL
ggplot(RATSL, aes(x = Time, y=Fitted, group=ID))+
  geom_line(aes(linetype = Group))+
  scale_x_continuous(name ="Time(days)", breaks = seq(0, 60, 20))+
  scale_y_continuous(name ="Fitted weight (grams)")+
  theme(legend.position = "top")
