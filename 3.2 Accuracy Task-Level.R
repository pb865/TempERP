
setwd("C:/Users/pjb35/Desktop")

data=read.csv("P2_and_dACC.csv")

library(ggplot2)
library(doBy)
library(reshape2)
library(tidyverse)

#----Create proportion variable 

#For each trial type, we need to calculate the proportion of correct vs incorrect responses for each person. 

temp=data.frame(unique(data$subID))
temp=dplyr::rename(temp, subID = unique.data.subID.)
temp$BGC=NA


#Drop outlier RTs (less than 100ms responses)
data=data[data$RT>100,]


#Now use this loop to create the proportion correct for each trial type:
#Prime (2 levels: Black and White) X Target (2 levels: Gun and Tool)

for (myperson in unique(data$subID)){
  
  Black_Gun_Correct=nrow(data[data$subID==myperson & data$Prime=="Black" & data$Target=="Gun" & data$Response=="Correct",])
  Black_Gun_Incorrect=nrow(data[data$subID==myperson & data$Prime=="Black" & data$Target=="Gun" & data$Response=="Incorrect",])
  
  Black_Tool_Correct=nrow(data[data$subID==myperson & data$Prime=="Black" & data$Target=="Tool" & data$Response=="Correct",])
  Black_Tool_Incorrect=nrow(data[data$subID==myperson & data$Prime=="Black" & data$Target=="Tool" & data$Response=="Incorrect",])
  
  White_Gun_Correct=nrow(data[data$subID==myperson & data$Prime=="White" & data$Target=="Gun" & data$Response=="Correct",])
  White_Gun_Incorrect=nrow(data[data$subID==myperson & data$Prime=="White" & data$Target=="Gun" & data$Response=="Incorrect",])
  
  White_Tool_Correct=nrow(data[data$subID==myperson & data$Prime=="White" & data$Target=="Tool" & data$Response=="Correct",])
  White_Tool_Incorrect=nrow(data[data$subID==myperson & data$Prime=="White" & data$Target=="Tool" & data$Response=="Incorrect",])
  
  temp[temp$subID==myperson,"BGC"]=Black_Gun_Correct
  temp[temp$subID==myperson,"BGI"]=Black_Gun_Incorrect
  temp[temp$subID==myperson,"BTC"]=Black_Tool_Correct
  temp[temp$subID==myperson,"BTI"]=Black_Tool_Incorrect
  
  temp[temp$subID==myperson,"WGC"]=White_Gun_Correct
  temp[temp$subID==myperson,"WGI"]=White_Gun_Incorrect
  temp[temp$subID==myperson,"WTC"]=White_Tool_Correct
  temp[temp$subID==myperson,"WTI"]=White_Tool_Incorrect
  
  rm(Black_Tool_Correct)
  rm(Black_Tool_Incorrect)
  rm(Black_Gun_Incorrect)
  rm(Black_Gun_Correct)
  rm(White_Tool_Incorrect)
  rm(White_Gun_Incorrect)
  rm(White_Gun_Correct)
  rm(White_Tool_Correct)
  
  
}


#Now convert them from raw numbers to percentages
temp$Porp.Correct.BPG=(temp$BGC)/(temp$BGC+temp$BGI)
temp$Porp.Correct.BPT=(temp$BTC)/(temp$BTC+temp$BTI)
temp$Porp.Correct.WPG=(temp$WGC)/(temp$WGC+temp$WGI)
temp$Porp.Correct.WPT=(temp$WTC)/(temp$WTC+temp$WTI)

#Arcsin transform for proper analysis
temp$Acc.Black.Gun=sqrt(asin(temp$Porp.Correct.BPG))
temp$Acc.Black.Tool=sqrt(asin(temp$Porp.Correct.BPT))
temp$Acc.White.Gun=sqrt(asin(temp$Porp.Correct.WPG))
temp$Acc.White.Tool=sqrt(asin(temp$Porp.Correct.WPT))


#-------------
#This first RMANOVA will just look at the Prime x Target interaction with 
#NO between-person moderators 

#First, we need to create the matrix for the ANOVA object to reference for 
#within-person variables 
myidata=expand.grid("Prime"=c("Black","White"), "Target"=c("Gun","Tool")) #Creates a matrix of target by prime 

#Load car
library(car)

#Target by Prime MANOVA 
mylm1=lm(cbind(Acc.Black.Gun, Acc.Black.Tool, Acc.White.Gun, Acc.White.Tool) ~ 1, data= temp)
myaov1=Anova(mylm1, idata=myidata, idesign=~Prime*Target, type=3)
summary(myaov1, multivariate=F, univariate=T, type="3")

#Get effect sizes (partial eta2)
library(heplots)

options(scipen=50)
etasq(myaov1, anova = TRUE)


#Descriptive stats
mean(temp$Acc.Black.Gun, na.rm=T)
sd(temp$Acc.Black.Gun,na.rm=T)

mean(temp$Acc.Black.Tool, na.rm=T)
sd(temp$Acc.Black.Tool,na.rm=T)

mean(temp$Acc.White.Gun, na.rm=T)
sd(temp$Acc.White.Gun,na.rm=T)

mean(temp$Acc.White.Tool, na.rm=T)
sd(temp$Acc.White.Tool,na.rm=T)

#within-person aka paired sample t-test and effect sizes 
library(lsr)

t.test(temp$Acc.Black.Gun,temp$Acc.White.Gun,paired=T)
cohensD(temp$Acc.Black.Gun,temp$Acc.White.Gun, method="paired")

t.test(temp$Acc.Black.Tool,temp$Acc.White.Tool,paired=T)
cohensD(temp$Acc.Black.Tool,temp$Acc.White.Tool, method="paired")

t.test(temp$Acc.White.Tool,temp$Acc.White.Gun,paired=T)
cohensD(temp$Acc.White.Tool,temp$Acc.White.Gun, method="paired")

t.test(temp$Acc.Black.Tool,temp$Acc.Black.Gun,paired=T)
cohensD(temp$Acc.Black.Tool,temp$Acc.Black.Gun, method="paired")


#visualize 
Freq3=temp[,c(1, 14:17)]
Freqlong=melt(data=Freq3, id.vars="subID")
Freqlong$Prime=as.factor(ifelse(Freqlong$variable %in% c("Acc.Black.Gun",
                                                         "Acc.Black.Tool"), "Black", "White"))
Freqlong$Target=as.factor(ifelse(Freqlong$variable %in% c("Acc.Black.Gun",
                                                         "Acc.White.Gun"), "Gun", "Tool"))

Freqlong2=summaryBy(value~Prime+Target,data=Freqlong,FUN=c(mean,sd))

Freqlong2$SE=Freqlong2$value.sd/sqrt(length(unique(data$subID)))

#prime on x-axis
ggplot(data=Freqlong2, mapping=aes(x=Prime,y=value.mean,fill=Target, group=Target))+geom_bar(stat="identity",position="dodge")+
  geom_errorbar(mapping=aes(ymax=value.mean+SE,ymin=value.mean-SE),position=position_dodge(width=.9),width=.5)+
  labs(y='Average Porprotion Correct \n (arcsine transform)', x='Prime', fill='Target')+
  theme_classic()+
  theme(text = element_text(size=15))+
  coord_cartesian(ylim=c(.60,1.03))


#------------------------

#Now we need a version that has the between-subject moderators: 
#IMS_Centered: Internal motivation to control prejudice 
#EMS_Centered: External motivation to control prejudice 
#P2_Between: Between-person component of the P2 ERP

#IMS/EMS, between-subject P2 are the following columns
motiv=data[,c(1, 8, 9, 10)]
motiv=motiv[!(duplicated(motiv$subID)),]

#Put them in the dataframe we have been using for analyses 
temp=merge(temp, motiv)


#Make sure car is loaded
library(car)

# #RMANOVA 2: includes white primes- comment out for potential future reference. 
# mylm1=lm(cbind(Acc.Black.Gun, Acc.Black.Tool, Acc.White.Gun, Acc.White.Tool) ~ IMS_Centered*EMS_Centered*P2_Between, data= temp)
# myaov1=Anova(mylm1, idata=myidata, idesign=~Prime*Target, type=3)
# summary(myaov1, multivariate=F, univariate=T, type="3")
# etasq(myaov1, anova = TRUE)


#Conduct the MANOVA 
myidata2=expand.grid("Target"=c("Gun","Tool")) #Creates a matrix of target


mylm1=lm(cbind(Acc.Black.Gun, Acc.Black.Tool) ~IMS_Centered*EMS_Centered*P2_Between, data= temp)
myaov1=Anova(mylm1, idata=myidata2, idesign=~Target, type=3)
summary(myaov1, multivariate=F, univariate=T, type="3")


#Typically here I would use emmeans, but how do I get the effect of Target?

library(ggplot2)
library(emmeans)

#Select values representative of high/low IMS and EMS: about +-2SDs
summary(temp$IMS_Centered)
sd(temp$IMS_Centered, na.rm=T)

myims=function(X...){vals=c(-15, 10)}

summary(temp$EMS_Centered)
sd(temp$EMS_Centered, na.rm=T)
myems=function(X...){vals=c(-15, 15)}

#Select the full range of P2
summary(temp$P2_Between)
mean(temp$P2_Between, na.rm=T)
sd(temp$P2_Between, na.rm=T)

myP2=function(X...){vals=c(-2, 1, 3, 5, 7, 9, 11)}

#emmeans
# mymeans=data.frame(emmeans(mylm1, pairwise~IMS_Centered+EMS_Centered+P2_Between,
#                            cov.reduce=list(IMS_Centered=myims,
#                                       EMS_Centered=myems,
#                                       P2_Between=myP2), adjust="none",
#                            type="response")$emmeans)

summary(emtrends(mylm1, pairwise~IMS_Centered|EMS_Centered, var="P2_Between",
                           cov.reduce=list(IMS_Centered=myims,
                                      EMS_Centered=myems,
                                      P2_Between=myP2), adjust="none",
                           type="response"), infer=T)






