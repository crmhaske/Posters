#Function to read the data
source("C:/Users/Christie/Documents/Research/RLibs/readdata.R")

#Requierd packages
library(ggplot2)
library(plyr)
library(e1071)
library(grid)

#Kurtosis plot
#----------------

#Generate a high and low kurtosis distribution
set.seed(123)
high.k<-c(rnorm(2000,mean=0,sd=.5),rnorm(1000,mean=0,sd=3))
low.k<-c(rnorm(2500,mean=0,sd=sd(high.k)),runif(250, -4, -1),runif(250, 1, 4))

#Mean sd of the two for the normal distribution
sd.norm<-(sd(high.k)+sd(low.k))/2

#Kurtosis of the high and low distributions
kur.high<-round(kurtosis(high.k),2)
kur.low<-round(kurtosis(low.k),2)

#Data frame for plotting
df.kur<-data.frame("value"=c(high.k,low.k),"dist"=rep(c("Positive Kurtosis","Negative Kurtosis"),each=3000))
df.kur$dist<-factor(df.kur$dist,levels=c("Positive Kurtosis","Negative Kurtosis"))

#Kurtosis label for plot
labs<-data.frame("x"=c(5,5),"y"=c(0.4,0.4),lab=c(as.character(kur.high),as.character(kur.low)),"dist"=c("Positive Kurtosis","Negative Kurtosis"))

#Plot
p.kur<-ggplot(df.kur,aes(x=value))+
  geom_histogram(aes(y=..density..),binwidth=0.5,colour="black",fill="light blue")+facet_grid(~dist)+
  stat_function(fun = dnorm, colour = "black", ,size=1.2,arg = list(mean = 0, sd = sd.norm))+
  xlab("")+ylab("Density")+geom_text(aes(x, y, label=paste("Kurtosis = ",lab)),data=labs)+xlim(c(-10,10))+
  theme_classic()+ theme(panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank(),
                         panel.border = element_blank(),
                         strip.background = element_blank(),
                         text=element_text(size=20),
                         axis.title.y=element_text(vjust=1.1))

#Save the plot
ggsave(p.kur,file="C:/Users/Christie/Documents/Research/gits/attnRew2/csbbcs2015/KurFig.svg",dpi=225,width=24,height=14.5,units="cm")

#Reward distribution shape plot
#--------------------------------

#Read the score distribution data and make plotting data frame
dhigh<-read.csv("C:/Users/Christie/Documents/Research/gits/attnRew2/csbbcs2015/high_reward.csv")
names(dhigh)<-c("error","points")
dhigh$Reward<-rep("Fine",length(dhigh$error))

dlow<-read.csv("C:/Users/Christie/Documents/Research/gits/attnRew2/csbbcs2015/low_reward.csv")
names(dlow)<-c("error","points")
dlow$Reward<-rep("Coarse",length(dlow$error))

dreward<-rbind(dhigh,dlow)

#Plot
p.rew<-ggplot(dreward,aes(x=error,y=points,colour=Reward))+geom_line(size=1.5)+geom_hline(yintercept=0)+
  xlab("Error (deg.)")+ylab("Reward (Points)")+ggtitle("Coarse & Fine Reward Distributions")+
  scale_y_continuous(breaks=seq(-10,70,10))+scale_colour_manual(values=c("light blue","dark blue"))+
  theme_classic()+theme(panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(),
                        panel.border = element_blank(),
                        text=element_text(size=20),
                        axis.title.y=element_text(vjust=1.1),
                        axis.title.x=element_text(vjust=0.9),
                        legend.position=c(0.8,0.7))

#Save plot
ggsave(p.rew,file="C:/Users/Christie/Documents/Research/gits/attnRew2/csbbcs2015/RewFig.svg",dpi=225,width=24,height=14.5,units="cm")


#Results
#--------

#Read the data
d<-read.data("C:/Users/Christie/Documents/Research/gits/attnRew2/data/attnRewPoints/orientation")

#Remove 27, 32, 33 and 34 because the monitor settings were wrong
d<-subset(d,d$userid!=27)
d<-subset(d,d$userid!=32)
d<-subset(d,d$userid!=33)
d<-subset(d,d$userid!=34)

#Remove practice trials
dnp<-subset(d,d$practice=="FALSE")

#Plot mean error for all subjects
sel<-prop.cor(dnp)
sel.ord<-sel[with(sel, order(prop.cor)), ]
qplot(data=sel.ord,x=reorder(userid, prop.cor),y=prop.cor,geom="point")

#Remove subjects 4, 11, 16 and 17 for poor performance
dnp<-subset(dnp,dnp$userid!=4)
dnp<-subset(dnp,dnp$userid!=11)
dnp<-subset(dnp,dnp$userid!=16)
dnp<-subset(dnp,dnp$userid!=17)

#Remap reward names for plotting
dnp$reward<-mapvalues(dnp$reward,from=c("low","high"),to=c("Coarse","Fine"))

#Correct the angular difference
dnp<-ddply(dnp,.(userid,reward,validcue),transform,meanAngdiff=mean(angdiff))
dnp$angdiff.cor<-dnp$angdiff-dnp$meanAngdiff

#Median absolute error omnibus ANOVA
err.user<-ddply(dnp,.(userid,reward,validcue),summarise,medErr=median(abs(angdiff.cor)))
summary(aov(medErr~as.factor(reward)*as.factor(validcue)+
              Error(as.factor(userid)/as.factor(validcue)),data=err.user))

#Remap names for plotting
err.user$validcue<-mapvalues(err.user$validcue,from=c("FALSE","neutral","TRUE"),to=c("Invalid","Neutral","Valid"))

#Plot
p.err.rew<-ggplot(err.user,aes(x=validcue,y=medErr,fill=reward))+
  facet_grid(~reward)+geom_boxplot()+geom_hline(y=mean(err.user$medErr))+
  scale_fill_manual("Reward",values=c("light blue","dark blue"))+
  xlab("Cueing")+ylab("Med. Abs. Error (deg.)")+scale_y_continuous(limits=c(4,18),breaks=seq(4,18,2))+
  theme_bw()+theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   text=element_text(size=20),
                   axis.title.y=element_text(vjust=1.1),
                   axis.title.x=element_text(vjust=0.4),
                   legend.position="none")

#Save plot
ggsave(p.err.rew,file="C:/Users/Christie/Documents/Research/gits/attnRew2/csbbcs2015/ErrRewFig.svg",dpi=225,width=24,height=12.5,units="cm")


#Kurtosis omnibus ANOVA
kur.user<-ddply(dnp,.(userid,reward,validcue),summarise,kur=kurtosis(angdiff))
summary(aov(kur~as.factor(reward)*as.factor(validcue)+
              Error(as.factor(userid)/as.factor(validcue)),data=kur.user))

#Remap names for plotting
kur.user$validcue<-mapvalues(kur.user$validcue,from=c("FALSE","neutral","TRUE"),to=c("Invalid","Neutral","Valid"))

#Plot
p.kur.rew<-ggplot(kur.user,aes(x=validcue,y=kur,fill=reward))+facet_grid(~reward)+
  geom_boxplot()+geom_hline(aes(yintercept=mean(kur.user$kur)))+geom_hline(data=data.frame("yint"=0,"label"="Normal Dist. Kurtosis"),aes(yintercept=yint,linetype=label),show_guide = TRUE)+
  xlab("Cueing")+ylab("Kurtosis")+scale_y_continuous(limits=c(0,15),breaks=seq(0,15,3))+
  theme_bw()+theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   text=element_text(size=20),
                   axis.title.y=element_text(vjust=1.1),
                   axis.title.x=element_text(vjust=0.4),
                   legend.position=c(0.85,0.95),
                   legend.background =element_rect(fill="transparent"),
                   legend.key = element_rect(colour="transparent"))+
  scale_fill_manual("Reward",values=c("light blue","dark blue"),guide="none")+
  scale_linetype_manual("",values="dashed")

#Save plot
ggsave(p.kur.rew,file="C:/Users/Christie/Documents/Research/gits/attnRew2/csbbcs2015/KurRewFig.svg",dpi=225,width=24,height=12.5,units="cm")


#Comparison of current results to older results with no reward on a similiar protocol (Anderson & Druker, 2013, PBR)
#-------------

#Read data, create reward variable, remove practice trials
dmich<-read.csv("C:/Users/Christie/Documents/Research/exp3.csv")
dmich$reward<-rep("No Reward",length(dmich$angdiff))
dmich.np<-subset(dmich,dmich$practice=="FALSE")

#Corrected angular difference
dmich.np<-ddply(dmich.np,.(userid,reward,validcue),transform,meanAngdiff=mean(angdiff))
dmich.np$angdiff.cor<-dmich.np$angdiff-dmich.np$meanAngdiff

#One data frame for all data, precision
err.user.mich<-ddply(dmich.np,.(userid,reward,validcue),summarise,medErr = median(abs(angdiff.cor)))
err.user.mich$validcue<-mapvalues(err.user.mich$validcue,from=c("FALSE","neutral","TRUE"),to=c("Invalid","Neutral","Valid"))
err.user.all<-rbind(err.user,err.user.mich)

#Fine vs. No Reward ANOVA
err.user.FN<-subset(err.user.all[err.user.all$reward!="Coarse",])
aov.user.FN<-aov(medErr~reward*validcue+Error(as.factor(userid)/validcue),err.user.FN)
summary(aov.user.FN)

#Coarse vs. No Reward ANOVA
err.user.CN<-subset(err.user.all[err.user.all$reward!="Fine",])
aov.user.CN<-aov(medErr~reward*validcue+Error(as.factor(userid)/validcue),err.user.CN)
summary(aov.user.CN)

#Plot
p.err.mich<-ggplot(err.user.mich,aes(x=validcue,y=medErr))+
  facet_grid(~reward)+geom_boxplot(fill="blue")+geom_hline(y=mean(err.user$medErr))+
  scale_fill_manual("Reward",values=c("light blue","dark blue"))+
  xlab("Cueing")+ylab("Med. Abs. Error (deg.)")+scale_y_continuous(limits=c(4,18),breaks=seq(4,18,2))+
  theme_bw()+theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   text=element_text(size=20),
                   axis.title.y=element_text(vjust=1.1),
                   axis.title.x=element_text(vjust=0.4),
                   legend.position="none")

#Save plot
ggsave(p.err.mich,file="C:/Users/Christie/Documents/Research/gits/attnRew2/csbbcs2015/ErrMichFig.svg",dpi=225,width=13.155,height=12.5,units="cm")


#One data frame for all data, kurtosis
kur.user.mich<-ddply(dmich,.(userid,reward,validcue),summarise,kur = kurtosis(angdiff))
kur.user.mich$validcue<-mapvalues(kur.user.mich$validcue,from=c("FALSE","neutral","TRUE"),to=c("Invalid","Neutral","Valid"))
kur.user.all<-rbind(kur.user,kur.user.mich)

#Fine vs. No reward ANOVA
kur.user.FN<-subset(kur.user.all[kur.user.all$reward!="Coarse",])
aov.user.FN<-aov(kur~reward*validcue+Error(as.factor(userid)/validcue),kur.user.FN)
summary(aov.user.FN)

#Coarse vs. No reward ANOVA
err.user.CN<-subset(kur.user.all[kur.user.all$reward!="Fine",])
aov.user.CN<-aov(kur~reward*validcue+Error(as.factor(userid)/validcue),err.user.CN)
summary(aov.user.CN)

#Plot
p.kur.mich<-ggplot(kur.user.mich,aes(x=validcue,y=kur))+facet_grid(~reward)+
  geom_boxplot(fill="blue")+geom_hline(aes(yintercept=mean(kur.user$kur)))+geom_hline(data=data.frame("yint"=0,"label"="Normal Dist. Kurtosis"),aes(yintercept=yint,linetype=label),show_guide = TRUE)+
  xlab("Cueing")+ylab("Kurtosis")+scale_y_continuous(limits=c(0,15),breaks=seq(0,15,3))+
  theme_bw()+theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   text=element_text(size=20),
                   axis.title.y=element_text(vjust=1.1),
                   axis.title.x=element_text(vjust=0.4),
                   legend.position=c(0.7,0.95),
                   legend.background =element_rect(fill="transparent"),
                   legend.key = element_rect(colour="transparent"))+
  scale_linetype_manual("",values="dashed")

#Save plot
ggsave(p.kur.mich,file="C:/Users/Christie/Documents/Research/gits/attnRew2/csbbcs2015/KurMichFig.svg",dpi=225,width=13.155,height=12.5,units="cm")

