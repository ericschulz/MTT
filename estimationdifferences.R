library(lmerTest)
library(lme4)
drec<-read.csv("/home/hanshalbe/MTT/recallperf.csv")
drec$cond<-mapvalues(drec$cond, 0:3, c("active", "passive", "active", "passive"))
m<-lm(error~cond, data=drec)
summary(m)

dint<-read.csv("/home/hanshalbe/MTT/interperf.csv")
dint$cond<-mapvalues(dint$cond, 0:3, c("active", "passive", "active", "passive"))
m<-lmer(error~cond+(1|id), data=dint)
summary(m)

dex<-read.csv("/home/hanshalbe/MTT/extraperf.csv")
dex$cond<-mapvalues(dex$cond, 0:3, c("active", "passive", "active", "passive"))
m<-lmer(error~cond+(1|id), data=dex)
summary(m)

dall<-rbind(drec,dint,dex)

m<-lm(error~cond, data=dall)
summary(m)

ddply(dall, ~cond, summarize, m=median(error))
