d<-read.csv("/home/hanshalbe/MTT/clean_data.csv")
d1<-subset(d, cond=="active")

nrow(d1)/22
d1$id<-rep(1:45, each=22)
head(d1)
d1$X<-NULL
write.csv(d1, "/home/hanshalbe/MTT/activeMTT.csv")
out<-expand.grid(2:4,2:4,2:4)
write.csv(out, "/home/hanshalbe/MTT/options.csv")
library(ggjoy)

d1$Round<-as.factor(d1$trial)



dp1<-rbind(data.frame(Round=d1$Round, x=d1$x, trial=d1$trial),
           data.frame(Round=d1$Round, x=d1$y, trial=d1$trial),
           data.frame(Round=d1$Round, x=d1$z, trial=d1$trial))
max(dp1$trial)           
p1<-ggplot(subset(dp1, trial %in% c(1,2,4,6,8,10,12,14,16,18,20)), aes(x = x, y = Round)) + 
  geom_joy()+ ylab("Trials")+theme_joy() +xlab("Feature value")+
  scale_x_continuous(breaks = round(seq(min(1), max(8), by = 1),1)) +
  ggtitle("Active condition")

pdf("/home/hanshalbe/MTT/activeonlymoretrials.pdf")
p1
dev.off()

d2<-subset(d, cond=="passive")

d2$Round<-as.factor(d2$trial)



dp2<-rbind(data.frame(Round=d2$Round, x=d2$x, trial=d2$trial),
           data.frame(Round=d2$Round, x=d2$y, trial=d2$trial),
           data.frame(Round=d2$Round, x=d2$z, trial=d2$trial))

p2<-ggplot(subset(dp2, trial %in% c(1,15,22)), aes(x = x, y = Round)) + 
  geom_joy()+ ylab("Trials")+theme_joy() +xlab("Feature value")+
  scale_x_continuous(breaks = round(seq(min(1), max(8), by = 1),1)) +
  ggtitle("Passive condition")
p2


library(gridExtra)
pdf("/home/hanshalbe/MTT/cosenfeature.pdf", width=8, height=5)
grid.arrange(p1,p2, nrow=1)
dev.off()

p1<-ggplot(subset(d1, trial %in% c(1,2,3,4,5,10,15,20)), aes(x = x, y = Round)) + 
  geom_joy()+ ylab("Trials")+theme_joy() +xlab("Feature value")+
  scale_x_continuous(breaks = round(seq(min(1), max(8), by = 1),1)) +
  ggtitle("X")
p1

p2<-ggplot(subset(d1, trial %in% c(1,2,3,4,5,10,15,20)), aes(x = y, y = Round)) + 
  geom_joy()+ ylab("Trials")+theme_joy() +xlab("Feature value")+
  scale_x_continuous(breaks = round(seq(min(1), max(8), by = 1),1)) +
  ggtitle("Y")
p2


p3<-ggplot(subset(d1, trial %in% c(1,2,3,4,5,10,15,20)), aes(x = z, y = Round)) + 
  geom_joy()+ ylab("Trials")+theme_joy() +xlab("Feature value")+
  scale_x_continuous(breaks = round(seq(min(1), max(8), by = 1),1)) +
  ggtitle("Z")
p3

pdf("/home/hanshalbe/MTT/featuresxyz.pdf", width=8, height=8)

grid.arrange(p1,p2,p3, nrow=1)

dev.off()
