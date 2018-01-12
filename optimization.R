dat<-read.csv("/home/hanshalbe/MTT/activeMTT.csv")

dp1<-subset(dat, id==1)
gp<-read.csv("/home/hanshalbe/MTT/gpdata/sigp1.csv", header=FALSE)

gpopt(32, 0.0001)
idx=32
tau<-0.001
gpopt<-function(idx, tau){
  dd<-subset(dat, id==idx)
  gp<-read.csv(paste0("/home/hanshalbe/MTT/gpdata/sigp", idx, ".csv"), header=FALSE)
  m<-t(matrix(gp[,1], ncol=22))
  opt<-expand.grid(x=2:4, y=2:4, z=2:4)
  pred<-rep(0,22)
  choices<-numeric()
  for (i in 1:22){
    util<-m[i,]-max(m[i,])
    util<-exp(tau*m[i,])
    util[choices]<-0
    util<-util/sum(util)
    chosen<-which(dd$x[i] == opt$x & dd$y[i] == opt$y & dd$z[i] == opt$z) 
    pred[i]<-util[chosen]
    choices<-c(choices,chosen)
  }
  #pred<-ifelse(is.na(pred), 1, pred)
  pred <- (pmax(pred, 0.00001))
  pred <- (pmin(pred, 0.99999))
  return(- sum(log(pred)))
}

library(DEoptim)
loss<-rep(0,45)
for (i in 2:45){
  mfit<-DEoptim(gpopt, lower=exp(-4), upper=exp(4), idx=i,  DEoptim.control(itermax = 30))
  loss[i]<-mfit$optim$bestval
}

loss
