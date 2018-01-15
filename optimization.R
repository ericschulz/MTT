dat<-read.csv("/home/hanshalbe/MTT/activeMTT.csv")

dp1<-subset(dat, id==1)
gp<-read.csv("/home/hanshalbe/MTT/gpdata/sigp1.csv", header=FALSE)


normalize<-function(x){((x-mean(x))/max(abs(x-mean(x))))}

mttsoftmax<-function(predmat, data, tau){
  m<-predmat
  opt<-expand.grid(x=2:4, y=2:4, z=2:4)
  pred<-rep(0,22)
  choices<-numeric()
  for (i in 1:22){
    util<- m[i,]
    util[choices]<-0
    util<-normalize(util)
    util<-exp(tau*util)
    util<-util/sum(util)
    chosen<-which(opt$x==data$x[i] & opt$y==data$y[i] & opt$z==data$z[i]) 
    pred[i]<-util[chosen]
    choices<-c(choices,chosen)
  }
  #pred<-ifelse(is.na(pred), 1, pred)
  pred <- (pmax(pred, 0.00001))
  pred <- (pmin(pred, 0.99999))
  return(- sum(log(pred)))
}


library(DEoptim)

randomloss<-rep(0,45)
for (i in 1:45){
  ddn<-subset(dat, id==i)
  m<-matrix(runif(22*27), nrow=22)
  mfit<-DEoptim(mttsoftmax,data=dd, predmat=m, lower=exp(-4), upper=exp(4),  DEoptim.control(itermax = 100))
  randomloss[i]<-mfit$optim$bestval
}
mean(randomloss)

regvarloss<-rep(0,45)
for (i in 1:45){
  gp<-read.csv(paste0("/home/hanshalbe/MTT/gpdata/sigp",i,".csv"), header=FALSE)
  ddn<-subset(dat, id==i)
  m<-t(matrix(gp[,1], ncol=22))
  mfit<-DEoptim(mttsoftmax,data=ddn, predmat=m, lower=exp(-4), upper=exp(4),  DEoptim.control(itermax = 100))
  regvarloss[i]<-mfit$optim$bestval
}
mean(1-regvarloss/randomloss)

gpvarloss<-rep(0,45)
for (i in 1:45){
  gp<-read.csv(paste0("/home/hanshalbe/MTT/gpdata/sigp",i,".csv"), header=FALSE)
  ddn<-subset(dat, id==i)
  m<-t(matrix(gp[,2], ncol=22))
  mfit<-DEoptim(mttsoftmax,data=ddn, predmat=m, lower=exp(-4), upper=exp(4),  DEoptim.control(itermax = 100))
  gpvarloss[i]<-mfit$optim$bestval
}
mean(1-gpvarloss/randomloss)

regmuloss<-rep(0,45)
for (i in 1:45){
  gp<-read.csv(paste0("/home/hanshalbe/MTT/gpdata/mup",i,".csv"), header=FALSE)
  ddn<-subset(dat, id==i)
  m<-t(matrix(gp[,1], ncol=22))
  mfit<-DEoptim(mttsoftmax,data=ddn, predmat=m, lower=exp(-4), upper=exp(4),  DEoptim.control(itermax = 100))
  regmuloss[i]<-mfit$optim$bestval
}
mean(1-regmuloss/randomloss)

gpmuloss<-rep(0,45)
for (i in 1:45){
  gp<-read.csv(paste0("/home/hanshalbe/MTT/gpdata/mup",i,".csv"), header=FALSE)
  ddn<-subset(dat, id==i)
  m<-t(matrix(gp[,2], ncol=22))
  mfit<-DEoptim(mttsoftmax,data=ddn, predmat=m, lower=exp(-4), upper=exp(4),  DEoptim.control(itermax = 100))
  gpmuloss[i]<-mfit$optim$bestval
}
mean(1-gpmuloss/randomloss)


shannonloss<-rep(0,45)
for (i in 1:45){
  gp<-read.csv(paste0("/home/hanshalbe/MTT/gpdata/shannon",i,".csv"), header=TRUE)
  gp<-rowSums(gp[,2:5])
  ddn<-subset(dat, id==i)
  m<-t(matrix(gp, ncol=22))
  mfit<-DEoptim(mttsoftmax,data=ddn, predmat=m, lower=exp(-4), upper=exp(4),  DEoptim.control(itermax = 100))
  shannonloss[i]<-mfit$optim$bestval
}
mean(1-shannonloss/randomloss)

focusedshannonloss<-rep(0,45)
for (i in 1:45){
  gp<-read.csv(paste0("/home/hanshalbe/MTT/gpdata/shannon",i,".csv"), header=TRUE)
  gp<-apply(gp[,3:5],1,max)
  ddn<-subset(dat, id==i)
  m<-t(matrix(gp, ncol=22))
  mfit<-DEoptim(mttsoftmax,data=ddn, predmat=m, lower=exp(-4), upper=exp(4),  DEoptim.control(itermax = 100))
  focusedshannonloss[i]<-mfit$optim$bestval
}
mean(1-focusedshannonloss/randomloss)


