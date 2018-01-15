library(tgp)

dat<-read.csv("/home/hanshalbe/MTT/activeMTT.csv")
opts<-expand.grid(x=2:4, y=2:4, z=2:4)

for (idn in 5:45){
  
  dp<-subset(dat, id==idn)
  dp<-dp[,4:7]
  
  infogain<-matrix(0, nrow=22, ncol=27)
  d<-data.frame(x=sample(2:4, 5, replace = TRUE), 
                y=sample(2:4, 5,  replace = TRUE),
                z=sample(2:4, 5,  replace = TRUE),
                out=rnorm(5, 20, 1))
  
  
  for (trials in 1:nrow(dp)){
    
    
    out <- bgp(X=d[,1:3], Z=d[,4], XX=opt, pred.n=FALSE, corr="exp", Ds2x=TRUE)
    infogain[trials,]<-out$Ds2x
    d<-rbind(d, dp[1:trials,])
    
  }
  write.csv(infogain, paste0("/home/hanshalbe/MTT/gpdata/gpcohon", idn, ".csv"))
  print(paste("Participant", idn, "is done."))
}