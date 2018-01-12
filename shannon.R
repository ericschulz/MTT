library(MCMCpack)

dat<-read.csv("/home/hanshalbe/MTT/activeMTT.csv")
opts<-expand.grid(x=2:4, y=2:4, z=2:4)

for (idn in 1:45){

  dp<-subset(dat, id==idn)
  dp<-dp[,4:7]
  
  infogain<-matrix(0, nrow=22*27, ncol=4)
  count<-1
  d<-data.frame(x=sample(2:4, 6, replace = TRUE), 
                y=sample(2:4, 6,  replace = TRUE),
                z=sample(2:4, 6,  replace = TRUE),
                out=rnorm(6, 20, 1))
  
  for (trials in 1:nrow(dp)){
  
    
    m<-MCMCregress(out~x+y+z, data=d)
    
    betas<-summary(m)$statistics[1:4,1]
    uncertainty<-summary(m)$statistics[1:4,2]
    for (i in 1:nrow(opts)){
      xeval<-opts[i,]
      yeval<-betas[1]+betas[2]*xeval[1]+betas[3]*xeval[2]+betas[4]*xeval[3]
      dummy<-rbind(d, data.frame(x=xeval[1],y=xeval[2],z=xeval[3], out=as.numeric(yeval)))
      m<-MCMCregress(out~x+y+z, data=dummy)
      infogain[count,]<-uncertainty-summary(m)$statistics[1:4,2]
      count<-count+1
      print(100*count/(22*27))
    }
    
    d<-rbind(d, dp[1:trials,])
  
  }
  write.csv(infogain, paste0("/home/hanshalbe/MTT/gpdata/shannon", idn, ".csv"))
  print(paste("Participant", idn, "is done."))
}