d<-read.csv("/home/hanshalbe/MTT/mtt_data.csv", header=FALSE)
#required packages packages
packages <- c('ggplot2', 'plyr', 'jsonlite', 'lme4', 'lsmeans', 'grid', 'gridExtra', 'png')

#load them
lapply(packages, library, character.only = TRUE)

dat<-data.frame(id=numeric(),trial=numeric(), x=numeric(), y=numeric(), z=numeric(), out=numeric(), cond=numeric())
for (idn in 1:nrow(d)){
  myjson<-fromJSON(paste(d$V10[idn]))
  names(myjson)
  for (i in 14:35){
    dd<-unlist(strsplit( myjson[[i]], ";"))[1:4]
    dd<-as.numeric(dd)
    dd<-data.frame(id=idn, trial=i-13, x=dd[1], y=dd[2], z=dd[3], out=dd[4], cond=myjson$condition_training_type)
    dat<-rbind(dat, dd)
  }
}

head(dat, 100)

write.csv(dat,"/home/hanshalbe/MTT/clean_data.csv")
