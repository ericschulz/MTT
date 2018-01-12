d<-read.csv("/home/hanshalbe/MTT/mtt_data.csv", header=FALSE)
#required packages packages
packages <- c('ggplot2', 'plyr', 'jsonlite', 'lme4', 'lsmeans', 'grid', 'gridExtra', 'png')

#load them
lapply(packages, library, character.only = TRUE)

myjson<-fromJSON(paste(d$V10[1]))


condition<-extrapolation<-recall<-interpolation<-inst<-numeric()
correct<-condition<-id<-numeric()
for (i in 1:nrow(d)){
  myjson<-fromJSON(paste(d$V10[i]))
  corr<-c(myjson$test_comparison_1_correct,myjson$test_comparison_2_correct,
  myjson$test_comparison_3_correct,myjson$test_comparison_4_correct,
  myjson$test_comparison_5_correct,myjson$test_comparison_6_correct,
  myjson$test_comparison_7_correct,myjson$test_comparison_8_correct)
  condition<-c(condition, myjson$condition_index)
  correct<-c(correct, corr)
  id<-c(id, i)
}

dcor<-data.frame(id=rep(id, each=8), cond=rep(condition, each=8), correct)
dcor$cond<-mapvalues(dcor$cond, 0:3, c("active", "passive", "active", "passive"))
ddply(dcor, ~cond, summarize, mean(correct))
m<-glm(correct~cond, data=dcor, family="binomial")
summary(m)
