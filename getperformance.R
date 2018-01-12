d<-read.csv("/home/hanshalbe/MTT/mtt_data.csv", header=FALSE)
#required packages packages
packages <- c('ggplot2', 'plyr', 'jsonlite', 'lme4', 'lsmeans', 'grid', 'gridExtra', 'png')

#load them
lapply(packages, library, character.only = TRUE)

myjson<-fromJSON(paste(d$V10[1]))


condition<-extrapolation<-recall<-interpolation<-inst<-numeric()

for (i in 1:nrow(d)){
  myjson<-fromJSON(paste(d$V10[i]))
  extra<-c(myjson$test_estimation_extrapolation_1_estimation_distance,myjson$test_estimation_extrapolation_2_estimation_distance,
    myjson$test_estimation_extrapolation_3_estimation_distance,myjson$test_estimation_extrapolation_4_estimation_distance,
    myjson$test_estimation_extrapolation_5_estimation_distance,myjson$test_estimation_extrapolation_6_estimation_distance,
    myjson$test_estimation_extrapolation_7_estimation_distance,myjson$test_estimation_extrapolation_8_estimation_distance)
  
  rec<-c(myjson$test_estimation_recall_1_estimation_distance,myjson$test_estimation_recall_2_estimation_distance,
    myjson$test_estimation_recall_3_estimation_distance,myjson$test_estimation_recall_4_estimation_distance,
    myjson$test_estimation_recall_5_estimation_distance)
  
  inter<-c(myjson$test_estimation_interpolation_1_estimation_distance,myjson$test_estimation_interpolation_2_estimation_distance,
    myjson$test_estimation_interpolation_3_estimation_distance,myjson$test_estimation_interpolation_4_estimation_distance,
    myjson$test_estimation_interpolation_5_estimation_distance)
  
  condition<-c(condition, myjson$condition_training_type)
  extrapolation<-c(extrapolation,extra)
  recall<-c(recall, rec)
  interpolation<-c(interpolation, inter)
  inst<-c(inst, myjson$instructions_check_attempts_counter)
  
}

drecall<-data.frame(id=rep(1:98, each=5), cond=rep(condition, each=5), error=recall, counter=rep(inst,each=5))
dinter<-data.frame(id=rep(1:98, each=5), cond=rep(condition, each=5), error=interpolation, counter=rep(inst,each=5))
dextra<-data.frame(id=rep(1:98, each=8), cond=rep(condition, each=8), error=extrapolation,counter=rep(inst,each=8))

write.csv(drecall, "/home/hanshalbe/MTT/recallperf.csv")
write.csv(dinter, "/home/hanshalbe/MTT/interperf.csv")
write.csv(dextra, "/home/hanshalbe/MTT/extraperf.csv")
