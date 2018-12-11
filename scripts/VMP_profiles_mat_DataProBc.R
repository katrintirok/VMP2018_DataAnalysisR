# import vmp profiles and convert to data frame

library(R.matlab)

rm(list=ls())

# read matlab structure with profiles for each file
matdata <- readMat('data/VMP_profiles.mat')
# unlist to reduce layers in R list
matdata1 <- unlist(matdata, recursive = F, use.names = T)
matdata2 <- unlist(matdata1, recursive = F, use.names = T)  

prof_num <- 0
varno <- 6 # number of variables extracted from vmp profiles
# loop through each file, each file represented by one list in matdata2 (18 in total)
for(i in 1:length(matdata2)){
  prof <- matdata2[[i]]
  if( length(prof) >= varno ) jend = length(prof)/varno
    for(j in 1:jend){
      prof_date <- prof[[1+(j-1)*varno]]
      prof_date <- as.POSIXct(prof_date, format = "%d-%b-%Y %H:%M:%S", tz = "UCT")
      prof_time <- prof[[2+(j-1)*varno]]
      prof_datetime <- prof_date + prof_time
      prof_pressure <- prof[[3+(j-1)*varno]]
      prof_eps <- prof[[4+(j-1)*varno]]
      prof_file <- prof[[5+(j-1)*varno]]
      prof_num_file <- rep(prof[[6+(j-1)*varno]], length(prof_pressure))
      prof_num <- rep(prof_num[1] + 1, length(prof_pressure))
      profj_df <- data.frame(datetime = as.character(prof_datetime),
                             time = prof_time,
                             pressure = prof_pressure,
                             eps1 = prof_eps[1,], eps2 = prof_eps[2,],
                             file_name = prof_file, prof_num_file = prof_num_file,
                             prof_num = prof_num)
      if(!exists('prof_df'))
        prof_df <- profj_df
      else
        prof_df <- rbind(prof_df, profj_df)
      }
}

prof_df$datetime <- as.POSIXct(prof_df$datetime)

prof_df <- prof_df[order(prof_df$datetime),]

# export data frame
# write.csv(prof_df, file = 'data_processed/VMP_table.csv', row.names = F)

