# import microrider profiles and convert to data frame

library(R.matlab)

#rm(list=ls())

matdata <- readMat('data/VMP_Rolfs_profiles.mat')
matdata1 <- unlist(matdata, recursive = F, use.names = T)
matdata2 <- unlist(matdata1, recursive = F, use.names = T)  
matdata3 <- unlist(matdata2, recursive = F, use.names = T)
matdata4 <- unlist(matdata3, recursive = F, use.names = T)

prof_numi <- 0
for(i in 1:length(matdata4)){
  prof <- matdata4[[i]]
  if(length(prof) > 0)
  for(j in 1:length(prof)){
    profj <- prof[[j]]
    len_prof <- length(profj[[1]][[4]])
    prof_datej <- profj[[1]][[1]]
    prof_date <- prof_datej[1]
    for(k in 2:length(prof_datej)){
      prof_date <- paste(prof_date, prof_datej[k], sep='')
    }
    prof_time <-  profj[[1]][[2]]
    prof_datetime_start <- as.POSIXct(paste(prof_date, prof_time), 
                                format='%Y-%m-%d %H:%M:%S', tz='UCT')
    prof_sec <- profj[[1]][[3]]
    prof_datetime <- prof_datetime_start + prof_sec
    prof_pressure <- profj[[1]][[4]] 
    prof_eps <- profj[[1]][[5]] 
    prof_filename <- rep(profj[[1]][[6]], len_prof) 
    prof_num_file <- rep(as.numeric(profj[[1]][[7]]), len_prof)
    prof_numi <- prof_numi + 1
    prof_num <- rep((prof_numi), len_prof)
    
    profj_df <- data.frame(datetime = as.character(prof_datetime),
                             time = prof_sec,
                             pressure = prof_pressure,
                             eps1 = prof_eps[1,], eps2 = prof_eps[2,],
                             filename = prof_filename,
                             prof_num_file = prof_num_file,
                             prof_num = prof_num)
      if(!exists('prof_df'))
        prof_df <- profj_df
      else
        prof_df <- rbind(prof_df, profj_df)
      }
}

prof_df$datetime <- as.POSIXct(prof_df$datetime, tz = 'UCT')

prof_df <- prof_df[order(prof_df$datetime),]

# kick out useless profiles as in VMP_profiles_info_GPS
prof_df_clean <- prof_df %>%
  filter(!(prof_num %in% c(29, 30, 55, 79)))

# export data frame
write.csv(prof_df_clean, file = 'data_processed/VMP_table.csv', row.names = F)

