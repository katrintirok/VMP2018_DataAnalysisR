# import microrider profiles and convert to data frame

library(R.matlab)

rm(list=ls())

# memory size
memory.size()
# garbage collection --> returns memory to operating system after clearing workspace, also returs overview of memory
gc()

### -------------------------------------------------- ###
# process slow data
### -------------------------------------------------- ###
matdata <- readMat('data/VMP_profiles_slow.mat')
matdata <- unlist(matdata, recursive = F, use.names = T)
matdata <- unlist(matdata, recursive = F, use.names = T)  

origin <- as.POSIXct('0000-01-01 00:00:00.000',format = "%Y-%m-%d %H:%M:%OS", tz = 'UCT')
prof_sl <- data.frame()
for(i in 1:1){#matdata){
  # get column names
  cols <- dimnames(matdata[[i]])[[1]]
  # convert list to data frame
  prof_sli <- as.data.frame.list(matdata[[i]], optional = T, col.names = cols)
  # convert datetime to POSIXct
  origin <- as.character(prof_sli$matlabnum.base.YMDHMS[1])
  prof_sli$datetime <- as.POSIXct.numeric(x = prof_sli$datetime.matlabnum, 
                                          tz = 'UCT', 
                                          origin = origin, format = "%Y-%m-%d %H:%M:%OS")
  prof_sl <- rbind(prof_sl, prof_sli)
  }

rm(matdata)
gc()
#memory.size()


prof_df$datetime <- as.POSIXct(prof_df$datetime)

prof_df <- prof_df[order(prof_df$datetime),]

# export data frame
#write.csv(profMR_df, file = 'data_processed/prof_table.csv', row.names = F)


val <- time1[1] #733038.6
as.POSIXct((val - 719529)*60*60*24, origin = "1970-01-01", tz = "UTC")
[1] "2006-12-27 14:23:59 UTC"
