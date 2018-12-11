# import microrider profiles and convert to data frame
library(tidyverse)

rm(list=ls())

# memory size
memory.size()
# garbage collection --> returns memory to operating system after clearing workspace, also returs overview of memory
gc()

### -------------------------------------------------- ###
# import prof_info
### -------------------------------------------------- ###
profiles <- read.csv('data/VMP_profiles_info.csv', stringsAsFactors = F)

# import GPS data
gps1 <- read.csv('data/20180129_Day1_GPSpoints.csv',stringsAsFactors = F, strip.white = T)
gps2 <- read.csv('data/20180130_Day2_GPSpoints.csv', stringsAsFactors = F,
                 strip.white = T)
gps3 <- read.csv('data/20180131_Day3_GPSpoints.csv', stringsAsFactors = F,
                 strip.white = T)
gps4 <- read.csv('data/20180201_Day4_GPSpoints.csv', stringsAsFactors = F,
                 strip.white = T)
gps <- rbind(gps1,gps2,gps3,gps4)

# 88 profiles in profiles, but only 86 gps pts in gps files???

# count per file name
gps_count <- gps %>%
  group_by(VMP_file) %>%
  tally()

prof_count <- profiles %>%
  group_by(file_name) %>%
  tally()

library('methods')
library('XML')

# Convert the input xml file to a data frame.
gpst1 <- xmlToDataFrame("data/Waypoints_29-JAN-18.xml")
gpst2 <- xmlToDataFrame("data/Waypoints_30-JAN-18.xml")
gpst3 <- xmlToDataFrame("data/Waypoints_31-JAN-18.xml")
gpst4 <- xmlToDataFrame("data/Waypoints_01-FEB-18.xml")

gpst <- rbind(gpst1, gpst2, gpst3, gpst4)
gpst$GPS_pt <- as.integer(gpst$name)

# join with gps1
gpst <- gpst[c(2,6)]
gpsm <- merge(gps, gpst, by ='GPS_pt', all = T)

# join gps info with profile info
profiles_gps_m <- merge(profiles, gpsm, by = 'profile_nr')





### ------------------------------------------------------------
# read slow data
data_sl <- read.csv('data/VMP_profiles_slow.csv')
subdata <- data_sl %>%
  filter(profile_no %in% c(29,30)) %>%
  ggplot() + geom_line(aes(x = time_sec, y = Pressure_dbar, col = as.factor(profile_no)))
subdata

# remove false profiles (when knot removed etc.)
# to remove: 29, 30, 55, 79
data_sl_clean <- data_sl %>%
  filter(!(profile_no %in% c(29, 30, 55, 79)))

### --- change profile numbers to have them in ascending order without gaps
# 31:54 --> -2, 56-78 --> -3, 80-88 --> -4
# data_sl_clean$profile_nrc <- ifelse(data_sl_clean$profile_no < 31, 
#                                     data_sl_clean$profile_no, 
#                                     ifelse(data_sl_clean$profile_no<55, 
#                                     data_sl_clean$profile_no - 2, 
#                                     ifelse(data_sl_clean$profile_no<79,
#                                            data_sl_clean$profile_no - 3, 
#                                            data_sl_clean$profile_no - 4)))





### ------------------------------------------------------------------------
# clean fast data
### ------------------------------------------------------------
# read slow data
data_fs <- read.csv('data/VMP_profiles_fast.csv')

# remove false profiles (when knot removed etc.)
# to remove: 29, 30, 55, 79
data_fs_clean <- data_fs %>%
  filter(!(profile_no %in% c(29, 30, 55, 79)))
rm(data_fs)
gc()





### -------------------------------------------------------------------
# save cleaned data
# save profile_info
profOut <- select(profiles_gps_m, -c(VMP_file,time_SAST, time), depth_m_echosound = depth_m)
profOut <- profOut[,c(1,2,3,5,4,6,7,8)]
#write.csv(profOut ,file = 'data_processed/VMP_profiles_info_GPS_clean.csv', row.names = F)

# save data_slow
slowOut <- rename(data_sl_clean, profile_nr = profile_no)
slowOut[,2:3] <- lapply(slowOut[,2:3], round, 4)
slowOut[,4:5] <- lapply(slowOut[,4:5], round, 2)
#write.csv(slowOut, file = 'data_processed/VMP_profiles_slowData_cleaned.csv', row.names = F)


# save data_fast
fastOut <- rename(data_fs_clean, profile_nr = profile_no)
rm(data_fs_clean)
gc()
fastOut[,3] <- lapply(fastOut[,3], round, 6)
fastOut[,c(2,4:7)] <- lapply(fastOut[,c(2,4:7)], round, 4)
#write.csv(fastOut, file = 'data_processed/VMP_profiles_fastData_cleaned.csv', row.names = F)

