# calculate N^2 from fast temperature profiles using function 'func_N2fastT.r'

library(tidyverse)

# load function
source('functions/func_LEfastT.R')

# get number of profiles
profiles <- read.csv('data_processed/VMP_profiles_info_GPS_clean.csv') %>%
  select(profile_nr)

LE <- data.frame()
start <- 1
#end <- 32
end <- length(profiles$profile_nr)
for(p in start:end){
  print(p)
  LE_p <- func_LEfastT(profiles$profile_nr[p])
  LE <- rbind(LE, LE_p)
}

# export data
write_csv(LE, 'data_processed/VMP_LE_LT_Tfast_disslength.csv')

# note: LE one less observation than vmp_table eps data --> profile 61 has 86 vs 87 values
