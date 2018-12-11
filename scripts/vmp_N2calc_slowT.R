# calculate N^2 from slow temperature profiles using function 'func_N2slowT.r'

library(tidyverse)

# load function
source('functions/func_N2slowT.R')

# get number of profiles
profiles <- read.csv('data_processed/VMP_profiles_info_GPS_clean.csv') %>%
  select(profile_nr)

N2 <- data.frame()
for(p in 1:length(profiles$profile_nr)){
  p
  N2_p <- func_N2slowT(profiles$profile_nr[p])
  N2 <- rbind(N2, N2_p)
}

# export data
#write_csv(N2, 'data_processed/VMP_N2_Tslow_disslength.csv')

# note: N2 one less observation than vmp_table eps data --> profile 61 has 86 vs 87 values
