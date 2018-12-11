# Check salinity -- same as what quicklook in matlab gives?

# need to join data slow (CTD data) with gps data (has info on individual files)

library(dplyr)
library(ggplot2)
library(tidyr)

rm(list=ls())
gc()

# read in slow CTD data
dat_slow <- read.csv('data_processed/VMP_gswVariables.csv')
head(dat_slow)

# read profile infos
dat_prof <- read.csv('data_processed/VMP_profiles_info_GPS_clean.csv')
head(dat_prof)

# data combined
dat <- merge(dat_slow, dat_prof, by='profile_nr')

dat %>%
  filter(file_name == 'KZN_032' & profile_nr == 53) %>%
  arrange(Pressure_dbar, SA) %>%
  #head()
  ggplot() + geom_line(aes(y = SA, x = Pressure_dbar))

