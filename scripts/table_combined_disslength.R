# Create a table with all the variables calculated and averaged over diss_length 
# as used for estimation of epsilon


# Load libraries ----------------------------------------------------------
library(tidyverse)


# Read in data ------------------------------------------------------------

# epsilon data
eps <- read_csv('data_processed/VMP_table.csv') %>%
  rename(profile_nr = prof_num) %>%
  arrange(profile_nr, pressure)
# before concatenating: there is one value in profile 61 less in N2 and LELT than in eps data, 
# need to remove that in eps data --> where pressure == floor(255) and profile_nr ==61
eps <- eps[!(floor(eps$pressure) == 255 & eps$profile_nr == 61),]

# N2 data from potential density from slow CTD data
N2 <- read_csv('data_processed/VMP_N2_RHOslow_disslength.csv') %>%
  arrange(profile_nr, pressure)

# Ellison scale and Thorpe scale from fast temperature data
LELT <- read_csv('data_processed/VMP_LE_LT_Tfast_disslength.csv') %>%
  arrange(profile_nr, pressure)

# slow CTD data for averaging SP and T
ctd <- read_csv('data_processed/VMP_gswVariables.csv')

# gps data
gps <- read.csv('data_processed/VMP_profiles_info_GPS_clean.csv')

# bathy data
bathy <- read.csv('data_processed/BathyFine_perProfile.csv')


# Average temp and salinity -----------------------------------------------
# average slow CTD data over same diss_length as rest of data
# source function
source('functions/func_ctd_av.R')
# get number of profiles
profiles <- select(gps,profile_nr)

ctd_av <- data.frame()
start <- 1
end <- length(profiles$profile_nr)
for(p in start:end){
  print(paste('averaging profile', p))
  ctd_av_p <- func_ctd_av(profdata = ctd[ctd$profile_nr == profiles$profile_nr[p],])
  ctd_av <- rbind(ctd_av, ctd_av_p)
}
# before concatenating: there is one value in profile 61 less in N2 and LELT than in ctd_av data, 
# need to remove that in ctd_av data --> where pressure == floor(255) and profile_nr ==61
ctd_av <- ctd_av[!(floor(ctd_av$pressure) == 255 & ctd_av$profile_nr == 61),] %>%
  arrange(profile_nr, pressure)


# Join data tables --------------------------------------------------------
data_merged <- N2 %>%
  select(-c(pressure, profile_nr, tsc_R, tsc_Rp)) %>%
  cbind(LELT) %>%
  select(-c(pressure, profile_nr, LE_T1, LE_T2)) %>%
  cbind(ctd_av) %>%
  select(-c(pressure, profile_nr)) %>%
  cbind(eps) %>%
  # calculate Ozmidov scale
  mutate(LO_1 = sqrt(eps1/(sqrt(N2_s_Rp)^3)),
         LO_2 = sqrt(eps2/(sqrt(N2_s_Rp)^3))) %>%
  # calculate Kolmogorov scale
  mutate(LK_1 = ((10^-6)/eps1)^(1/4), LK_2 = ((10^-6)/eps2)^(1/4))  %>%
  select(-c(N2_s_R, prof_num_file, time)) %>%
  rename(N2 = N2_s_Rp, LT_T1 = tsc_T1, LT_T2 = tsc_T2, LE_T1 = LEb_T1, LE_T2 = LEb_T2,
         LO_eps1 = LO_1, LO_eps2 = LO_2, LK_eps1 = LK_1, LK_eps2 = LK_2)


# Join with gps data ------------------------------------------------------
data_merged_gps <- data_merged %>%
  merge(gps, by = 'profile_nr')


# Join with bathy data ----------------------------------------------------
data_merged_bathy <- data_merged_gps %>%
  merge(bathy, by = 'profile_nr')


# Clean table -------------------------------------------------------------
# order columns with profile_nr etc first
data_merged_sort <- data_merged_bathy %>%
  rename(depth_m_echosound = depth_m_echosound.x) %>%
  select(sample_day, filename, datetime, profile_nr, lat, long, depth_m_echosound, bathy, pressure, Temp_degC, SP, rho, 
         eps1, eps2, N2, 
         LT_T1, LT_T2, LE_T2, LE_T2, LO_eps1, LO_eps2, LK_eps1, LK_eps2)

# reduce precision of float point numbers
data_merged_sort[,8] <- round(data_merged_sort[,8])
data_merged_sort[,12] <- signif(data_merged_sort[,12], 6)
data_merged_sort[,c(9:11,13:22)] <- lapply(data_merged_sort[,c(9:11,13:22)], signif, 4)


# Export new table --------------------------------------------------------
#write_csv(data_merged_sort, 'data_processed/VMP_profiles_VarCombined_disslength.csv')

