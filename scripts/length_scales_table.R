# make a table with all length scales
library(tidyverse)

#dat1 <- read_csv('data_processed/VMP_N2_Tfast_disslength.csv') %>%
#  arrange(profile_nr, pressure)
dat1 <- read_csv('data_processed/VMP_N2_RHOslow_disslength.csv') %>%
  arrange(profile_nr, pressure)

dat2 <- read_csv('data_processed/VMP_LE_LT_Tfast_disslength.csv') %>%
  arrange(profile_nr, pressure)

# epsilon data
dat3 <- read_csv('data_processed/VMP_table.csv') %>%
  rename(profile_nr = prof_num) %>%
  arrange(profile_nr, pressure)

# before concatenating: there is one value in profile 61 less in dat1 and dat2 than in dat3, 
# need to remove that in dat3 --> where pressure == floor(255) and profile_nr ==61
dat3 <- dat3[!(floor(dat3$pressure) == 255 & dat3$profile_nr == 61),]

# merge tables
dat4 <- dat1 %>%
  select(-c(pressure, profile_nr, tsc_R, tsc_Rp)) %>%
  cbind(dat2) %>%
  select(-c(pressure, profile_nr, LE_T1, LE_T2)) %>%
  cbind(dat3) %>%
  # calculate Ozmidov scale
  mutate(LO_1 = sqrt(eps1/(sqrt(N2_s_Rp)^3)),
                 LO_2 = sqrt(eps2/(sqrt(N2_s_Rp)^3))) %>%
  # calculate Kolmogorov scale
  mutate(LK_1 = ((10^-6)/eps1)^(1/4), LK_2 = ((10^-6)/eps2)^(1/4))  %>%
  select(-c(N2_s_Rp, N2_s_R, eps1, eps2, filename, prof_num_file, datetime, time)) %>%
  rename(LT_T1 = tsc_T1, LT_T2 = tsc_T2, LE_T1 = LEb_T1, LE_T2 = LEb_T2,
         LO_eps1 = LO_1, LO_eps2 = LO_2, LK_eps1 = LK_1, LK_eps2 = LK_2)


write_csv(dat4, 'data_processed/LengthScales_dissipationLength.csv')
