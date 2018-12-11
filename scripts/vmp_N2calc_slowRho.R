# calculate N^2 from slow temperature profiles using function 'func_N2slowT.r'

library(tidyverse)

# load function
source('functions/func_N2slowRho.R')

# get number of profiles
profiles <- read.csv('data_processed/VMP_profiles_info_GPS_clean.csv') %>%
  select(profile_nr)

N2 <- data.frame()
for(p in 1:length(profiles$profile_nr)){
  print(p)
  N2_p <- func_N2slowRho(profiles$profile_nr[p])
  N2 <- rbind(N2, N2_p)
}

# export data
write_csv(N2, 'data_processed/VMP_N2_RHOslow_disslength.csv')

## note: N2 one less observation than vmp_table eps data --> profile 61 has 86 vs 87 values

# load function
source('functions/func_N2slowRho.R')


# ---------------------------------------------------------------------------------
# N2 with rho estimated from rho-temp linear model!
# ---------------------------------------------------------------------------------

# load function
source('functions/func_N2slowRhoT.R')

N2_RT <- data.frame()
for(p in 1:length(profiles$profile_nr)){
  p
  N2_p <- func_N2slowRhoT(profiles$profile_nr[p])
  N2_RT <- rbind(N2_RT, N2_p)
}

# export data
#write_csv(N2_RT, 'data_processed/VMP_N2_RHOTslow_disslength.csv')


# -------------------------------------------------------------------------------------
# compare N2_RHOs with N2_RHOTs
N2_rho <- read.csv('data_processed/VMP_N2_RHOslow_disslength.csv')
N2_rhoT <- read.csv('data_processed/VMP_N2_RHOTslow_disslength.csv')

# merge data
N2s <- N2_rho %>%
  select(-N2_s_Rp, -tsc_Rp) %>%
  merge(N2_rhoT, by=c('profile_nr','pressure')) %>%
  rename(N2_rho_s = N2_s_R.x, N2_rhoT_s = N2_s_R.y) %>%
  select(-c(tsc_R.x, tsc_R.y))

ggplot(data = N2s) + 
  geom_point(aes(x = N2_rho_s, y = N2_rhoT_s)) +
  geom_abline(slope = 1, intercept = 0, col='blue')

ggplot(data = gather(N2s, key=N2_var, -profile_nr, -pressure, value=N2)) +
  geom_density(aes(log10(N2), col = N2_var))

ggplot(data = gather(N2s, key=N2_var, -profile_nr, -pressure, value=N2)) +
  geom_point(aes(x = pressure, y = N2, col = N2_var), alpha = 0.2) +
  scale_x_reverse() +
  coord_flip() +
  theme_bw()
          
ggplot(data = N2s) + 
  geom_point(aes(x = pressure, y = N2_rho_s - N2_rhoT_s), alpha = 0.2) +
  scale_x_reverse() +
  coord_flip() +
  theme_bw()
  

# compare pot and not pot rho N2s
# merge data
ggplot(data = N2_rho) + 
  geom_point(aes(x = N2_s_Rp, y = N2_s_R)) +
  geom_abline(slope = 1, intercept = 0, col='blue')

ggplot(data = gather(N2_rho, key=N2_var, -profile_nr, -pressure, -tsc_R, -tsc_Rp, value=N2)) +
  geom_density(aes(log10(N2), col = N2_var))

ggplot(data = gather(N2_rho, key=N2_var, -profile_nr, -pressure, -tsc_R, -tsc_Rp, value=N2)) +
  geom_point(aes(x = pressure, y = N2, col = N2_var), alpha = 0.2) +
  scale_x_reverse() +
  coord_flip() +
  theme_bw()

ggplot(data = N2s) + 
  geom_point(aes(x = pressure, y = N2_rho_s - N2_rhoT_s), alpha = 0.2) +
  scale_x_reverse() +
  coord_flip() +
  theme_bw()