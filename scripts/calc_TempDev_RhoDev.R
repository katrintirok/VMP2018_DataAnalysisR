# calculate deviations in Temp and Rho

# --- load libraries ---
library(dplyr)
library(dbplyr)
library(gsw)

# --- read data ---
#connect data base SQLite_VMP
vmp_profiles <- DBI::dbConnect(RSQLite::SQLite(), "data/SQLite_VMP.sqlite")
# read out data from VMP_profiles_fastData table for profile given as input to function
vmp <- tbl(vmp_profiles, "VMP_profiles_fastData")
gps <- tbl(vmp_profiles, 'VMP_profiles_info_GPS_clean')
vmp_df <- vmp %>%
  filter(profile_nr == profnum) %>%
  # to get latitude for profile, join with gps table
  inner_join(., gps, by = 'profile_nr') %>%
  # choose wich variables needed for further calculations
  select(Pressure_dbar, T1_degC, T2_degC, lat) %>%
  # collect data from database to put into tibble
  collect()
# disconnect from database
DBI::dbDisconnect(vmp_profiles)

# ---------------------------------------------------------
# calculate N and N^2 from fast temperature profiles, 
# i.e. sort temperature profile like normally density profile

# --- 1) sort profiles for decreasing temperature ---
b_fast <- vmp_df
# sort pressure increasing
b_fast <- b_fast[order(b_fast$Pressure_dbar, decreasing=F),]
# make subset with pressure and temp to be sorted for temp
b_T1 <- select(b_fast,Pressure_dbar,T1_degC)   #pressure, T1
b_T2 <- select(b_fast, Pressure_dbar, T2_degC) #pressure, T2
# sort T1 and T2 decreasing!, in subsets
b_T1s <- b_T1[order(b_T1$T1_degC, decreasing=T),]
b_T2s <- b_T2[order(b_T2$T2_degC, decreasing=T),]
# add sorted T1 and T2 columns to dataframe b  
b_fast$T1_s <- b_T1s$T1_degC
b_fast$T2_s <- b_T2s$T2_degC
# add sorted T1/T2 pressure to calculate Thorpe's scales for T1/T2
b_fast$pressure_T1s <- b_T1s$Pressure_dbar
b_fast$pressure_T2s <- b_T2s$Pressure_dbar
b_fast$tsc_T1 <- b_fast$pressure_T1s - b_fast$Pressure_dbar
b_fast$tsc_T2 <- b_fast$pressure_T2s - b_fast$Pressure_dbar
b_fast$T1_dev <- c(NaN,abs(b_fast$T1_s[2:length(b_fast$T1_s)] - b_fast$T1_s[1:length(b_fast$T1_s)-1]))/mean(b_fast$T1_s, na.rm=T)
b_fast$T2_dev <- c(NaN,abs(b_fast$T2_s[2:length(b_fast$T2_s)] - b_fast$T2_s[1:length(b_fast$T2_s)-1]))/mean(b_fast$T2_s, na.rm=T)


# slow data
vmp_profiles_sl <- DBI::dbConnect(RSQLite::SQLite(), "data/SQLite_VMP.sqlite")
# read out data from VMP_profiles_slowData_cleaned table for profile given as input to function
vmp <- tbl(vmp_profiles_sl, "VMP_profiles_slowData_cleaned")
gps <- tbl(vmp_profiles_sl, 'VMP_profiles_info_GPS_clean')
vmp_df <- vmp %>%
  filter(profile_nr == profnum) %>%
  # to get latitude for profile, join with gps table
  inner_join(., gps, by = 'profile_nr') %>%
  # choose wich variables needed for further calculations
  select(Pressure_dbar, Temperature_degC, Conductivity_mS_per_cm, lat) %>%
  # collect data from database to put into tibble
  collect()
# disconnect from database
DBI::dbDisconnect(vmp_profiles_sl)

C <- vmp_df$Conductivity_mS_per_cm
t <- vmp_df$Temperature_degC
p <- vmp_df$Pressure_dbar

# practical salinity using conductivity in ...?
SP = gsw_SP_from_C(C,t,p)
# absolute salinity
#average latitude for vmp profiles = -27.52277 and av long = 32.7259
long <- 32.7259
lat <- -27.52277
SA = gsw_SA_from_SP(SP,p,long,lat)
# conservative temperature from in-situ temperature
CT = gsw_CT_from_t(SA,t,p)
# in-situ density
rho = gsw_rho(SA,CT,p)
# potential density
p_ref = 0      #[dbar] --> =0bar (next would be 100bar=1000dbar, but max p of 300dbar closer to 0
rho_pot = gsw_rho(SA,CT,p_ref)

vmp_df$rho_pot <- rho_pot

rm(SP,SA,CT,rho,rho_pot,lat, long, C, p, t, p_ref)


# --- 1) sort profiles for indreasing density ---
b_slow <- vmp_df
# sort pressure increasing
b_slow <- b_slow[order(b_slow$Pressure_dbar, decreasing=F),]
# make subset with pressure and density to be sorted for dens
b_R <- select(b_slow,Pressure_dbar,rho_pot)   #pressure, rho
b_T <- select(b_slow,Pressure_dbar,Temperature_degC)   #pressure, rho
# sort rho increasing!, in subsets
b_Rs <- b_R[order(b_R$rho_pot, decreasing=F),]
b_Ts <- b_T[order(b_T$Temperature_degC, decreasing=T),]
# add sorted Temp column to dataframe b  
b_slow$R_s <- b_Rs$rho_pot
b_slow$T_s <- b_Ts$Temperature_degC
# add sorted Temp pressure to calculate Thorpe's scales for density
b_slow$pressure_Rs <- b_Rs$Pressure_dbar
b_slow$pressure_Ts <- b_Ts$Pressure_dbar
b_slow$tsc_R <- b_slow$pressure_Rs - b_slow$Pressure_dbar
b_slow$tsc_T <- b_slow$pressure_Ts - b_slow$Pressure_dbar
b_slow$R_dev <- c(NaN, abs(b_slow$R_s[2:length(b_slow$R_s)] - b_slow$R_s[1:length(b_slow$R_s)-1]))/mean(b_slow$R_s, na.rm=T)
b_slow$T_dev <- c(NaN,abs(b_slow$T_s[2:length(b_slow$T_s)] - b_slow$T_s[1:length(b_slow$T_s)-1]))/mean(b_slow$T_s, na.rm=T)


# prepare data for plot
pl.fast <- b_fast %>% 
  select(Pressure_dbar, T1_dev, T2_dev, tsc_T1, tsc_T2) %>%
  gather(key = dev_var, -Pressure_dbar, -tsc_T1, -tsc_T2, value=dev) %>%
  gather(key = displ_var, -Pressure_dbar, -dev_var, -dev, value=displ)

pl.slow <- b_slow %>%
  select(Pressure_dbar, T_dev, R_dev, tsc_T, tsc_R) %>%
  gather(key = dev_var, -Pressure_dbar, -tsc_T, -tsc_R, value=dev) %>%
  gather(key = displ_var, -Pressure_dbar, -dev_var, -dev, value=displ)

pl.data <- rbind(pl.fast, pl.slow)

#--- density plots of the deviations ... ---
pd1 <- ggplot(data = filter(pl.data, displ_var %in% c('tsc_T','tsc_R','tsc_T1','tsc_T2')), aes(x=log10(displ), col=displ_var)) + 
  geom_density() +
  scale_colour_discrete(name="dev source",
                        breaks=c("R_dev", "T_dev", "T1_dev", "T2_dev"),
                        labels=c(expression(rho['slow']), 
                                 expression("T"['slow']), 
                                 expression("T1"['fast']),
                                 expression("T2"['fast']))) +
  labs(title = 'distributions of deviation values (log10) \ncalculated from slow data vs fast data', 
       x = 'log10(deviation)') +
  theme_bw()
pd1
