# calculate sea water quantities

# load packages
library(dplyr)
library(gsw)      #gibbs sea water library

#clear workspace
rm(list=ls())

#import data, use slow data
vmp_slow <- read.csv("data_processed/VMP_profiles_slowData_cleaned.csv", header=T, sep=',')

C <- vmp_slow$Conductivity_mS_per_cm
t <- vmp_slow$Temperature_degC
p <- vmp_slow$Pressure_dbar

# practical salinity using conductivity in ...?
SP = gsw_SP_from_C(C,t,p)

# absolute salinity
#average latitude for vmp profiles = -28.08 and av long = 32.75
long <- 32.7428
lat <- -27.5242
SA = gsw_SA_from_SP(SP,p,long,lat)

# conservative temperature from in-situ temperature
CT = gsw_CT_from_t(SA,t,p)

# potential temperature from conservative temperature
PT = gsw_pt_from_CT(SA,CT)

# in-situ density
rho = gsw_rho(SA,CT,p)

# potential density
#Reference pressures are often chosen as a whole multiple of 100 bar; 
#for water near a pressure of 400 bar (40 MPa), say, the reference pressure 400 bar 
#would be used, and the potential density anomaly symbol would be written ? 4 
#{\displaystyle \sigma _{4}}. 
#Using these reference pressures can be defined the potential density surfaces, 
#used in the analyses of ocean data and to construct models of the ocean current. 
#Neutral density surfaces, defined using another variable called neutral density
#( ? n {\displaystyle \gamma ^{n}}), can be considered the continuous analog of 
#these potential density surfaces.
p_ref = 0      #[dbar] --> =0bar (next would be 100bar=1000dbar, but max p of 300dbar closer to 0
rho_pot = gsw_rho(SA,CT,p_ref)  #varies between 1023 and 1027


# add new variables to dataframe
vmp_slowb <- data.frame(vmp_slow,SP,SA,CT,PT,rho,rho_pot)

#export data
#write.csv(vmp_slowb,'data_processed/VMP_gswVariables.csv',row.names=F)

