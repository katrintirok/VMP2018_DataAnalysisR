# plots_VMP_profiles

library(tidyverse)
library(lubridate)
library(shape)

source('functions/func_VMPprofilesPlot.R')

# for all plots
width <- 7
height <- width/sqrt(2)

# --- plot EPS ---
# read data 
eps <- read_csv('data_processed/VMP_table.csv') 
eps <- rename(eps, profile_nr = prof_num)

plotvar <- log10(eps$eps2)
figtitle <- 'lg(Eps2)'
vartitle <- expression(paste(epsilon, '(', m^2, '/', s^3, ')'))
fig <- func_VMPprofilesPlot(eps, plotvar, figtitle, vartitle)

filename <- paste('figures/Profiles_', figtitle, '.pdf', sep="")
ggsave(filename, fig, width = width, height = height)

# --- plot N ---
N <- read_csv('data_processed/VMP_N2_Tfast_disslength.csv')
plotvar <- log10(sqrt(N$N2_s_T1))
vartitle <- expression('lg(N'['T1']*') (s'^-1*')')
figtitle <- 'log(N_T1)'
fig <- func_VMPprofilesPlot(N, plotvar, figtitle, vartitle)

filename <- paste('figures/Profiles_', figtitle, '.pdf', sep="")
ggsave(filename, fig, width = width, height = height)

# --- plot L_T (Thorpe's scale) ---
plotvar <- N$tsc_T2 #log10(N$tsc_T2)
vartitle <- expression('L_T'[2]*' (m)')
figtitle <- 'Thorpe_2'
fig <- func_VMPprofilesPlot(N, plotvar, figtitle, vartitle)

filename <- paste('figures/Profiles_', figtitle, '.pdf', sep="")
ggsave(filename, fig, width = width, height = height)


# plot Temp
temp <- read_csv('data_processed/VMP_gswVariables.csv')
temp <- rename(temp, pressure = Pressure_dbar)

vartitle <- expression('T ('*degree*'C)')
figtitle <- 'Temperature'
plotvar <- temp$Temperature_degC
fig <- func_VMPprofilesPlot(temp, plotvar, figtitle, vartitle)

filename <- paste('figures/Profiles_', figtitle, '.png', sep="")
ggsave(filename, fig, width = width, height = height, dpi = 600)

# plot Salinity
vartitle <- 'SP'
figtitle <- 'Practical Salinity'
plotvar <- temp$SP
fig <- func_VMPprofilesPlot(temp, plotvar, figtitle, vartitle)

filename <- paste('figures/Profiles_', figtitle, '.png', sep="")
ggsave(filename, fig, width = width, height = height, dpi = 600)

# plot density
vartitle = expression(paste(rho, 'g/', m^3))
figtitle <- 'Density'
plotvar <- temp$rho
fig <- func_VMPprofilesPlot(temp, plotvar, figtitle, vartitle)

filename <- paste('figures/Profiles_', figtitle, '.png', sep="")
ggsave(filename, fig, width = width, height = height, dpi = 600)


# --- Ozmidov scale ---
# L_O = (eps/N^3)^1/2
# join eps and N dataset
eps$roundpr <- round(eps$pressure, 0)
N$roundpr <- round(N$pressure, 0)
epsN <- merge(eps, N, by=c('profile_nr', 'roundpr'))
# calculate osmodov for each eps1/2-N1/2 combination --> eps1-N1,eps1-N2,eps2-N1,eps2-N2, then average
epsNO <- epsN %>%
  mutate(L_O_11 = sqrt(eps1/(sqrt(N2_s_T1)^3)), L_O_12 = sqrt(eps1/(sqrt(N2_s_T2)^3)),
         L_O_21 = sqrt(eps2/(sqrt(N2_s_T1)^3)), L_O_22 = sqrt(eps2/(sqrt(N2_s_T2)^3)),
         L_O_av = (L_O_11 + L_O_12 + L_O_21 + L_O_22)/4) %>%
  rename(pressure = pressure.x) %>%
  select(profile_nr, pressure, L_O_av, tsc_T1, tsc_T2)

vartitle <- 'L_O'
figtitle <- 'Osmodov'
plotvar <- epsNO$L_O_av
fig <- func_VMPprofilesPlot(epsNO, plotvar, figtitle, vartitle)

filename <- paste('figures/Profiles_', figtitle, '.pdf', sep="")
ggsave(filename, fig, width = width, height = height)


# --- L_T/L_O ---
epsNO$LT1_LO <- epsNO$tsc_T1/epsNO$L_O_av
epsNO$LT2_LO <- epsNO$tsc_T2/epsNO$L_O_av

vartitle <- expression('L_T'[1]*'/L_O')
figtitle <- 'LT1_LO_ratio'
plotvar <- epsNO$LT1_LO
fig <- func_VMPprofilesPlot(epsNO, plotvar, figtitle, vartitle)

filename <- paste('figures/Profiles_', figtitle, '.pdf', sep="")
ggsave(filename, fig, width = width, height = height)
