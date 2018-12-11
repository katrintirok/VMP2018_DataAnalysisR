# calculate N2 with depth avereraged data

library(tidyverse)
library(lubridate)
library(gsw)      #gibbs sea water library
library(shape)   # for femmecol colour palette (like jet in matlab)

# ----------------------------------------------------------------
# --- data import, rename variable profile_no in vmp, add variable sample_day to gps table---
# ----------------------------------------------------------------
vmp <- read_csv('data/VMP_profiles_averaged.csv') %>%
  rename('profile_nr' = 'profile_no')

gps <- read_csv('data_processed/VMP_profiles_info_GPS_clean.csv') %>%
  mutate(sample_day = factor(day(datetime_start_YMDHMS_UTC), levels=c(29,30,31,1), labels=c('Day1','Day2','Day3','Day4'))) %>%
  select(c('profile_nr', 'sample_day', 'lat', 'long'))

# -----------------------------------------------------------
# --- calculate sea water chars for average data ----------
# -----------------------------------------------------------
C <- vmp$Conductivity_mS_per_cm
t <- vmp$Temperature_degC
p <- vmp$Pressure_dbar

# practical salinity using conductivity in ...?
SP = gsw_SP_from_C(C,t,p)

# absolute salinity
#average latitude for vmp profiles = -27.52277 and av long = 32.7259
long <- 32.7259
lat <- -27.52277
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
rho_pot = gsw_rho(SA,CT,p_ref)

vmp_gsw <- data.frame(vmp,SP,SA,CT,PT,rho,rho_pot)

rm(SP,SA,CT,PT,rho,rho_pot,lat, long, C, p, t, p_ref)



# ---------------------------------------------------------------------------
# --- calculate N^2 with sorted (over pressure) and unsorted data ---
# ---------------------------------------------------------------------------

vmp_gsw_a <- vmp_gsw %>%
  inner_join(gps, by = 'profile_nr') %>%
  select(profile_nr, Pressure_dbar, SA, CT, rho_pot, lat)

vmp_gsw_s <- data.frame()
for(i in 1:max(vmp_gsw_a$profile_nr)){
  vmp_gsw_ai <- subset(vmp_gsw_a, profile_nr==i)
  #1st sort dataframe for pressure
  vmp_gsw_ai <- vmp_gsw_ai[order(vmp_gsw_ai$Pressure_dbar, decreasing=F),]
  #2nd add sorted rho column and SA and CT
  vmp_gsw_ai_r <- select(vmp_gsw_ai, Pressure_dbar, SA, CT, rho_pot)   #pressure, SA, CT, rho_pot
  vmp_gsw_ai_rs <- vmp_gsw_ai_r[order(vmp_gsw_ai_r$rho_pot, decreasing=F),]
  vmp_gsw_ai$rho_pot_s <- vmp_gsw_ai_rs$rho_pot   # sorted rho_pot
  vmp_gsw_ai$CT_s <- vmp_gsw_ai_rs$CT             # sorted conservative temp
  vmp_gsw_ai$SA_s <- vmp_gsw_ai_rs$SA             # sorted absolute salinity
  vmp_gsw_ai$pressure_s <- vmp_gsw_ai_rs$Pressure_dbar    # sorted pressure
  
  vmp_gsw_s <- rbind(vmp_gsw_s, vmp_gsw_ai)
}

#calculate N2 of sorted densities and unsorted densities, per profile --> make this a function!
vmp_N2 <- data.frame()
for(i in 1:max(vmp_gsw_s$profile_nr)){
  vmp_gsw_si <- subset(vmp_gsw_s, profile_nr==i)
  if(dim(vmp_gsw_si)[1] > 0){
    #sort for depth(p)
    vmp_gsw_si <- vmp_gsw_si[order(vmp_gsw_si$Pressure_dbar, decreasing=F),]
    attach(vmp_gsw_si)
    N2_s <- gsw_Nsquared(SA_s,CT_s,Pressure_dbar,lat)
    N2   <- gsw_Nsquared(SA,CT,Pressure_dbar,lat)
    grav <- gsw_grav(lat)
    # make this a function
    N2m_s <- (grav[2:length(grav)]/rho_pot_s[2:length(rho_pot_s)] * 
                (rho_pot_s[2:length(rho_pot_s)]-rho_pot_s[1:length(rho_pot_s)-1])/
                (Pressure_dbar[2:length(Pressure_dbar)]-Pressure_dbar[1:length(Pressure_dbar)-1]))
    N2m   <- (grav[2:length(grav)]/rho_pot[2:length(rho_pot)] *
                (rho_pot[2:length(rho_pot)]-rho_pot[1:length(rho_pot)-1])/
                (Pressure_dbar[2:length(Pressure_dbar)]-Pressure_dbar[1:length(Pressure_dbar)-1]))
    tsc <- pressure_s - Pressure_dbar
    N2i <- data.frame(profile_nr=profile_nr[1:length(profile_nr)-1], pressure=N2$p_mid, 
                      N2_s=N2_s$N2, N2=N2$N2, N2m_s, N2m, tsc=tsc[1:length(tsc)-1])
    detach(vmp_gsw_si)
    vmp_N2 <- rbind(vmp_N2, N2i)
  } #end if statement
} # end for loop through profiles

#where pressure was equal, Inf values generated --> delete
vmp_N2$N2[vmp_N2$N2==-Inf | vmp_N2$N2==Inf] <- NaN 
vmp_N2 <- na.omit(vmp_N2)

# export N2 data
#write.csv(vmp_N2, 'data_processed/VMP_N2_averaged.csv', row.names = F)


# ---------------------------------------------------------------------------
# --- plot N2 values as profiles over longitude per latitude/sample day ---
# ---------------------------------------------------------------------------

# join N2 table and gps table
vmp_N2_gps <- inner_join(vmp_N2, gps, by = 'profile_nr')
head(vmp_N2_gps)

# define average latitude per day to plot per latitude not per day
vmp_N2_gps_l <- vmp_N2_gps %>%
  group_by(sample_day) %>%
  summarise(lat_av = factor(round(mean(lat),2), 
                            levels = c('-27.48', '-27.51', '-27.53', '-27.56'))) %>%
  inner_join(vmp_N2_gps, by = 'sample_day')
head(vmp_N2_gps_l)

pl.data <- vmp_N2_gps_l

#plot N2 over pressure
#plot(pl.data$N2m_s, pl.data$pressure, ylim = rev(range(pl.data$pressure)))
#plot(pl.data$tsc, pl.data$pressure, ylim=rev(range(pl.data$pressure)))

# plot all N values over depth
plot_Nall <- ggplot(data = pl.data, mapping = aes(x = log10(sqrt(N2m_s)), y = pressure, col = long)) +
  geom_point(alpha = 0.3) +
  scale_y_reverse(limits=c(500,0)) +
  scale_x_continuous(breaks = seq(-3.5, -1.5, by = 0.5), limits = c(-3.5, -1.5),
                     label = c('', expression(10^-3), '', expression(10^-2), '')) + 
  labs(x = 'N (1/s)',
       y = 'depth (m)',
       title = 'all N profiles, N averaged over same depths as eps',
       subtitle = 'shades of blue represent longitude') +
  theme_bw()

# write plot to file
#pdf('figures/N_all-profiles_depth-averaged.pdf', width=7/sqrt(2),height=7)
#plot_Nall
#dev.off()

# -------------------------------------------------------------------------------------
# --- plotting 'colour' profiles for N (sqrt of N2m_s --> N^2 manual from sorted profiles)
# -------------------------------------------------------------------------------------
# define colours for plotting N
pl.data$lg.Nm_s <- log10(sqrt(pl.data$N2m_s))   # convert N^2 to N and take log10
topW <- quantile(pl.data$lg.Nm_s, probs=c(0.75)) + IQR(pl.data$lg.Nm_s)
botW <- quantile(pl.data$lg.Nm_s, probs=c(0.25)) - IQR(pl.data$lg.Nm_s)
top <- ifelse(topW>max(pl.data$lg.Nm_s), max(pl.data$lg.Nm_s), topW)   #-1.682539  for kriged
bot <- ifelse(botW<min(pl.data$lg.Nm_s), min(pl.data$lg.Nm_s), botW)   #-2.509479 for kriged
#top <- -1.683
#bot <- -2.509

#make factor variable for colours
var <- pl.data$lg.Nm_s
colf <- ifelse(is.na(var),NaN,
               ifelse(var<bot,bot,
                      ifelse(var>bot & var<=top,var,
                             top)))
pl.data$colf <- colf

# Plot N^2 over longitude per latitude
plot1 <- ggplot(data = pl.data, mapping = aes(x = long, y = pressure))
# plot with facet lat_av and eps
plot2 <- plot1 + 
  geom_point(mapping = aes(colour = colf), pch = 15, size = 1) +
  facet_grid(lat_av ~ .) +
  scale_y_reverse() +
  scale_color_gradientn(colours = femmecol(200), na.value = 'transparent',
                        guide = guide_colorbar(title=expression(paste('N (', s^-1, ')     '))),
                        limits=c(min(pl.data$colf),max(pl.data$colf)), breaks=seq(-2.6,-1.6,0.4), 
                        label = c(expression(paste('0.25 ', 10^-2)), expression(paste('0.63 ', 10^-2)),
                                  expression(paste('1.6  ', 10^-2)))) +
  theme_bw()

plot2


#---------------------------------------------
# plot lg_N with spread between profiles
# ---------------------------------------------------
pl.data_diff <- pl.data %>%
  group_by(sample_day, long) %>%
  summarise(mN2 = mean(N2m_s)) %>%
  mutate(longpl = long) %>%
  mutate(diff = c(NA,diff(longpl, lag = 1))) %>%
  select(sample_day, long, mN2, longpl, diff)

mindiff <- 0.002
while(sum(round(pl.data_diff$diff,3)<mindiff, na.rm = T) != 0){
  #for(i in 1:length(pl.data_diff$diff)){
  #  if(!is.na(pl.data_diff$diff[i]) & pl.data_diff$diff[i] < mindiff){
  #    pl.data_diff$longpl[i] <- pl.data_diff$longpl[i] + mindiff - pl.data_diff$diff[i]
  #  }
  #}
  pl.data_diff$longpl <- ifelse(!is.na(pl.data_diff$diff) & pl.data_diff$diff < mindiff, 
                                pl.data_diff$longpl + mindiff - pl.data_diff$diff, 
                                pl.data_diff$longpl)
  pl.data_diff <- pl.data_diff %>%
    mutate(diff = c(NA,diff(longpl, lag = 1)))
}

# now join long1 again with datj_lat using sample-day and long as keys
pl.data_spread <- inner_join(pl.data, pl.data_diff, by=c('sample_day','long')) %>%
  select(-diff, mN2)
head(pl.data_spread)

# define values for hlines to show original positions of profiles
orig_lines <- pl.data_spread %>%
  select(lat_av, lg.Nm_s, long)
head(orig_lines)

## plot
plot1a <- ggplot(data = pl.data_spread, mapping = aes(x = longpl, y = pressure)) +
  facet_grid(lat_av ~ .)
plot2a <- plot1a + 
#  geom_vline(data = orig_lines, mapping = aes(xintercept = long), 
#             color = 'black', alpha = 0.1, size = 0.1) +
  geom_point(mapping = aes(colour = colf), pch = 15, size = 1.2) +
  scale_y_reverse() +
  scale_color_gradientn(colours = femmecol(200), na.value = 'transparent',
                        guide = guide_colorbar(title=expression(paste('N (', s^-1, ')     '))),
                        limits=c(min(pl.data$colf),max(pl.data$colf)), breaks=seq(-2.6,-1.6,0.4), 
                        label = c(expression(paste('0.25 ', 10^-2)), expression(paste('0.63 ', 10^-2)),
                                  expression(paste('1.6  ', 10^-2)))) +
  theme_bw()

plot2a


# ---------------------------------------------------------------------------------------
### add bathymetry to plots ---
#----------------------------------------------------------------------------------------
#add bathymetry
bathy <- read_csv('data_processed/BathyLong_latav.csv')
bathy$lat_av <- as.factor(round(bathy$lat_av,2))
# define sample day
bathy$sample_day <- ifelse(bathy$lat_av == 1, 'Day3', 
                           ifelse(bathy$lat_av==2, 'Day2', 
                                  ifelse(bathy$lat_av==3, 1, 'Day4')))
# change level -27.54 to -27.54 for lat_av (for consistency with levels from ohter data sets)
levels(bathy$lat_av) <- c(levels(bathy$lat_av), '-27.53')
bathy$lat_av[bathy$lat_av == '-27.54'] <- '-27.53'

# define average latitude per day to plot per latitude not per day
bathyA <- read_csv('data_processed//BathyLong.csv')
bathy_la <- bathyA %>%
  group_by(sample_day) %>%
  summarise(lat_av = factor(round(mean(lat),2), 
                            levels = c('-27.48', '-27.51', '-27.54', '-27.56'))) %>%
  inner_join(bathyA, by = 'sample_day')
# change level -27.54 to -27.54 for lat_av (for consistency with levels from ohter data sets)
levels(bathy_la$lat_av) <- c(levels(bathy_la$lat_av), '-27.53')
bathy_la$lat_av[bathy_la$lat_av == '-27.54'] <- '-27.53'
head(bathy_la)

bathy_l <- bathy

gB <- ggplot(data=bathy_l, aes(x=lon, y=bathy)) + geom_line(size=0.6) +
  labs(x="longitude", 
       y="bathymetry (m)") +
  facet_grid(lat_av ~ .) +
  theme_bw() +
  theme(legend.box.margin = margin(0,0,0,0,'mm'), plot.margin = margin(3,3,3,3,'mm'))
gB

# plot individual bathy points on top of lines to check whether interpolation was ok
gB + geom_point(data = bathy_la, aes(x = lon, y = bathy)) +
  facet_grid(lat_av ~ .)

# plto profiles with bathymentry
plot2aB <- plot2a + geom_line(data = bathy_l, aes(x = lon, y = abs(bathy)), size = 0.2, col = 'black') + 
  geom_point(data = bathy_la, aes(x = lon, y = abs(bathy)), col = 'black', size = 0.6) + 
  labs(x = 'longitude', y = 'depth (m)',
    title='N profiles for individual sample days from north (top) to south (bottom)',
       subtitle = 'profiles stretched out to avoid overlaying,
lines show bathymetry (GEBCO) with points at positions of profiles') +
  facet_grid(lat_av ~ .)

# export plot
pdf('figures/N-profiles_days_bathy.pdf', width = 7, height = 7/sqrt(2))
plot2aB
dev.off()
