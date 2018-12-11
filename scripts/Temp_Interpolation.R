# interpolate temperature from slow data

library(tidyverse)
library(lubridate)
library(shape)
library(directlabels)


# temp data
temp <- read_csv('data_processed/VMP_profiles_slowData_cleaned.csv')
# gps data
gps <- read_csv('data_processed/VMP_profiles_info_GPS_clean.csv') %>%
  mutate(sample_day = factor(day(datetime_start_YMDHMS_UTC), levels=c(29,30,31,1), labels=c('Day1','Day2','Day3','Day4'))) %>%
  select(c('profile_nr', 'sample_day', 'lat', 'long'))

# join temp with gps
tempgps <- temp %>% 
  inner_join(gps, by='profile_nr') %>%
  group_by(sample_day, profile_nr, long, Pressure_dbar) %>%
  summarise(Tav = mean(Temperature_degC))

# need to bin pressure values! --> steps of 0.1 m? or better 0.01?
# round pressure to 2 digits after comma and then average
tempav <- tempgps %>%
  mutate(Pbin = round(Pressure_dbar, 1)) %>%
  group_by(sample_day, profile_nr, long, Pbin) %>%
  summarise(Tbin = mean(Tav))

# couple profiles have same longitude --> add 0.00005
tempav_diff <- tempav %>%
  group_by(sample_day, profile_nr, long) %>%
  summarise(mT = mean(Tbin)) %>%
  mutate(longpl = long)
tempav_diff$longdiff <- c(NA,diff(tempav_diff$longpl, lag = 1))
tempav_diff$longpl <- ifelse(tempav_diff$longdiff == 0, tempav_diff$long+0.00005, tempav_diff$long)
tempav_diff$longpl[1] <- tempav_diff$long[1]

# now join tempav again with tempav_diff using sample-day and long as keys
tempav_spread <- inner_join(tempav, tempav_diff, by=c('sample_day','profile_nr','long')) %>%
  select(-longdiff, -mT)
head(tempav_spread)

# interpolate temperature
data_interp <- data.frame()
samples <- data.frame(sample_day = c('Day1','Day2','Day3','Day4'), lat_av = c(-27.53, -27.51, -27.48, -27.56))
for(d in 1:length(samples$sample_day)){
  # test with sample_day = 1
  day1 <- tempav_spread %>%
    filter(sample_day == samples$sample_day[d]) %>%
    ungroup() %>%
    select(Pbin, Tbin, longpl)
  
  # make wide dataset with longitude as columns and pressure as rows
  # loop through each preassure bin and interpolate for longitudes
  day1_wide <-  spread(day1, key=longpl, value=Tbin)
  long_data <- as.numeric(colnames(day1_wide[-c(1)]))
  long_interp <- seq(min(long_data),max(long_data),length.out = 100)
  
  xinterp <- c()
  for(i in 1:length(day1_wide$Pbin)){
    xdat <- as.numeric(day1_wide[i,-c(1)])
    if(sum(is.na(xdat)) < length(long_data)-1){
      xinterp_i <- approx(x=long_data,xout=long_interp,y=xdat,method='linear')
      xinterp <- rbind(xinterp,xinterp_i$y)
    } else {
      xinterp_i <- rep(NaN, length(long_interp))
      xinterp <- rbind(xinterp,xinterp_i)
    }
  }
  
  xinterpdfw <- as.data.frame(xinterp)
  colnames(xinterpdfw) <- as.character(long_interp)
  
  xinterpdf <- xinterpdfw %>%
    mutate(pressure_bin = day1_wide$Pbin) %>%
    gather(key=long_interp, value=Temp_interp, -pressure_bin) %>%
    mutate(long_interp = as.numeric(long_interp))
  xinterpdf$sample_day <- samples$sample_day[d]
  xinterpdf$lat_av <- samples$lat_av[d]
  
  data_interp <- rbind(data_interp,xinterpdf)
}

#write_csv(data_interp, 'data_processed/Temp_interp.csv')

# -------------------------------------------------------------------------------------------
#___ plot data ---
#--------------------------------------------------------------------------------------------
pl_data <- read_csv('data_processed/Temp_interp.csv')
pl_data$lat_av <- as.factor(pl_data$lat_av) #,  
# sort levels correct way around
levels(pl_data$lat_av) = c('-27.48', '-27.51', '-27.54', '-27.56')

pT <- ggplot(data = pl_data, aes(x = long_interp, y = pressure_bin)) +
  geom_raster(aes(fill = Temp_interp), interpolate = FALSE) +
  scale_fill_gradientn(colours = femmecol(46), na.value = 'transparent',
                       limits = c(9.6, 27.6), breaks=seq(5,30,5)) +
  scale_y_reverse() + 
  facet_grid(. ~ lat_av) +
  theme_bw()
pT

pTc <- ggplot(data = pl_data, aes(x = long_interp, y = pressure_bin, z = Temp_interp)) +
  geom_contour(aes(colour = ..level..), breaks=seq(8,28,0.2), col='black') + #bins = 20) +
  scale_y_continuous(limits=c(50,0), trans='reverse') +
  scale_colour_continuous(low = "black", high = "black") +
  facet_grid(lat_av ~ .) +
  theme_bw()
pTc
direct.label(pTc, method="bottom.pieces")


# make a plot with the bathymetry and locations of measurements
# read bathy data
bathy <- read_csv('data_processed/BathyCombLong_latav.csv')
head(bathy)

bathy$lat_av <- factor(round(bathy$lat_av,2)) 
levels(bathy$lat_av) <- c(levels(bathy$lat_av), '-27.53')
bathy$lat_av[bathy$lat_av == '-27.54'] <- '-27.53'
bathy$bathy <- ifelse(is.na(bathy$fine_bathy), bathy$gebco_bathy, bathy$fine_bathy)
# reset factor to drop unused level
bathy$lat_av <- as.numeric(as.character(bathy$lat_av))
bathy$lat_av <- as.factor(bathy$lat_av)
# sort levelsS
levels(bathy$lat_av) = c('-27.48', '-27.51', '-27.54', '-27.56')
# add sample_day variable
bathy$sample_day <- ifelse(bathy$lat_av == '-27.56', 'Day3', 
                           ifelse(bathy$lat_av=='-27.54', 'Day2', 
                                  ifelse(bathy$lat_av=='-27.51', 'Day1', 'Day4')))

vmp_gps <- read_csv('data_processed/VMP_profiles_info_GPS_clean.csv')
bathyA <- read_csv('data_processed//BathyLong.csv')
bathy_la <- bathyA %>%
  group_by(sample_day) %>%
  summarise(lat_av = factor(round(mean(lat),2), 
                            levels = c('-27.48', '-27.51', '-27.54', '-27.56'))) %>%
  inner_join(bathyA, by = 'sample_day')
# change level -27.54 to -27.54 for lat_av (for consistency with levels from ohter data sets)
levels(bathy_la$lat_av) <- c(levels(bathy_la$lat_av), '-27.53')
bathy_la$lat_av[bathy_la$lat_av == '-27.54'] <- '-27.53'
# reset factor to drop unused level
bathy_la$lat_av <- as.numeric(as.character(bathy_la$lat_av))
bathy_la$lat_av <- as.factor(bathy_la$lat_av)
# sort levelsS
levels(bathy_la$lat_av) = c('-27.48', '-27.51', '-27.54', '-27.56')

bathy_points <- bathy_la %>%
  inner_join(vmp_gps, by='profile_nr') %>%
  select(sample_day, profile_nr, lat_av, lon, bathy, depth_m_bathymetry.x, depth_m_echosound)



gB <- ggplot(data=bathy, aes(x=lon, y=bathy)) + geom_line(size=0.6) +
  labs(x="longitude", 
       y="bathymetry (m)") +
  facet_grid(lat_av ~ .) +
  theme_bw() +
  theme(legend.box.margin = margin(0,0,0,0,'mm'), plot.margin = margin(3,3,3,3,'mm'))
gB

# joined plot
# plto profiles with bathymentry
pBT <- pT + 
  # contour for temp
  geom_contour(aes(z = Temp_interp), bins = 45, breaks=seq(9.6,27.6,0.4), col='black', alpha = 0.5) +
  # fine bathy
  geom_line(data = bathy, aes(x = lon, y = abs(bathy)), size = 0.2, col = 'black') + 
  # depth from profiles
  geom_point(data = bathy_points, aes(x = lon, y = depth_m_bathymetry.x), col = 'black', size = 0.6) + 
  labs(x = 'longitude', y = 'depth (m)',
       title='interpolated Temperature for individual sample days from north (top) to south (bottom)')
pBT



pBTc <- pTc +
  # fine bathy
  geom_line(data = bathy, aes(x = lon, y = abs(bathy)), size = 0.2, col = 'black') + 
  # depth from profiles
  geom_point(data = bathy_points, aes(x = lon, y = depth_m_bathymetry.x), col = 'black', size = 0.6) + 
  labs(x = 'longitude', y = 'depth (m)',
       title='interpolated Temperature for individual sample days from north (top) to south (bottom)')
pBTc



# --- individual plots per day ---
sday <- 'Day4'
lat <- pl_data[pl_data$sample_day == sday,]$lat_av[1]
pT1 <- ggplot(data = filter(pl_data, sample_day==sday), aes(x = long_interp, y = pressure_bin)) +
  geom_raster(aes(fill = Temp_interp), interpolate = TRUE) +
  scale_fill_gradientn(colours = femmecol(100), na.value = 'transparent',
                       limits = c(9.6, 27.6), breaks=seq(5,30,5), name='T') +
  # contour for temp
  geom_contour(aes(z = Temp_interp), breaks=seq(9.6,27.6,0.4), col='black', alpha = 0.5) +
  # fine bathy
  geom_line(data = filter(bathy, sample_day==sday), aes(x = lon, y = abs(bathy)), size = 0.2, col = 'black') + 
  # depth from profiles
  geom_point(data = filter(bathy_points, sample_day==sday), aes(x = lon, y = depth_m_bathymetry.x), col = 'black', size = 0.6) + 
  labs(x = 'longitude', y = 'depth (m)',
       title=paste('interpolated temperature for', sday, 'at lat=', lat)) + 
  scale_y_reverse() + 
  theme_bw()
pT1

#fname <- paste('figures/TempInterpolated', sday, '.pdf', sep='')
#ggsave(fname, pT1, width=7, height=7, units=c('in'))



