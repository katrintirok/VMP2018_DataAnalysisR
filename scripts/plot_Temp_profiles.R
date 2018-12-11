# plot epsilon in the ways


library(tidyverse)
library(lubridate)
library(shape) # for colour femmecol=jet in matlab
library(gridExtra)

# read data
dat1 <- read_csv('data_processed/VMP_gswVariables.csv') %>%
  rename(T = Temperature_degC, C = Conductivity_mS_per_cm, pressure = Pressure_dbar)
head(dat1)

# Join dat1 table with gps data
# read gps info
dat_gps1 <- read_csv('data_processed/VMP_profiles_info_GPS_clean.csv') %>%
  mutate(sample_day = factor(day(datetime_start_YMDHMS_UTC), levels=c(29,30,31,1), labels=c('Day1','Day2','Day3','Day4'))) %>%
  select(c('profile_nr', 'sample_day', 'lat', 'long'))
head(dat_gps1)

# join dat1 and dat_gps1
datj <- inner_join(dat1, dat_gps1, by = 'profile_nr')
head(datj)

# Now defining average Latitude per day to plot per latitude not per day
datj_lat <- datj %>%
  group_by(sample_day) %>%
  summarise(lat_av = factor(round(mean(lat),2), 
                            levels = c('-27.48', '-27.51', '-27.53', '-27.56'))) %>%
  inner_join(datj, by = 'sample_day')
head(datj_lat)

# Plot Temp over longitude per day
plot1 <- ggplot(data = datj_lat, mapping = aes(x = long, y = pressure))
plot2 <- plot1 + 
  geom_point(mapping = aes(colour = T), pch = 15, size = 0.5) +
  facet_grid(lat_av ~ .) +
  scale_y_reverse() +
  scale_color_gradientn(colours = femmecol(200), na.value = 'transparent') +
  theme_bw()

plot2

# arrange plots for export
#grid.arrange(plot_eps1, plot_eps2, plot_epsa, ncol = 2) #, widths = c(4, 4))
# ggsave('figures/Eps_profiles.pdf', plot2, width = 10, height=5, units = 'in')


#---------------------------------------------
# plot with spread between profiles
# ---------------------------------------------------
long1 <- datj_lat %>%
  group_by(sample_day, long) %>%
  summarise(mT = mean(T)) %>%
  mutate(longpl = long) %>%
  mutate(diff = c(NA,diff(longpl, lag = 1)))

mindiff <- 0.002
while(sum(round(long1$diff,3)<mindiff, na.rm = T) != 0){
  for(i in 1:length(long1$diff)){
    if(!is.na(long1$diff[i]) & long1$diff[i] < mindiff){
      long1$longpl[i] <- long1$longpl[i] + mindiff - long1$diff[i]
    }
  }
  long1 <- long1 %>%
    mutate(diff = c(NA,diff(longpl, lag = 1)))
}


# now join long1 again with datj_lat using sample-day and long as keys
datj_lat2 <- inner_join(datj_lat, long1, by=c('sample_day','long')) %>%
  select(-diff)
head(datj_lat2)

# define values for hlines to show original positions of profiles
orig_lines <- datj_lat2 %>%
  group_by(lat_av, eps, long) %>%
  summarise(eps_av= mean(eps_val)) %>%
  select(lat_av, eps, long)
head(orig_lines)

plot1a <- ggplot(data = datj_lat2, mapping = aes(x = longpl, y = pressure)) +
  facet_grid(lat_av ~ eps)
plot2a <- plot1a + 
  geom_vline(data = orig_lines, mapping = aes(xintercept = long), 
             color = 'black', alpha = 0.1, size = 0.1) +
  geom_point(mapping = aes(colour = log10(eps_val)), pch = 15, size = 0.8) +
  scale_y_reverse() +
  scale_color_gradientn(colours = femmecol(200), na.value = 'transparent') +
  theme_bw()

plot2a

#ggsave('figures/Eps_profiles_spread.pdf', plot2a, width = 14, height=7, units = 'in')


## --- plot eps_av only
# less spread of profiles!
long1 <- datj_lat %>%
  group_by(sample_day, long) %>%
  summarise(meps = mean(eps_val)) %>%
  mutate(longpl = long) %>%
  mutate(diff = c(NA,diff(longpl, lag = 1)))

mindiff <- 0.002
while(sum(round(long1$diff,3)<mindiff, na.rm = T) != 0){
  for(i in 1:length(long1$diff)){
    if(!is.na(long1$diff[i]) & long1$diff[i] < mindiff){
      long1$longpl[i] <- long1$longpl[i] + mindiff - long1$diff[i]
    }
  }
  long1 <- long1 %>%
    mutate(diff = c(NA,diff(longpl, lag = 1)))
}

# now join long1 again with datj_lat using sample-day and long as keys
datj_lat2 <- inner_join(datj_lat, long1, by=c('sample_day','long')) %>%
  select(-diff)
head(datj_lat2)

# define values for hlines to show original positions of profiles
orig_lines <- datj_lat2 %>%
  group_by(lat_av, eps, long) %>%
  summarise(eps_av= mean(eps_val)) %>%
  select(lat_av, eps, long)
head(orig_lines)

# plot
plot1_av <- ggplot(data = filter(datj_lat2, eps %in% 'eps_av'), 
                   mapping = aes(x = longpl, y = pressure)) +
  facet_grid(lat_av ~ eps)
plot2_av <- plot1_av + 
  geom_vline(data = filter(orig_lines, eps %in% 'eps_av'), 
             mapping = aes(xintercept = long), 
             color = 'black', alpha = 0.1) +
  geom_point(mapping = aes(colour = log10(eps_val)), pch = 15, size = 2) +
  scale_y_reverse() +
  scale_color_gradientn(colours = femmecol(200), na.value = 'transparent') +
  theme_bw()

plot2_av



##--- plot eps1 and eps2 only
# plot eps1
plot1_eps1 <- ggplot(data = filter(datj_lat2, eps %in% 'eps1'), 
                   mapping = aes(x = longpl, y = pressure)) +
  facet_grid(lat_av ~ .)
plot2_eps1 <- plot1_eps1 + 
  #geom_vline(data = filter(orig_lines, eps %in% 'eps1'), 
  #           mapping = aes(xintercept = long), 
  #           color = 'black', alpha = 0.1) +
  geom_point(mapping = aes(colour = log10(eps_val)), pch = 15, size = 1.2) +
  scale_y_reverse() +
  scale_color_gradientn(colours = femmecol(200), na.value = 'transparent',
                        guide=guide_colorbar(
                          title=expression(paste(epsilon, '(', m^2, '/', s^3, ')'))),
                        #limits=c(min(datj_lat2$eps_val),max(datj_lat2$eps_val)), 
                        breaks=seq(-9,-6,1), 
                        label = c(expression(10^-9), expression(10^-8), 
                                  expression(10^-7), expression(10^-6))) +
  theme_bw()

plot2_eps1

# plot eps2
plot1_eps2 <- ggplot(data = filter(datj_lat2, eps %in% 'eps2'), 
                     mapping = aes(x = longpl, y = pressure)) +
  facet_grid(lat_av ~ .)
plot2_eps2 <- plot1_eps2 + 
  #geom_vline(data = filter(orig_lines, eps %in% 'eps1'), 
  #           mapping = aes(xintercept = long), 
  #           color = 'black', alpha = 0.1) +
  geom_point(mapping = aes(colour = log10(eps_val)), pch = 15, size = 1.2) +
  scale_y_reverse() +
  scale_color_gradientn(colours = femmecol(200), na.value = 'transparent',
                        guide=guide_colorbar(
                          title=expression(paste(epsilon, '(', m^2, '/', s^3, ')'))),
                        #limits=c(min(datj_lat2$eps_val),max(datj_lat2$eps_val)), 
                        breaks=seq(-9,-6,1), 
                        label = c(expression(10^-9), expression(10^-8), 
                                  expression(10^-7), expression(10^-6))) +
  theme_bw()

plot2_eps2

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
plot2_eps2_B <- plot2_eps2 + geom_line(data = bathy_l, aes(x = lon, y = abs(bathy)), size = 0.2, col = 'black') + 
  geom_point(data = bathy_la, aes(x = lon, y = abs(bathy)), col = 'black', size = 0.6) + 
  
  labs(x = 'longitude', y = 'depth (m)',
       title='Epsilon2 profiles for individual sample days from north (top) to south (bottom)',
       subtitle = 'profiles stretched out to avoid overlaying,
lines show bathymetry (GEBCO) with points at positions of profiles') +
  facet_grid(lat_av ~ .)

# export plot
pdf('figures/Epsilon2-profiles_days_bathy.pdf', width = 7, height = 7/sqrt(2))
plot2_eps2_B
dev.off()
