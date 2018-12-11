# ---------------------------------------------------------------------------
# --- plot N2 values as profiles over longitude per latitude/sample day ---
# ---------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(shape)

# join N2 table and gps table
N2 <- read_csv('data_processed/VMP_N2_Tfast_disslength.csv')
gps <- read_csv('data_processed/VMP_profiles_info_GPS_clean.csv') %>%
  mutate(sample_day = factor(day(datetime_start_YMDHMS_UTC), levels=c(29,30,31,1), labels=c('Day1','Day2','Day3','Day4'))) %>%
  select(c('profile_nr', 'sample_day', 'lat', 'long'))
vmp_N2_gps <- inner_join(N2, gps, by = 'profile_nr')
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
plot_Nall <- ggplot(data = pl.data, mapping = aes(x = log10(sqrt(N2_s_T1)), y = pressure, col = long)) +
  geom_point(alpha = 0.3) +
  scale_y_reverse(limits=c(500,0)) +
  scale_x_continuous(breaks = seq(-2.5, 0, by = 0.5), limits = c(-2.5, 0),
                     label = c('', expression(10^-2), '', expression(10^-1), '', expression(10^0))) + 
  labs(x = 'N (1/s)',
       y = 'depth (m)',
       title = 'all N profiles, N from fast T1 averaged over same depths as eps',
       subtitle = 'shades of blue represent longitude') +
  theme_bw()
plot_Nall
# write plot to file
#pdf('figures/N_all-profiles_depth-averaged.pdf', width=7/sqrt(2),height=7)
#plot_Nall
#dev.off()

# -------------------------------------------------------------------------------------
# --- plotting 'colour' profiles for N (sqrt of N2m_s --> N^2 manual from sorted profiles of temperature)
# -------------------------------------------------------------------------------------
# define colours for plotting N
pl.data$lg.N_s_T1 <- log10(sqrt(pl.data$N2_s_T1))   # convert N^2 to N and take log10
topW <- quantile(pl.data$lg.N_s_T1, probs=c(0.75)) + IQR(pl.data$lg.N_s_T1)
botW <- quantile(pl.data$lg.N_s_T1, probs=c(0.25)) - IQR(pl.data$lg.N_s_T1)
top <- ifelse(topW>max(pl.data$lg.N_s_T1), max(pl.data$lg.N_s_T1), topW)   #-1.682539  for kriged
bot <- ifelse(botW<min(pl.data$lg.N_s_T1), min(pl.data$lg.N_s_T1), botW)   #-2.509479 for kriged
#top <- for N from fast T -0.449   for N from rho -1.683
#bot <- for N from fast T -1.298   for N from rho -2.509

#make factor variable for colours
var <- pl.data$lg.N_s_T1
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
                        limits=c(min(pl.data$colf),max(pl.data$colf)), breaks=seq(-1.2,-0.6,0.2), 
                        label = c('0.63', '0.10','0.16', '0.25')) +
  theme_bw()

plot2


#---------------------------------------------
# plot lg_N with spread between profiles
# ---------------------------------------------------
pl.data_diff <- pl.data %>%
  group_by(sample_day, long) %>%
  summarise(mN2 = mean(N2_s_T1)) %>%
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
  select(lat_av, lg.N_s_T1, long)
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
                        limits=c(min(pl.data$colf),max(pl.data$colf)), breaks=seq(-1.2,-0.6,0.2), 
                        label = c('0.63', '0.10','0.16', '0.25')) +
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

# use combined bathy
comb_bathy <- read_csv('data_processed/BathyCombLong_latav.csv')
# change level -27.54 to -27.54 for lat_av (for consistency with levels from ohter data sets)
comb_bathy$lat_av <- factor(round(comb_bathy$lat_av,2)) 
levels(comb_bathy$lat_av) <- c(levels(comb_bathy$lat_av), '-27.53')
comb_bathy$lat_av[comb_bathy$lat_av == '-27.54'] <- '-27.53'
bathy_l <- comb_bathy

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
plot2aB <- plot2a + 
  # gebco bathy
  geom_line(data = bathy_l, aes(x = lon, y = abs(gebco_bathy)), size = 0.2, col = 'black') + 
  # fine bathy
  # depth from profiles
  geom_line(data = bathy_l, aes(x=lon, y = abs(fine_bathy)), size=0.2, col='gray') +
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