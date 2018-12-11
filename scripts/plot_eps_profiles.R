# plot epsilon in the ways


library(tidyverse)
library(lubridate)
library(shape) # for colour femmecol=jet in matlab
library(gridExtra)

# read data
dat1 <- read_csv('data_processed/VMP_table.csv') %>%
  select(c('pressure','eps1','eps2','prof_num')) %>%
  rename(profile_nr = prof_num) %>%
  mutate(eps_av = (eps1 + eps2)/2)
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
  inner_join(datj, by = 'sample_day') %>%
  gather(key = eps, value = eps_val, eps1, eps2, eps_av) %>%
  mutate(eps = factor(eps, levels = c('eps1','eps2','eps_av')))
head(datj_lat)


# Plot epsilon over longitude per day
plot1 <- ggplot(data = datj_lat, mapping = aes(x = long, y = pressure))
# plote1 <- plot1 + 
#   geom_point(mapping = aes(colour = log10(eps1)), pch = 15, size = 0.5) +
#   labs(title = 'eps1')
# plote2 <- plot1 + 
#   geom_point(mapping = aes(colour = log10(eps2)), pch = 15, size = 0.5) +
#   labs(title = 'eps2')
# plotea <- plot1 + 
#   geom_point(mapping = aes(colour = log10(eps_av)), pch = 15, size = 0.5) +
#   labs(title = 'average eps')

# 
# plot1 +
#   facet_grid(sample_day ~ .) +
#   scale_y_reverse() +
#   scale_color_gradientn(colours = femmecol(200), na.value = "transparent")


# plot_eps1 <- plote1 +
#   facet_grid(lat_av ~ .) +
#   scale_y_reverse() +
#   scale_color_gradientn(colours = femmecol(200), na.value = "transparent") +
#   theme_bw() 
# plot_eps2 <- plote2 +
#   facet_grid(lat_av ~ .) +
#   scale_y_reverse() +
#   scale_color_gradientn(colours = femmecol(200), na.value = "transparent") +
#   theme_bw() 
# plot_epsa <- plotea +
#   facet_grid(lat_av ~ .) +
#   scale_y_reverse() +
#   scale_color_gradientn(colours = femmecol(200), na.value = "transparent") +
#   theme_bw() 

# plot with facet lat_av and eps
plot2 <- plot1 + 
  geom_point(mapping = aes(colour = log10(eps_val)), pch = 15, size = 0.5) +
  facet_grid(lat_av ~ eps) +
  scale_y_reverse() +
  scale_color_gradientn(colours = femmecol(200), na.value = 'transparent') +
  theme_bw()

plot2

# arrange plots for export
#grid.arrange(plot_eps1, plot_eps2, plot_epsa, ncol = 2) #, widths = c(4, 4))
ggsave('figures/Eps_profiles.pdf', plot2, width = 10, height=5, units = 'in')


#---------------------------------------------
# plot with sperad between profiles
# ---------------------------------------------------
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

ggsave('figures/Eps_profiles_spread.pdf', plot2a, width = 14, height=7, units = 'in')


##--- plot eps1 and eps2 only

## --- plot eps_av only
# less spread of profiles!
long1 <- datj_lat %>%
  group_by(sample_day, long) %>%
  summarise(meps = mean(eps_val)) %>%
  mutate(longpl = long) %>%
  mutate(diff = c(NA,diff(longpl, lag = 1)))

mindiff <- 0.0015
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
