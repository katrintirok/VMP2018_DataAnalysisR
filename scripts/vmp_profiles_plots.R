
library(tidyverse)
library(lubridate)
library(shape) # for colour femmecol=jet in matlab

# Try different ways to plot the data

# read data
dat1 <- read_csv('data_processed/VMP_gswVariables.csv') %>%
  select(c(profile_nr,Pressure_dbar, Temperature_degC, SP, rho))
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


# Plot temperature over longitude per day

plot2 <- ggplot(data = datj, mapping = aes(x = long, y = Pressure_dbar)) +
  geom_point(mapping = aes(colour = Temperature_degC), pch=0)
plot2 +
  facet_grid(sample_day ~ .) +
  scale_y_reverse() +
  scale_color_gradientn(colours = femmecol(200), na.value = "transparent")


# Now defining average Latitude per day to plot per latitude not per day
datj_lat <- datj %>%
  group_by(sample_day) %>%
  summarise(lat_av = factor(round(mean(lat),2), 
                            levels = c('-27.48', '-27.51', '-27.53', '-27.56'))) %>%
  inner_join(datj, by = 'sample_day')
head(datj_lat)

plot3 <- ggplot(data = datj_lat, mapping = aes(x = long, y = Pressure_dbar)) +
  geom_point(mapping = aes(colour = Temperature_degC), pch = 0, size = 0.8)
plot_ex <- plot3 +
  facet_grid(lat_av ~ .) +
  scale_y_reverse() +
  scale_color_gradientn(colours = femmecol(200), na.value = "transparent") +
  theme_bw()

ggsave('figures/Temp_profiles.pdf', plot_ex, width = 14, height=7, units = 'in')
ggsave('figures/Temp_profiles.png', plot_ex, width = 14, height=7, units = 'in', dpi=300)

