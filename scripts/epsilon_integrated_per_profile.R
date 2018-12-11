# epsilon integrated over depth/profile

library(tidyverse)
library(lubridate)

# read data
eps <- read_csv('data_processed/VMP_table.csv') %>%
  rename(profile_nr = prof_num) %>%
  select(eps1, eps2, profile_nr, pressure) %>%
  gather(key = 'eps_var', -profile_nr, -pressure, value = 'eps')

ggplot(eps) +
  geom_density(aes(log10(eps), col = eps_var)) +
  theme_bw()

# do the density per depth bins of 50 m
eps$dbins <- cut(eps$pressure, breaks = c(seq(0,50,10), seq(100,500,100))) #, labels = c(seq(10,50,10), seq(100,500,100)))

# density per depth bin
ggplot(eps) +
  geom_density(aes(log10(eps), col = eps_var)) +
  facet_wrap(~dbins, ncol = 5) +
  theme_bw()

ggplot(filter(eps, pressure <= 50)) + 
  geom_point(aes(log10(eps), pressure)) +
  scale_y_reverse()

# integrate over depth by profile
eps_int <- eps %>%
  group_by(profile_nr, eps_var) %>%
  summarise(eps_sum = sum(eps),
            eps_mean = mean(eps),
            eps_int = sum(eps)/max(pressure),
            #eps_geom = geomean(eps),
            eps_median = median(eps),
            press_max = max(pressure)) %>%
  gather(key = int_var, -profile_nr, -press_max, -eps_var, value = eps_int)

eps_int_20 <- eps %>%
  group_by(profile_nr, eps_var) %>%
  filter(pressure > 10) %>%
  summarise(eps_mean = mean(eps),
            press_max = max(pressure))

eps_int_dbins <- eps %>%
  group_by(profile_nr, eps_var, dbins) %>%
  summarise(eps_mean = mean(eps),
            eps_sum = sum(eps)) %>%
  gather(key = int_var, -profile_nr, -dbins, -eps_var, value = eps_int)

ggplot(eps_int) +
  geom_line(aes(x = profile_nr, y = eps_int, col = int_var)) +
  scale_y_log10()

# add with gps data
gps <- read_csv('data_processed/VMP_profiles_info_GPS_clean.csv')
eps_gps <- eps_int_dbins %>%
  merge(gps, by='profile_nr') %>%
  mutate(day = day(datetime_start_YMDHMS_UTC))

ggplot(eps_gps) +
  geom_point(aes(x = long, y = log10(eps1), col = as.factor(day))) +
  facet_wrap(~eps1_var, ncol = 1)

test1 <- ggplot(filter(eps_gps, int_var == 'eps_mean' )) +
  geom_point(aes(x = long, y = log10(eps_int), col = as.factor(day))) +
  theme_bw()
test1

#ggsave('figures/eps_integrated_vsLong.pdf', test1)

# plot dbins integrates
ggplot(filter(eps_gps, int_var == 'eps_sum')) +
  geom_line(aes(x = depth_m_echosound, y = log10(eps_int), col = dbins), alpha = 0.5) #+
#  facet_wrap(~int_var, ncol = 1)

ggplot(eps_gps) +
  geom_point(aes(x = press_max, y = log10(eps_mean), col = as.factor(day))) +
  theme_bw()

test2 <- ggplot(filter(eps_gps, int_var == 'eps_int')) +
  geom_point(aes(x = depth_m_echosound, y = log10(eps_int), col = as.factor(day)), alpha = 0.8) +
  theme_bw() +
  labs(title = 'eps integrated per profile vs depth of profile site')
  #labs(title = 'mean value per profile')
test2

test3 <- ggplot(filter(eps_gps, int_var == 'eps_sum')) +
  geom_point(aes(x = depth_m_echosound, y = log10(eps_int), col = as.factor(day)), alpha = 0.8) +
  theme_bw() +
  labs(title = 'sum per profile')
test3

ggsave('figures/eps_integrated_vsDepth.pdf', test2)

library(gridExtra)

grid.arrange(test1, test2)
# calculate distance from shore
# plot over bathymetry