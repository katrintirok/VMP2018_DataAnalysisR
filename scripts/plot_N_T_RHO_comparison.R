# plot N calculated from rho, T_slow and T_fast in comparison

library(tidyverse)

# read data and sort by profile_nr and pressure
N_R <- read_csv('data_processed/VMP_N2_RHOslow_disslength.csv') %>%
  arrange(profile_nr,pressure)
N_Ts <- read_csv('data_processed/VMP_N2_Tslow_disslength.csv') %>%
  arrange(profile_nr,pressure)
N_Tf <- read_csv('data_processed/VMP_N2_Tfast_disslength.csv') %>%
  arrange(profile_nr,pressure)

# remove zeros from N_Ts$N2_s_T
#N_Ts$N2_s_T[N_Ts$N2_s_T==0] <- NaN


# if data are in correct order (arranged), I can use cbind to join the tables
pl.data <- N_R %>%
  select(-c(pressure, profile_nr)) %>%
  cbind(N_Ts) %>%
  select(-c(profile_nr, pressure)) %>%
  cbind(N_Tf) %>%
  gather(key = N2_var, -c(profile_nr, pressure, tsc_R, tsc_T, tsc_T1, tsc_T2), value = N2) %>%
  gather(key = tsc_var, -c(profile_nr, pressure, N2_var, N2), value = tsc)


p1 <- ggplot(data = pl.data) +
  geom_point(aes(x = pressure, y = log10(sqrt(N2)), col = N2_var), alpha = 0.2) +
  scale_colour_discrete(name="N source",
                      breaks=c("N2_s_R", "N2_s_T", "N2_s_T1", "N2_s_T2"),
                      labels=c(expression("N from "*rho*""['slow']), 
                               expression("N from T"['slow']), 
                               expression("N from T1"['fast']),
                               expression("N from T2"['fast']))) +
  title(ylab = 'N 1/s') +
  scale_x_continuous(trans = 'reverse') +
  coord_flip() +
  theme_bw()
p1
#ggsave(filename = 'plot_Nsource_comparison.png', p1, width = 7, height = 14)

#ggplot(data = N_Ts) +
#  geom_point(aes(x = pressure, y = (sqrt(N2_s_T))), alpha = 0.2) +
#  scale_x_continuous(trans = 'reverse') +
#  coord_flip() +
#  theme_bw()

# plot tsc
p2 <- ggplot(data = filter(pl.data, tsc_var %in% c('tsc_T','tsc_T1'))) +
  geom_point(aes(x = pressure, y = log10(tsc), col = tsc_var), alpha = 0.2) +
  scale_colour_discrete(name=expression("L"['T']*" source"),
                        breaks=c("tsc_R", "tsc_T", "tsc_T1", "tsc_T2"),
                        labels=c(expression("L"['T']*" from "*rho*""['slow']), 
                                 expression("L"['T']*" from T"['slow']), 
                                 expression("L"['T']*" from T1"['fast']),
                                 expression("L"['T']*" from T2"['fast']))) +
  scale_x_continuous(trans = 'reverse') +
  ylim(-4,1) +
  labs(y = expression("L"['T'])) +
  coord_flip() +
  theme_bw()
p2

#ggsave(filename = 'plot_Thorpe_source_comparison.png', p1, width = 7, height = 14)

# plot as density functions
#pl.data$N2[pl.data$N2 == 0] <- NaN
pd1 <- ggplot(data = pl.data, aes(x=log10(sqrt(N2)), col=N2_var)) + 
  geom_density() +
  scale_colour_discrete(name="N source",
                        breaks=c("N2_s_R", "N2_s_T", "N2_s_T1", "N2_s_T2"),
                        labels=c(expression(rho['slow']), 
                                 expression("T"['slow']), 
                                 expression("T1"['fast']),
                                 expression("T2"['fast']))) +
  labs(title = 'distributions of N values (log10) \ncalculated from slow data vs fast data', 
        x = 'log10(N)') +
  theme_bw()
pd1


pd2 <- ggplot(data = pl.data, aes(x=log10(tsc), col=tsc_var)) + 
  geom_density(alpha = 0.1) +
  scale_colour_discrete(name=expression("L"['T']*" source"),
                        breaks=c("tsc_R", "tsc_T", "tsc_T1", "tsc_T2"),
                        labels=c(expression(rho['slow']), 
                                 expression("T"['slow']), 
                                 expression("T1"['fast']),
                                 expression("T2"['fast']))) +
  labs(title = 'distributions of Thorpe scale (log10) \ncalculated from slow data vs fast data', 
       x = expression("log10(L"['T']*')')) +
  theme_bw()
pd2

library(gridExtra)
pexp <- grid.arrange(pd1, pd2, nrow = 1, ncol = 2)
ggsave('figures/DistributionsNLT_source_comparison.pdf',pexp, width=10, height=5)
