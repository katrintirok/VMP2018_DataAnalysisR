
library(tidyverse)

dat1 <- read_csv('data_processed/VMP_gswVariables.csv')

# calculate linear model between PT and rho_pot
lmTrho <- summary(with(dat1, lm(PT ~ rho_pot)))

# plot potential temperature (PT) vs potential density (rho_pot)

p1 <- ggplot(data = dat1, aes(x = PT, y = rho_pot)) + 
  geom_point(alpha = 0.2) +
  geom_smooth(method='lm') +
  annotate('text', label = paste('rho_pot = ', round(lmTrho$coefficients[2],2), 
                          ' * T_pot + ', round(lmTrho$coefficients[1]), ', Rsq = ', round(lmTrho$r.squared,2), sep=''),
           x = 15, y = 1023) +
  labs(title = 'Relationship between potential temperature and potential density') +
  theme_bw()
p1Vie

ggsave('figures/plot_Temp_Density_Relation.png', p1, width =7, height=7, dpi = 150)


p1 <- ggplot(data = dat1, aes(x = rho, y = rho_pot)) + 
  geom_point(alpha = 0.2) +
 # geom_smooth(method='lm') +
 #  annotate('text', label = paste('rho_pot = ', round(lmTrho$coefficients[2],2), 
 #                                 ' * T_pot + ', round(lmTrho$coefficients[1]), ', Rsq = ', round(lmTrho$r.squared,2), sep=''),
 #           x = 15, y = 1023) +
  labs(title = 'Relationship between potential temperature and potential density') +
  theme_bw()
p1
ggsave('figures/plot_RHOpotVsRHO.png',p1,width=6,height = 6,dpi=150)
