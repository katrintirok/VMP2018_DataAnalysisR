# plot Length scales against each other ...


library(tidyverse)

dat1 <- read_csv('data_processed/LengthScales_dissipationLength.csv')
dat1$LE_T1[is.infinite(dat1$LE_T1)] <- NaN
dat1$LE_T2[is.infinite(dat1$LE_T2)] <- NaN

pOT <- ggplot(data = dat1) + 
  geom_point(aes(x = log10(LT_T1), log10(LO_eps1)), alpha = 0.2, size = 0.7) +
  geom_point(aes(x = log10(LT_T2), log10(LO_eps2)), alpha = 0.1, size = 0.7, col = 'red') +
  geom_abline(slope = 1, intercept = 0, size = 0.5, col = 'gray') +
  scale_y_continuous(limits = c(-4.1, 1.5), expand = c(0.01,0)) +
  scale_x_continuous(limits = c(-4.1, 1.5), expand = c(0.01,0)) +
  labs(title = 'Ozmidov scale versus Thorpe scale',
       x = expression(paste('log'[10], '(L'['T'], ') [m]')),
       y = expression(paste('log'[10], '(L'['O'], ') [m]'))) +
  theme_bw()
pOT
ggsave('figures/plot_LovsLT.pdf', pOT, width = 5, height = 5)

pET <- ggplot(data = dat1) + 
  geom_point(aes(y = log10(LT_T1), x = log10(LE_T1)), alpha = 0.2, size = 0.7) +
  geom_point(aes(y = log10(LT_T2), x = log10(LE_T2)), alpha = 0.1, size = 0.7, col = 'red') +
  geom_abline(slope = 1, intercept = 0, size = 0.5, col = 'gray') +
  scale_y_continuous(limits = c(-5, 2), expand = c(0.01,0)) +
  scale_x_continuous(limits = c(-5, 2), expand = c(0.01,0)) +
  labs(title = 'Ellison scale versus Thorpe scale',
       x = expression(paste('log'[10], '(L'['T'], ') [m]')),
       y = expression(paste('log'[10], '(L'['E'], ') [m]'))) +
  theme_bw()
pET
ggsave('figures/plot_LEvsLT.pdf', pET, width = 5, height = 5)

pOE <- ggplot(data = dat1) + 
  geom_point(aes(x = log10(LE_T1), log10(LO_eps1)), alpha = 0.2, size = 0.7) +
  geom_point(aes(x = log10(LE_T2), log10(LO_eps2)), alpha = 0.1, size = 0.7, col = 'red') +
  geom_abline(slope = 1, intercept = 0, size = 0.5, col = 'gray') +
  scale_y_continuous(limits = c(-5, 2), expand = c(0.01,0)) +
  scale_x_continuous(limits = c(-5, 2), expand = c(0.01,0)) +
  labs(title = 'Ozmidov scale versus Ellison scale',
       x = expression(paste('log'[10], '(L'['E'], ') [m]')),
       y = expression(paste('log'[10], '(L'['O'], ') [m]'))) +
  theme_bw()
pOE
ggsave('figures/plot_LOvsLE.pdf', pOE, width = 5, height = 5)


# standardise using kolmogorov scale
pOTs <- ggplot(data = dat1) + 
  geom_point(aes(x = log10(LT_T1/LK_eps1), log10(LO_eps1/LK_eps1)), alpha = 0.2, size = 0.7) +
  labs(title = 'Ozmidov scale versus Thorpe scale') +
  theme_bw()
pOTs

pKT <- ggplot(data = dat1) + 
  geom_point(aes(x = log10(LT_T1), (LK_eps1)), alpha = 0.2, size = 0.7) +
  labs(title = 'Kolmogorov scale versus Thorpe scale') +
  theme_bw()
pKT

pKO <- ggplot(data = dat1) + 
  geom_point(aes(x = log10(LO_eps1), log10(LK_eps1)), alpha = 0.2) +
  labs(title = 'Kolmogorov scale versus Ozmidov scale') +
  theme_bw()
pKO

# LT:LO over depth
pLTp <- ggplot(data = dat1) + 
  geom_point(aes(x = log10(LT_T1/LO_eps1), y=pressure), alpha = 0.2) +
  labs(title = 'Ratio of Thorpe to Ozmidov scale over depth') +
  scale_y_reverse() +
  theme_bw()
pLTp

pLTLEp <- ggplot(data = dat1) +
  geom_point(aes(x = log10(LT_T1/LE_T1), y = pressure), alpha = 0.2) +
  labs(title = 'Ratio of Thorpe to Ellison scale over depth') +
  scale_y_reverse() +
  theme_bw()
pLTLEp
