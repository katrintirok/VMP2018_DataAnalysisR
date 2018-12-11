# plots of gsw vars ...

library(tidyverse)

data <- read_csv('data_processed/VMP_gswVariables.csv')

ggplot(data) +
  geom_line(aes(x = Pressure_dbar, y = Temperature_degC, group = profile_nr)) +
  coord_flip() +
  scale_x_reverse()

ggplot(data) +
  geom_line(aes(x = Pressure_dbar, y = SP, group = profile_nr)) +
  coord_flip() +
  scale_x_reverse()

ggplot(data) +
  geom_line(aes(x = Pressure_dbar, y = rho, group = profile_nr)) +
  coord_flip() +
  scale_x_reverse()
