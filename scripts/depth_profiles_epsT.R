# profiles of epsilon and fast temperature, examples

# some from shallow, some from deep?


# Load libraries ----------------------------------------------------------
library(dbplyr)
library(tidyverse)
library(patchwork)
library(gridExtra)



# Read in data ------------------------------------------------------------

# choose profile numbers
profnum <- c(32,54,59,74) 
#32 = day1 #59 = day 3, #54 = day2, #74 = day4

# epsilon data
eps <- read_csv('data_processed/VMP_table.csv') %>%
  rename(profile_nr = prof_num) %>%
  arrange(profile_nr, pressure)

N2 <- read_csv('data_processed/VMP_N2_RHOslow_disslength.csv') %>%
  arrange(profile_nr, pressure)

# fast temperature data
#connect data base SQLite_VMP
vmp_profiles <- DBI::dbConnect(RSQLite::SQLite(), "data/SQLite_VMP.sqlite")
# read out data from VMP_profiles_fastData table for profile given as input to function
vmp <- tbl(vmp_profiles, "VMP_profiles_fastData")
gps <- tbl(vmp_profiles, 'VMP_profiles_info_GPS_clean')
temp_df <- vmp %>%
  filter(profile_nr %in% profnum) %>%
  # to get latitude for profile, join with gps table
  inner_join(., gps, by = 'profile_nr') %>%
  # choose wich variables needed for further calculations
  select(profile_nr, Pressure_dbar, T1_degC, T2_degC, lat) %>%
  # collect data from database to put into tibble
  collect()
# disconnect from database
DBI::dbDisconnect(vmp_profiles)



# find datapoints for zoomed in Temp graphs -------------------------------
# find datapoints for specific epsilon value
# find pressure for high eps around 90 - 110 m
# compare that mid pressure to temp pressures
# epsa <- filter(eps, profile_nr %in% 54)
# epsa$lgeps1 <- log10(epsa$eps1)
# 
# epsb <- epsa[epsa$pressure >= 110 & epsa$pressure <= 125,]
# # lg eps1 --> lowest -7.24 at 95 m, -7.77 at 91.7m, -7.41 at 98.2 m
# # these are the average pressures


# function to get segments ------------------------------------------------
segT <- function(segs, temp_df_pr){
  segsts <- segs - 1
  segst <- c()
  segend <- c()
  for(i in 1:3){
    segst[i] <- 1 + segsts[i] * 2.5 * 1024
    segend[i] <- segst[i] + 4 * (2.5 * 1024)  # 4 * diss_length/2!, because of overlap
  }
  segT1 <- temp_df_pr[segst[1]:segend[1],]
  segT2 <- temp_df_pr[segst[2]:segend[2],]
  segT3 <- temp_df_pr[segst[3]:segend[3],]
  
  segTs <- list(segT1=segT1,segT2=segT2,segT3=segT3)

  return(segTs)
} # end of function

# call function segT for the different profiles
# segments for profile_nr = 32: 27-29, 45-47, 92-94
segs <- list(pr32 = c(27,45,92), pr54 = c(37,71,91), pr59 = c(45,68,141), pr74 = c(11,62,94))
segTs <- vector('list',length(profnum))
names(segTs) <- names(segs)
for(i in 1:length(profnum)){
  temp_df_pr <- filter(temp_df, profile_nr == profnum[i])
  segT_pr <- segT(segs[[i]], temp_df_pr)
  segTs[[i]] <- segT_pr
} # end for loop

# Make plots --------------------------------------------------------------
xlimits <- c(470,0)
xbreaks <- seq(500,0,-50)

for(i in 1:length(profnum)){
  # temperature plot
  temp_df_pl <- temp_df %>%
    filter(profile_nr == profnum[i]) %>%
    gather(key = probe, -profile_nr, -Pressure_dbar, -lat, value = T_degC)
    
  plT <- ggplot(data = temp_df_pl, aes(x = Pressure_dbar, y = T_degC, col = probe)) +
    geom_line(aes(linetype = probe)) +
    coord_flip() +
    scale_x_continuous(trans = 'reverse', limits = xlimits, breaks = xbreaks) +
    scale_y_continuous(position = "right") +
    scale_color_grey(start = 0, end = 0.6) +
    theme_bw() +
    #labs(title = paste('profile', profnum)) +
    labs(x = 'Pressure (dBar)', y = expression(paste('Temperature (', degree, 'C)'))) +
    theme(legend.position="none")
#  plT
    
  # plot N
  N2_plt <- N2 %>%
    filter(profile_nr == profnum[i]) %>%
    select(pressure, N2_s_Rp)
  
  plN <- ggplot(data = N2_plt,
                 aes(x = pressure, y = log10(sqrt(N2_s_Rp)))) +
    geom_line() +
    geom_point(size = 1) +
    coord_flip() +
    scale_y_continuous(position = "right", limits = c(-3.2,-1.5), breaks = seq(-3,-1,1),
                     labels = c(expression(10^-3),expression(10^-2),expression(10^-1))) +
    scale_x_continuous(trans = 'reverse', limits = xlimits, breaks = xbreaks) +
    scale_color_grey(start = 0, end = 0.6) +
    theme_bw() +
    #labs(title = paste('profile', profnum)) +
    labs(x = '', y = 'N (1/s)') +
    theme(legend.position="none")
  #plN

  # plot epsilon
  eps_pl <- eps %>%
    filter(profile_nr == profnum[i]) %>% 
    select(pressure, profile_nr, eps1, eps2) %>%
    gather(key = probe, -pressure, -profile_nr, value = eps)

  plE <- ggplot(data = eps_pl, 
                aes(x = pressure, y = log10(eps), col = probe)) +
    geom_line(aes(linetype = probe)) +
    geom_point(aes(shape = probe), size = 1) +
    coord_flip() +
    scale_y_continuous(position = "right", limits = c(-9.5,-6.45), breaks = seq(-10,-6,1),
                       labels = c(expression(10^-10), expression(10^-9), expression(10^-8), 
                                  expression(10^-7), expression(10^-6))) +
    scale_x_continuous(trans = 'reverse', limits = xlimits, breaks = xbreaks) +
    scale_color_grey(start = 0, end = 0.6) +
    theme_bw() +
    #labs(title = paste('profile', profnum)) +
    labs(x = '', y = expression(paste(epsilon, ' (', m^2, '/', s^3, ')'))) +
    theme(legend.position="none")
#plE

# segments for zoomed in plots are in segTs
segT1 <- segTs[[i]]$segT1 %>%
  gather(key = probe, -profile_nr, -Pressure_dbar, -lat, value = T_degC)
segT2 <- segTs[[i]]$segT2 %>%
  gather(key = probe, -profile_nr, -Pressure_dbar, -lat, value = T_degC)
segT3 <- segTs[[i]]$segT3 %>%
  gather(key = probe, -profile_nr, -Pressure_dbar, -lat, value = T_degC)

plT_rect <- plT + 
  geom_rect(aes(xmin = min(segT1$Pressure_dbar) - 1, xmax = max(segT1$Pressure_dbar) + 1, 
                ymin = min(segT1$T_degC) - 0.05, 
                ymax = max(segT1$T_degC) + 0.05),
            col = 'red', size = 0.05, alpha = 0, inherit.aes = F) +
  geom_rect(aes(xmin = min(segT2$Pressure_dbar) - 1, xmax = max(segT2$Pressure_dbar) + 1, 
                ymin = min(segT2$T_degC) - 0.05, 
                ymax = max(segT2$T_degC) + 0.05),
            col = 'red', size = 0.05, alpha = 0, inherit.aes = F) +
  geom_rect(aes(xmin = min(segT3$Pressure_dbar) - 1, xmax = max(segT3$Pressure_dbar) + 1, 
                ymin = min(segT3$T_degC) - 0.05, 
                ymax = max(segT3$T_degC) + 0.05),
            col = 'red', size = 0.05, alpha = 0, inherit.aes = F) +
  theme(legend.position="none")

# plot rectangles to make where zoomed in
N2_plta <- N2 %>%
  filter(profile_nr == profnum[i]) %>%
  mutate(lgN = log10(sqrt(N2_s_Rp)))

plN_rect <- plN +  
  geom_rect(aes(xmin = min(segT1$Pressure_dbar) - 1, xmax = max(segT1$Pressure_dbar) + 1, 
                ymin = min(N2_plta$lgN[(segs[[i]][1]):(segs[[i]][1]+2)]) - 0.05, 
                ymax = max(N2_plta$lgN[(segs[[i]][1]):(segs[[i]][1]+2)]) + 0.05),
            col = 'red', size = 0.05, alpha = 0, inherit.aes = F) +
  geom_rect(aes(xmin = min(segT2$Pressure_dbar) - 1, xmax = max(segT2$Pressure_dbar) + 1, 
                ymin = min(N2_plta$lgN[(segs[[i]][2]):(segs[[i]][2]+2)]) - 0.05, 
                ymax = max(N2_plta$lgN[(segs[[i]][2]):(segs[[i]][2]+2)]) + 0.05),
            col = 'red', size = 0.05, alpha = 0, inherit.aes = F) +
  geom_rect(aes(xmin = min(segT3$Pressure_dbar) - 1, xmax = max(segT3$Pressure_dbar) + 1, 
                ymin = min(N2_plta$lgN[(segs[[i]][3]):(segs[[i]][3]+2)]) - 0.05, 
                ymax = max(N2_plta$lgN[(segs[[i]][3]):(segs[[i]][3]+2)]) + 0.05),
            col = 'red', size = 0.05, alpha = 0, inherit.aes = F) +
  theme(legend.position="none")

# plot rectangles to make where zoomed in
epsa <- eps %>%
  filter(profile_nr == profnum[i]) %>%
  mutate(lgeps1 = log10(eps1), lgeps2 = log10(eps2))

plE_rect <- plE +  
  geom_rect(aes(xmin = min(segT1$Pressure_dbar) - 1, xmax = max(segT1$Pressure_dbar) + 1, 
                ymin = min(min(epsa$lgeps1[(segs[[i]][1]):(segs[[i]][1]+2)]), 
                           min(epsa$lgeps2[(segs[[i]][1]):(segs[[i]][1]+2)])) - 0.05, 
                ymax = max(max(epsa$lgeps1[(segs[[i]][1]):(segs[[i]][1]+2)]),
                           max(epsa$lgeps2[(segs[[i]][1]):(segs[[i]][1]+2)])) + 0.05),
            col = 'red', size = 0.05, alpha = 0, inherit.aes = F) +
  geom_rect(aes(xmin = min(segT2$Pressure_dbar) - 1, xmax = max(segT2$Pressure_dbar) + 1, 
                ymin = min(min(epsa$lgeps1[(segs[[i]][2]):(segs[[i]][2]+2)]), 
                           min(epsa$lgeps2[(segs[[i]][2]):(segs[[i]][2]+2)])) - 0.05, 
                ymax = max(max(epsa$lgeps1[(segs[[i]][2]):(segs[[i]][2]+2)]),
                           max(epsa$lgeps2[(segs[[i]][2]):(segs[[i]][2]+2)])) + 0.05),
            col = 'red', size = 0.05, alpha = 0, inherit.aes = F) +
  geom_rect(aes(xmin = min(segT3$Pressure_dbar) - 1, xmax = max(segT3$Pressure_dbar) + 1, 
                ymin = min(min(epsa$lgeps1[(segs[[i]][3]):(segs[[i]][3]+2)]), 
                           min(epsa$lgeps2[(segs[[i]][3]):(segs[[i]][3]+2)])) - 0.05, 
                ymax = max(max(epsa$lgeps1[(segs[[i]][3]):(segs[[i]][3]+2)]),
                           max(epsa$lgeps2[(segs[[i]][3]):(segs[[i]][3]+2)])) + 0.05),
            col = 'red', size = 0.05, alpha = 0, inherit.aes = F) +
  theme(legend.position="none")

  #plE_rect
  
#plT + plE_rect

# plTzoom <- ggplot(data = filter(temp_df, Pressure_dbar < 110 & Pressure_dbar > 90), 
#                   aes(x = Pressure_dbar, y = T1_degC)) +
#   geom_line() +
#   coord_flip() +
#   scale_x_reverse()
# plTzoom

# arrange plots with zoomed in plot as inset
# library(cowplot)
# plT2 <- ggdraw() +
#   draw_plot(plT, 0, 0, 1, 1) +
#   draw_plot(plTzoom + labs(x='', y=''), 0.25, 0.07, 0.8, 0.5) #+
# #  draw_plot_label(c("A", "B"), c(0, 0.5), c(1, 0.92), size = 15)


# 3 zoomed temp plots -----------------------------------------------------
# 3 zoomed temp plots for profile_nr 32
  if(profnum[i] %in% c(74)){
    scale1 <- 1.2 - (max(segT1$T_degC) - min(segT1$T_degC))
  }else{
    scale1 <- 0.65 - (max(segT1$T_degC) - min(segT1$T_degC))
  }
  yTmin <- min(segT1$T_degC) - scale1/2
  yTmax <- max(segT1$T_degC) + scale1/2
  plTz1 <- ggplot(data = segT1, 
                  aes(x = Pressure_dbar, y = T_degC, col = probe)) +
    geom_line(aes(linetype = probe)) +
    coord_flip() +
    scale_x_continuous(trans = 'reverse', breaks = seq(500,10,-2)) +
    scale_y_continuous(position = "right", limits = c(yTmin,yTmax), 
                       breaks = if(i==4){seq(10,30,0.4)
                         }else{seq(10,30,0.2)}) +
    scale_color_grey() +
    labs(x='', y=expression(paste('T (', degree, 'C)'))) +
    theme_bw() +
    theme(legend.position="none")
  
  scale2 <- 0.65 - (max(segT2$T_degC) - min(segT2$T_degC))
  yTmin <- min(segT2$T_degC) - scale2/2
  yTmax <- max(segT2$T_degC) + scale2/2
  plTz2 <- ggplot(data = segT2, 
                  aes(x = Pressure_dbar, y = T_degC, col = probe)) +
    geom_line(aes(linetype = probe)) +
    coord_flip() +
    scale_x_continuous(trans = 'reverse', breaks = seq(500,10,-2)) +
    scale_y_continuous(position = "right", limits = c(yTmin,yTmax), breaks = seq(10,30,0.2)) +
    scale_color_grey() +
    labs(x='', y='') +
    theme_bw() + 
    theme(legend.position="none")
  
  scale3 <- 0.65 - (max(segT3$T_degC) - min(segT3$T_degC))
  yTmin <- min(segT3$T_degC) - scale3/2
  yTmax <- max(segT3$T_degC) + scale3/2
  plTz3 <- ggplot(segT3, 
                  aes(x = Pressure_dbar, y = T_degC, col = probe)) +
    geom_line(aes(linetype = probe)) +
    coord_flip() +
    scale_x_continuous(trans = 'reverse', breaks = seq(500,10,-2)) +
    scale_y_continuous(position = "right", limits = c(yTmin,yTmax), breaks = seq(10,30,0.2)) +
    scale_color_grey() +
    labs(x='', y='') +
    theme_bw() +
    theme(legend.position="none")


# library(cowplot)
# plT3 <- ggdraw() +
#    draw_plot(plTz1, x = 0, y = 0.68, width = 1, height = 0.33) +
#    draw_plot(plTz2, x = 0, y = 0.34, width = 1, height = 0.33) +
#    draw_plot(plTz3, x = 0, y = 0.02, width = 1, height = 0.33)
# plT3 <- plot_grid(plTz1, plTz2, plTz3, ncol = 1, nrow = 3, align = 'hv')
# # 
# png('figures/test_align.png', width = 7, height = 5, units = 'in', res = 150)
# plot_grid(plN_rect, plE_rect, plTz1,
#           ncol = 3,  rel_width = c(1,1,2), align = "vh")
#   #plot_grid(a, b, c, ncol = 1, align = "v", rel_heights = c(3, 1, 1))
# dev.off()
  

# combine all with ggarrange ----------------------------------------------
library(ggpubr)
p1 <- ggarrange(plT_rect, plN_rect, plE_rect, ncol = 3, nrow = 1, align = 'hv')
p2 <- ggarrange(plTz1, plTz2, plTz3, nrow = 3, ncol = 1, align = "hv")
figure_name <- paste('VerticalProfiles_', as.character(profnum[i]), sep='')
#pdf(paste('figures/', figure_name, '.pdf', sep=''), width = 7, height = 5)
png(paste('figures/', figure_name, '.png', sep=''), width = 7, height = 5, 
    units = "in", res = 300)
# arrange plots
ggarrange(p1, p2, ncol = 2, nrow = 1, widths = c(3,1))
dev.off()


# # combine all plots with plot_grid ----------------------------------------
# library(gridExtra)
# lay <- rbind(c(1,2,3,4),
#              c(1,2,3,5),
#              c(1,2,3,6))
#   figure_name <- paste('VerticalProfiles_test', as.character(profnum[i]), sep='')
# #pdf(paste('figures/', figure_name, '.pdf', sep=''), width = 7, height = 5)
# png(paste('figures/', figure_name, '.png', sep=''), width = 7, height = 5, units = "in", res = 300)
# grid.arrange(plN_rect, plN_rect, plE_rect, plTz1, plTz2, plTz3, layout_matrix = lay)
# dev.off()
} # end of for loop


# combined plot data
# temp_pl1 <- temp_df_pl %>%
#   rename(pressure = Pressure_dbar, value = T_degC) %>%
#   mutate(vartype = 'T') %>%
#   select(profile_nr, pressure, vartype, probe, value)
# eps_pl1 <- eps_pl %>%
#   rename(value = eps) %>%
#   mutate(vartype = 'eps') %>%
#   select(profile_nr, pressure, vartype, probe, value)
# N2_pl <- N2 %>%
#   filter(profile_nr == profnum[i]) %>%
#   select(pressure, profile_nr, N2_s_Rp)
# N2_pl1 <- N2_pl %>%
#   rename(value = N2_s_Rp) %>%
#   mutate(probe = 'N1', vartype = 'N') %>%
#   select(profile_nr, pressure, vartype, probe, value)
#   
# data_pl <- rbind(temp_pl1,eps_pl1,N2_pl1)
# 
# plot_all <- ggplot(data = data_pl, aes(x = pressure, y = value, col = probe)) +
#   geom_line(aes(linetype = probe)) +
#   coord_flip() +
#   scale_x_continuous(trans = 'reverse', limits = xlimits, breaks = xbreaks) +
#   scale_y_continuous(position = "right") +
#   scale_color_grey(start = 0, end = 0.6) +
#   theme_bw() +
#   #labs(title = paste('profile', profnum)) +
#  # labs(x = 'Pressure (dBar)', y = expression(paste('Temperature (', degree, 'C)'))) +
#   facet_wrap(~ vartype, scales = 'free') +
#   theme(legend.position="none")
# plot_all
