#calculate N2

#load packages
library(dplyr)
library(ggplot2)
library(gsw)

#rm(list=ls())

#import data
a <- read.csv('data_processed/VMP_gswVariables.csv',header=T, sep=',')

#sort profiles for density (potential density!)
b <- a %>%
  group_by(profile_nr) %>%
  arrange(rho_pot, .by_group = T)

for(i in 1:max(b$profile_nr)){
  bi <- subset(b, profile_nr==i)
  #1st sort dataframe for depth
  bi <- bi[order(bi$Pressure_dbar, decreasing=F),]
  #2nd add sorted rho column and SA and CT
  bi.r <- select(bi, profile_nr, Pressure_dbar, SA, CT, rho_pot)   #pressure, SA, CT, rho_pot
  bi.rs <- bi.r[order(bi.r$rho_pot, decreasing=F),]
  bi$rho_pot.s <- bi.rs$rho_pot
  bi$CT.s <- bi.rs$CT
  bi$SA.s <- bi.rs$SA
  bi$pressure.s <- bi.rs$Pressure_dbar
  if(exists('dat_c')){
    dat_c <- rbind(dat_c, bi)
  }else{
    dat_c <- bi
  }
}

#average latitude for vmp profiles = -27.52277 and av long = 32.7259
dat_c$long <- 32.7259
dat_c$lat <- -27.52277

#calculate N2 of sorted densities and unsorted densities, per dive
for(i in 1:max(dat_c$profile_nr)){
  ci <- subset(dat_c, profile_nr==i)
  #sort for depth(p)
  if(nrow(ci) > 0){
    ci <- ci[order(ci$Pressure_dbar, decreasing=F),]
    attach(ci)
    N2.s <- gsw_Nsquared(SA.s,CT.s,Pressure_dbar,lat)
    N2   <- gsw_Nsquared(SA,CT,Pressure_dbar,lat)
    grav <- gsw_grav(lat)
    N2m.s <- (grav[2:length(grav)]/rho_pot.s[2:length(rho_pot.s)] * 
            (rho_pot.s[2:length(rho_pot.s)]-rho_pot.s[1:length(rho_pot.s)-1])/
            (Pressure_dbar[2:length(Pressure_dbar)]-Pressure_dbar[1:length(Pressure_dbar)-1]))
    N2m   <- (grav[2:length(grav)]/rho_pot[2:length(rho_pot)] *
              (rho_pot[2:length(rho_pot)]-rho_pot[1:length(rho_pot)-1])/
              (Pressure_dbar[2:length(Pressure_dbar)]-Pressure_dbar[1:length(Pressure_dbar)-1]))
    tsc <- pressure.s - Pressure_dbar
    N2i <- data.frame(profile_nr=profile_nr[1:length(profile_nr)-1],long=long[1:length(long)-1], p=N2$p_mid, 
                    N2.s=N2.s$N2, N2=N2$N2, N2m.s, N2m, tsc=tsc[1:length(tsc)-1])
    detach(ci)
    if(exists(x='d')){
      d <- rbind(d, N2i)
    }else{
      d <- N2i
    }
  }
}

#where pressure was equal, Inf values generated --> delete
d$N2[d$N2==-Inf | d$N2==Inf] <- NaN 
d <- na.omit(d)

#write.csv(d, 'data_processed/N2_all.csv', row.names = F)
#=============================================================================================


#=============================================================================================
# calculate averages
# what are the depth ranges for epsilon?


pl.data <- read.csv("data_processed/vmp_N2_all.csv", header=T)

#plot N2 over pressure
plot(pl.data$N2.s, pl.data$p, ylim = rev(range(pl.data$p)))
plot(pl.data$tsc, pl.data$p, ylim=rev(range(pl.data$p)))

#scaterplot with N2 colour coded
ggplot(d, aes(x=long, y=p)) +
  geom_point(aes(col = log10(N2m.s)), size=0.8, shape='c') +
  scale_colour_gradientn(colours = rainbow(20)) + # terrain.colors(20)) 
  scale_y_reverse() 

#scaterplot with tsc (thorpson scale) colour coded
ggplot(d, aes(x=dist, y=p)) +
  geom_point(aes(col = abs(tsc)), size=0.8, shape='c') +
  scale_colour_gradientn(colours = rainbow(10)) + # terrain.colors(20)) 
  scale_y_reverse()





