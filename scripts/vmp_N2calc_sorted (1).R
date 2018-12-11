#calculate N2
# 1) use density from slow temp and conductivity
# 2) use fast temperature profile, i.e. sort temperature profile like normally density profile

#load packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(shape)


rm(list=ls())

#import data - all slow data!, not averaged over depth
a <- read.csv('data_processed/VMP_gswVariables.csv',header=T, sep=',')
gps <- read.csv('data_processed/VMP_profiles_info_GPS_clean.csv')

b <- a %>%
  merge(gps, by = 'profile_nr') %>%
  select(profile_nr, Pressure_dbar, SA, CT, rho_pot, lat)

#sort profiles for density (potential density!)
#b <- a %>%
#  group_by(profile_nr) %>%
#  arrange(rho_pot, .by_group = T)

c <- data.frame()
for(i in 1:max(b$profile_nr)){
  bi <- subset(b, profile_nr==i)
  #1st sort dataframe for pressure
  bi <- bi[order(bi$Pressure_dbar, decreasing=F),]
  #2nd add sorted rho column and SA and CT
  bi.r <- bi[2:5]   #pressure, SA, CT, rho_pot
  bi.rs <- bi.r[order(bi.r$rho_pot, decreasing=F),]
  bi$rho_pot.s <- bi.rs$rho_pot
  bi$CT.s <- bi.rs$CT
  bi$SA.s <- bi.rs$SA
  bi$pressure.s <- bi.rs$Pressure_dbar
  
  c <- rbind(c, bi)
}

#calculate N2 of sorted densities and unsorted densities, per profile
d <- data.frame()
for(i in 1:max(c$profile_nr)){
  ci <- subset(c, profile_nr==i)
  if(dim(ci)[1] > 0){
  #sort for depth(p)
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
  N2i <- data.frame(profile_nr=profile_nr[1:length(profile_nr)-1], pressure=N2$p_mid, 
                    N2.s=N2.s$N2, N2=N2$N2, N2m.s, N2m, tsc=tsc[1:length(tsc)-1])
  detach(ci)
  d <- rbind(d, N2i)
  } #end if statement
} # end for loop through profiles

#where pressure was equal, Inf values generated --> delete
d$N2[d$N2==-Inf | d$N2==Inf] <- NaN 
d <- na.omit(d)

#write.csv(d, 'data_processed/VMP_N2_all.csv', row.names = F)
pl.data <- read.csv("N2_all.csv", header=T)
#pl.data <- d


# ----------------------------------------------------------------------------------------------
# --- plots ---
# ----------------------------------------------------------------------------------------------
#plot N2 over pressure
plot(pl.data$N2m.s, pl.data$pressure, ylim = rev(range(pl.data$pressure)))
plot(pl.data$tsc, pl.data$pressure, ylim=rev(range(pl.data$pressure)))

pl.data.d <- pl.data %>%
  inner_join(gps, by = 'profile_nr') %>%
  mutate(sample_day = factor(day(datetime_start_YMDHMS_UTC), levels=c(29,30,31,1), labels=c('Day1','Day2','Day3','Day4'))) %>%
  select(profile_nr, sample_day, long, lat, pressure, N2.s, N2, N2m.s, N2m, tsc)

# Now defining average Latitude per day to plot per latitude not per day
datj_lat <- datj %>%
  group_by(sample_day) %>%
  summarise(lat_av = factor(round(mean(lat),2), 
                            levels = c('-27.48', '-27.51', '-27.53', '-27.56'))) %>%
  inner_join(datj, by = 'sample_day')
head(datj_lat)


#cut off at whiskers from boxplot?
pl.data.d$var <- log10(pl.data.d$N2.s)
pl.data.d$var[pl.data.d$var==-Inf | pl.data.d$var==Inf] <- NaN 
pl.data.d <- na.omit(pl.data.d)
var <- pl.data.d$var
topW <- quantile(var, probs=c(0.75)) + 1.5 * IQR(var)
botW <- quantile(var, probs=c(0.25)) - 1.5 * IQR(var)
top <- ifelse(topW>max(var), max(var), topW)   #3.574 for kriged, 5.06 for original
bot <- ifelse(botW<min(var), min(var), botW)   #-0.741 for kriged, -2.12 for original

top <- -1.683
bot <- -2.509

#scatterplot with N colour coded
#make factor variable for colours
var <- pl.data.d$var
colf <- ifelse(var<bot, bot, ifelse(var>bot & var<top, var, top))
pl.data.d$colf <- colf

#scatterplot with N2 colour coded
plot1 <- ggplot(data = pl.data.d, mapping = aes(x = long, y = pressure)) +
  geom_point(mapping = aes(colour = log10(N2)), pch = 0, size = 0.8) + 
  scale_color_gradientn(colours = femmecol(200), na.value = "transparent",
                        guide = guide_colorbar(title=expression(paste(N^2))),
                        limits=c(min(pl.data.d$colf),max(pl.data.d$colf))) #, breaks=seq(0,3,1), 
                        #label = c(expression(10^0), expression(10^1), 
                        #          expression(10^2), expression(10^3))

plot1 +
  facet_grid(sample_day ~ .) +
  scale_y_reverse() +
  #scale_color_gradientn(colours = femmecol(200), na.value = "transparent") +
  theme_bw()


#scatterplot with N2 colour coded
ggplot(d, aes(x=dist, y=pressure)) +
  geom_point(aes(col = log10(N2m.s)), size=0.8, shape='c') +
  scale_colour_gradientn(colours = rainbow(20)) + # terrain.colors(20)) 
  scale_y_reverse()
  
#scatterplot with tsc (thorp's scale) colour coded
ggplot(d, aes(x=dist, y=p)) +
  geom_point(aes(col = abs(tsc)), size=0.8, shape='c') +
  scale_colour_gradientn(colours = rainbow(10)) + # terrain.colors(20)) 
  scale_y_reverse()





