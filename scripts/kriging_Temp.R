#try local kriging

#load packages
library(reshape)
library(gstat)
library(sp)
#library(kriging)
#library(geoR)

rm(list=ls())

#------------- read in data -----------------------------------------------------
vmp <- read_csv('data/VMP_profiles_averaged.csv') %>%
  rename('profile_nr' = 'profile_no')

gps <- read_csv('data_processed/VMP_profiles_info_GPS_clean.csv') %>%
  mutate(sample_day = factor(day(datetime_start_YMDHMS_UTC), levels=c(29,30,31,1), labels=c('Day1','Day2','Day3','Day4'))) %>%
  select(c('profile_nr', 'sample_day', 'lat', 'long'))

# join gsw table and gps table
vmp_gps <- inner_join(vmp, gps, by = 'profile_nr')
head(vmp_gps)

# define average latitude per day to plot per latitude not per day
vmp_gps_l <- vmp_gps %>%
  group_by(sample_day) %>%
  summarise(lat_av = factor(round(mean(lat),2), 
                            levels = c('-27.48', '-27.51', '-27.53', '-27.56'))) %>%
  inner_join(vmp_gps, by = 'sample_day')
head(vmp_gps_l)



# --- which data to use ------------------------
c <- filter(vmp_gps_l, sample_day == "Day2")
c$depth <- c$Pressure_dbar
c$var <- c$Temperature_degC
head(c)

#standardise data
c$long.s <- (c$long-min(c$long))/(max(c$long)-min(c$long))
c$depth.s <- (c$depth-min(c$depth))/(max(c$depth)-min(c$depth))


#fit variogram model
f1 <- as.data.frame(c)
#f1 <- as.data.frame(c[(length(c$depth)-10000):length(c$depth),])
coordinates(f1) <- ~long.s + depth.s

#for epsilon --> no anisotropy trend!? (at least when using standardised depth and distance)
#use universal kriging --> formula in variogram needs to contain x or y
x2 <- 0.26
v2 <- variogram(var~long.s+depth.s, f1, width = 0.05) #, cutoff=x2, width=x2/100)
fitvm2 <- fit.variogram(v2, vgm(c("Wav","Gau","Ste","Mat","Sph","Per")), fit.kappa = T) #   vgm(c("Exp", "Mat", "Sph","Ste")),

#plot line of fitted model
plot(v2,fitvm2)
#print results in console
#fitvm2


## --- kriging ---
#new data grid
x.range <- as.integer(range(f1$long.s)) #+ c(0,0.55)
y.range <- as.integer(range(f1$depth.s)) #+ c(0.3,0.85) 
data.grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.005),
                        y=seq(from=y.range[1], to=y.range[2], by=0.005))

coordinates(data.grd) <- ~x+y
gridded(data.grd) <- TRUE
plot(data.grd, cex=0.5)
points(f1, pch=1, col='red', cex=0.7) 


# universal kriging
# variogram model from fitted model
m2 <- vgm(psill=fitvm2$psill[2], model=fitvm2$model[2],range=fitvm2$range[2], 
          nugget=fitvm2$psill[1],kappa=fitvm2$kappa[2])

#f2 <- data.frame(c[c(7,9,10)])
f2 <- data.frama(select(c, depth.s, long.s, var))
#f2 <- f2[(length(f2$var)-10000):length(f2$var),]
f2 <- rename(f2,c(long.s='x',depth.s='y'))
coordinates(f2) <- ~x+y

# universal local kriging
#maximum distance --> 20km (ca 31.5 m for depth)
maxd <- (min(f1$long))/(max(f1$long)-min(f1$long))
kr.l <- krige(var ~ x + y, f2, newdata=data.grd, model = m2, maxdist = maxd, nmin=10)

#spplot(kr.l["var1.pred"], main = "universal local kriging predictions")

kr.l.res <- kr.l@data
kr.l.resC <- kr.l@coords
kriging.ul <- data.frame(kr.l.res,kr.l.resC)
kriging.ul <- rename(kriging.ul, c(var1.pred='T.pred',var1.var='T.var',
                                   x='long.m',y='depth.m'))

#convert standardised dist and depth back to real values in km and m
krigign.ul$long <- kriging.ul$long.m * (max(c$long)-min(c$long)) + min(c$long)
kriging.ul$depth <- kriging.ul$depth.m * (max(c$depth)-min(c$depth)) + min(c$depth)

#export results
#write.csv(kriging.ul,'kriged_eps1.csv', row.names = F)

