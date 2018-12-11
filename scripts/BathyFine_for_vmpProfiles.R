### join bathy data with vmp data to get depth per profile (longitude and latitude)
#rm(list=ls())

library(tidyverse)
library(fields)
library(lubridate)
library(rgdal)

# detailed bathymetry data as xyz data (x and y in utm)
#gebco <- read_csv('data_processed/GebcoT.csv') 
sodwana <- read_csv('data/Bathymetry Data/Sodwana.xyz', col_names = c('x','y','z'))
# convert UTM to lat/long
# 1) make spatial points object with defined projection
utms <- SpatialPoints(sodwana[-c(3)], proj4string=CRS("+proj=utm +zone=36 +south +datum=WGS84"))
# 2) transfrom to geo-coordinates
latlong <- spTransform(utms, CRS("+proj=longlat +datum=WGS84"))
# 3) add coordinates to sodwana dataframe 
sodwana$lon <- latlong@coords[,1]
sodwana$lat <- latlong@coords[,2]
rm(utms, latlong)


# vmp gps data
vmp_gps <- read_csv('data_processed/VMP_profiles_info_GPS_clean.csv') %>%
  mutate(sample_day = factor(day(datetime_start_YMDHMS_UTC), levels=c(29,30,31,1), labels=c('Day1','Day2','Day3','Day4'))) %>%
  select('profile_nr', 'sample_day', 'lat', lon=long, 'depth_m_bathymetry')
# calculate average latitude to generate more data points along those
vmp_gps_alat <- vmp_gps %>%
  group_by(sample_day) %>%
  summarise(lat_av = mean(lat)) %>%
  inner_join(vmp_gps, by = 'sample_day')

# 1) group & summarise vmp-gps data by lat and long
vmp_gpsu <- vmp_gps_alat %>%
  group_by(lat_av, lat, lon) %>%
  summarise(m_depth = mean(depth_m_bathymetry)) %>%
  select(lon, lat, lat_av, m_depth)

# 2) choose range from gebco data within vmp data
sodwana_u <- sodwana$lon<min(vmp_gpsu$lon) | sodwana$lon>max(vmp_gpsu$lon) | sodwana$lat<min(vmp_gpsu$lat) | sodwana$lat>max(vmp_gpsu$lat)
sodwana <- sodwana[!sodwana_u,]
rm(sodwana_u)


# convert sodwana table to matrix using x and y!
sodwana_wide <- sodwana %>%
  select(x,y,z) %>%
  spread(key=y, value=z) 
#image(sodw_lon, sodw_lat, matrix_sodwana) #image comes out wrong ???
sodw_x <- sodwana_wide$x #unique(sodwana$x)
sodw_y <- as.numeric(colnames(sodwana_wide)[-1])

# 3) interpolate depth grid data per lon and lat 0.001 deg
library(fields)
# convert matrix with gebco data to numeric (from character)
matrix_sodwana <- as.matrix(sodwana_wide[-c(1)])
matrix_sodwana_num <- apply(matrix_sodwana, 2, as.numeric)
# define list with x, y and z values for interpolation
sodwana_list <- list(x = sodw_x, y = sodw_y, z = matrix_sodwana)
# get locations of vmp data
#vmp_loc <- as.matrix(data.frame(vmp_gpsu$lon, vmp_gpsu$lat))
vmp_lon <- c()
vmp_lat <- c()
lat_range <- levels(as.factor(vmp_gpsu$lat_av))
for(lat_av in lat_range){
  vmp_sub <- filter(vmp_gpsu, as.factor(lat_av) == lat_av)
  vmp_loni <- seq(min(vmp_sub$lon), max(vmp_sub$lon), 0.002)
  vmp_lati <- rep(as.numeric(lat_av), length(vmp_loni))
  vmp_lon <- c(vmp_lon, vmp_loni)
  vmp_lat <- c(vmp_lat, vmp_lati)
}
vmp_loc <- as.matrix(data.frame(vmp_lon, vmp_lat))
# convert vmp_loc geo coordinates to utm
# 1) make spatial points object with defined projection
vmp_latlong <- SpatialPoints(vmp_loc, proj4string=CRS("+proj=longlat +datum=WGS84"))
# 2) transfrom to geo-coordinates
vmp_utms <- spTransform(vmp_latlong, CRS("+proj=utm +zone=36 +south +datum=WGS84"))
vmp_locB <- vmp_utms@coords

# interpolate 
sodwana_bathy_int <- interp.surface(sodwana_list, vmp_locB)
#gebco_bathy <- data.frame(lon=vmp_gpsu$lon, lat=vmp_gpsu$lat, bathy=gebco_bathy_int)
sodwana_bathy <- data.frame(lon=vmp_lon, lat_av=vmp_lat, bathy=sodwana_bathy_int)
# interpolate over grid instead just profile points --> not working
# define grid points
#lon_grid <- seq(from = min(vmp_loc[,1]), to= max(vmp_loc[,1]), by = 0.002)
#lat_grid <- seq(fro= min(vmp_loc[,2]), to = max(vmp_loc[,2]), by = 0.002)
#vmp_grid <- list(lon_grid, lat_grid) #as.list(expand.grid(x=lon_grid, y=lat_grid))
#gebco_bathy_grid_int <- interp.surface.grid(gebco_list, vmp_grid)
#gebco_bathy_grid <- data.frame(lon=gebco_bathy_grid_int$x, lat = gebco_bathy_grid_int$y, bathy = gebco_bathy_grid_int$z)


#merge with vmp gps/day data
#vmp_bathy <- merge(gebco_bathy,vmp_gps, by=c('lat','lon'), all=T, sorted=F)
vmp_bathy <- merge(sodwana_bathy,vmp_gpsu, by=c('lat_av'), all=T, sorted=F)

ggplot(data = sodwana_bathy) + geom_point(aes(x = lon, y = lat_av, col = bathy)) # +
facet_grid(sample_day ~ .)
#write_csv(sodwana_bathy, 'data_processed/BathyFineLong_latav.csv')

# --> not really interpolated???, just got the bathymetry at the locations of profiles, but not in between!!!

#export
#write.csv(gebco_bathy, 'data_processed/BathyLong_latav.csv', row.names = F)


# combine gebco bathy and fine bathy
gebco_bathy <- read_csv('data_processed/BathyLong_latav.csv')
sodw_bathy <- read_csv('data_processed/BathyFineLong_latav.csv')

test <- sodw_bathy
test$bathy[is.na(test$bathy)] <- gebco_bathy$bathy[is.na(test$bathy)]

head(test)
ggplot(data = test) + geom_point(aes(x = lon, y = lat_av, col = bathy))


test$lat_av <- as.factor(round(test$lat_av,2))
# define sample day
test$sample_day <- ifelse(test$lat_av == 1, 'Day3', 
                           ifelse(test$lat_av==2, 'Day2', 
                                  ifelse(test$lat_av==3, 1, 'Day4')))
# change level -27.54 to -27.54 for lat_av (for consistency with levels from ohter data sets)
levels(test$lat_av) <- c(levels(test$lat_av), '-27.53')
test$lat_av[test$lat_av == '-27.54'] <- '-27.53'

# combine gebco_bathy and sodw_bathy
comb_bathy <- data.frame(lon = sodw_bathy$lon, lat_av=sodw_bathy$lat_av, 
                         fine_bathy=sodw_bathy$bathy, gebco_bathy=gebco_bathy$bathy)

bathypl <- ggplot(data=comb_bathy) + 
  geom_line(aes(x=lon, y=fine_bathy), size=0.6) + 
  geom_line(aes(x=lon, y=gebco_bathy), size=0.6, col='red') +
  labs(x="longitude", 
       y="bathymetry (m)") +
  facet_grid(lat_av ~ .) +
  theme_bw() +
  theme(legend.box.margin = margin(0,0,0,0,'mm'), plot.margin = margin(3,3,3,3,'mm'))
bathypl


#write_csv(comb_bathy, 'data_processed/BathyCombLong_latav.csv')

# check with measured depth
vmp_gps <- read_csv('data_processed/VMP_profiles_info_GPS_clean.csv')

bathyA <- read_csv('data_processed//BathyLong.csv')
bathy_la <- bathyA %>%
  group_by(sample_day) %>%
  summarise(lat_av = factor(round(mean(lat),2), 
                            levels = c('-27.48', '-27.51', '-27.54', '-27.56'))) %>%
  inner_join(bathyA, by = 'sample_day')
# change level -27.54 to -27.54 for lat_av (for consistency with levels from ohter data sets)
levels(bathy_la$lat_av) <- c(levels(bathy_la$lat_av), '-27.53')
bathy_la$lat_av[bathy_la$lat_av == '-27.54'] <- '-27.53'
head(bathy_la)

bathy_lb <- bathy_la %>%
  inner_join(vmp_gps, by='profile_nr') %>%
  select(sample_day, profile_nr, lat_av, lon, bathy, depth_m_bathymetry.x, depth_m_echosound)

#write_csv(bathy_lb, 'data_processed/BathyFine_perProfile.csv')

#bathypl2 <- bathypl +
  ggplot() + geom_point(data = bathy_lb, aes(x = lon, y = -1*depth_m_echosound), col = 'blue') +
  geom_point(data = bathy_lb, aes(x = lon, y = bathy), col= 'green') +
  geom_point(data = bathy_lb, aes(x = lon, y = -1*depth_m_bathymetry.x), col = 'red')
bathypl2
