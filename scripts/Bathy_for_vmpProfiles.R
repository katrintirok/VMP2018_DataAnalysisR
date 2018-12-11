### join bathy data with vmp data to get depth per profile (longitude and latitude)
#rm(list=ls())

library(tidyverse)
library(fields)
library(lubridate)

# gebco bathymetry data as table
gebco <- read_csv('data_processed/GebcoT.csv') 
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
gebco_u <- gebco$lon<min(vmp_gpsu$lon) | gebco$lon>max(vmp_gpsu$lon) | gebco$lat<min(vmp_gpsu$lat) | gebco$lat>max(vmp_gpsu$lat)
gebco <- gebco[!gebco_u,]

# 3)
# read in original gebco ascii file (matrix)
gebco_matrix <- read.table("data/GEBCO2014.asc", header=F, skip=7, sep="")
# read header info from gebco ascii file which contains info on coordinates (corners and steps)
gebco_head <- read.table("data/GEBCO2014.asc", header=F, nrows=7, sep="") %>%
  spread(key = V1, value = V2)

#build vector of longitudes and latitudes
#rows are latitude! and columns are longitude (check with qgis if unsure)
lon <- seq(gebco_head$xllcorner, length.out=gebco_head$ncols, by=gebco_head$dx)
lat <- seq(gebco_head$yllcorner, length.out=gebco_head$nrows, by=gebco_head$dy)
lat <- lat[order(lat,decreasing = T)]

colnames(gebco_matrix) <- lon

# reshape gebco_matrix into proper table format columns lat, long, depth
gebco_table <- gebco_matrix %>%
  mutate(lat = lat) %>%
  gather(key = lon, value = depth, -lat)
#export gebco table
#write.csv(gebco_table, file='GebcoT.csv', row.names = F)

matrix_gebco <- t(as.matrix(gebco_matrix))
#order lat and xm for image
#orderi <- order(lat,decreasing=F)
lat_o <- lat[order(lat,decreasing = F)]
matrix_gebco_o <- matrix_gebco[,order(lat,decreasing = F)]
image(lon, lat_o, matrix_gebco_o) #image comes out wrong ???


# #) interpolate depth grid data per lon and lat 0.001 deg
library(fields)
# convert matrix with gebco data to numeric (from character)
matrix_gebco_num <- apply(matrix_gebco_o, 2, as.numeric)
# define list with x, y and z values for interpolation
gebco_list <- list(x=lon, y=lat_o, z=matrix_gebco_num)
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
# interpolate 
gebco_bathy_int <- interp.surface(gebco_list, vmp_loc)
#gebco_bathy <- data.frame(lon=vmp_gpsu$lon, lat=vmp_gpsu$lat, bathy=gebco_bathy_int)
gebco_bathy <- data.frame(lon=vmp_lon, lat_av=vmp_lat, bathy=gebco_bathy_int)
# interpolate over grid instead just profile points --> not working
# define grid points
#lon_grid <- seq(from = min(vmp_loc[,1]), to= max(vmp_loc[,1]), by = 0.002)
#lat_grid <- seq(fro= min(vmp_loc[,2]), to = max(vmp_loc[,2]), by = 0.002)
#vmp_grid <- list(lon_grid, lat_grid) #as.list(expand.grid(x=lon_grid, y=lat_grid))
#gebco_bathy_grid_int <- interp.surface.grid(gebco_list, vmp_grid)
#gebco_bathy_grid <- data.frame(lon=gebco_bathy_grid_int$x, lat = gebco_bathy_grid_int$y, bathy = gebco_bathy_grid_int$z)


#merge with vmp gps/day data
#vmp_bathy <- merge(gebco_bathy,vmp_gps, by=c('lat','lon'), all=T, sorted=F)
vmp_bathy <- merge(gebco_bathy,vmp_gps, by=c('lat_av'), all=T, sorted=F)

ggplot(data = gebco_bathy) + geom_point(aes(x = lon, y = lat_av, col = bathy)) # +
  facet_grid(sample_day ~ .)

  # --> not really interpolated???, just got the bathymetry at the locations of profiles, but not in between!!!

#export
#write.csv(gebco_bathy, 'data_processed/BathyLong_latav.csv', row.names = F)

