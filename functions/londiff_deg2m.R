# this script contains functions to convert differences between longitudes and latigudes 
# from decimal degrees to meter and vice versa
# Reference: American Practical Navigator, Vol II, 1975 Edition, p5

longdiff_deg2m <- function(dlon, alat){
  # function to convert longitude difference from decimal degree to meter
  # takes:
  # dlon = vector of differences of longitude between 2 points in decimal degrees
  # alat = vector of average latitudes of the 2 points
  # returns: 
  # dx = vector of differences of longitude in m
  rlat <- alat * pi/180
  p <- 111415.13 * cos(rlat) - 94.55 * cos(3*rlat)
  dx <- dlon * p
  return(dx)
}

latdiff_deg2m <- function(dlat, alat){
  # function to convert latitude difference from decimal degree to meter
  # takes:
  # dlon = vector of differences of latitude between 2 points in decimal degrees
  # alat = vector of average latitudes of the 2 points
  # returns: 
  # dy = vector of differences of latitude in m
  rlat <- alat * pi/180
  m <- 111132.09 * rep(1, length(rlat)) - 566.05 * cos(2*rlat) + 1.2 * cos(4*rlat)
  dy <- dlat * m
  return(dy)
}

londiff_m2deg <- function(dx, alat){
  # function to convert longitude difference from meter to decimal degree
  # takes:
  # dx = vector of differences of longitude between 2 points in meter
  # alat = vector of average latitudes of the 2 points
  # returns: 
  # dlon = vector of differences of longitude in decimal degree
  rlat <- alat * pi/180
  p <- 111415.13 * cos(rlat) - 94.55 * cos(3*rlat)
  dlon = dx/p
  return(dlon)
}

latdiff_m2deg <- function(dy, alat){
  # function to convert latitude difference from meter to decimal degree
  # takes:
  # dy = vector of differences of latitude between 2 points in meter
  # alat = vector of average latitudes of the 2 points
  # returns: 
  # dlat = vector of differences of latitude in decimal degree
  rlat <- alat * pi/180
  m <- 111132.09 * rep(1, length(rlat)) - 566.05 * cos(2*rlat) + 1.2 * cos(4*rlat)
  dlat <- dy/m
  return(dlat)
}