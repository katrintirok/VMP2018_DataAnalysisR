longdiff_deg2m <- function(dlon, alat){
  # function to convert longitude difference from decimal degree to metre
  # takes:
  # dlon = difference of longitude between 2 points in decimal degrees
  # alat = average latitude of the 2 points
  # returns: 
  # dx = difference of longitude in m
  rlat = alat * pi/180
  p = 111415.13 * cos(rlat) - 94.55 * cos(3*rlat)
  dx = dlon * p
  return(dx)
}