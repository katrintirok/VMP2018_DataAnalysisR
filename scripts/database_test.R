# access data base

library(dplyr)
library(dbplyr)
library(ggplot2)
library(gsw)

#connect data base
vmp_profiles <- DBI::dbConnect(RSQLite::SQLite(), "data/SQLite_VMP.sqlite")
# show infos about database
src_dbi(vmp_profiles)

# read out data from fastData table for profile 53 (KZN_032)
vmp53 <- tbl(vmp_profiles, "VMP_profiles_fastData")
vmp53_df <- vmp53 %>%
  filter(profile_nr == 53) %>%
  select(-c(Chla_mug_L, Turbidity)) %>%
  collect()

head(vmp53_df)

# calculate N and N^2
# 2) use fast temperature profile, i.e. sort temperature profile like normally density profile

# sort profiles for temperature
b <- vmp53_df
# sort pressure increasing
b <- b[order(b$Pressure_dbar, decreasing=F),]
#2nd add sorted T1 column a
b_t <- select(b,Pressure_dbar,T1_degC)   #pressure, T1, T2
# sort T1 decreasing!
b_ts <- b_t[order(b_t$T1_degC, decreasing=T),]
# add sorted T1 to b  
b$T1_s <- b_ts$T1_degC
b$pressure_s <- b_ts$Pressure_dbar
b$tsc <- b$pressure_s - b$Pressure_dbar
head(b)

ggplot(data = subset(b, time_sec > 0 & time_sec <= time_sec[1024*4])) + 
  geom_line(aes(y=T1_degC, x=Pressure_dbar)) +
  geom_line(aes(y=T1_s, x=Pressure_dbar), col='red') +
  scale_x_reverse() +
  coord_flip()

# calculate N as dT/dP --> gradient of Temp over pressure
# calculate per time step and then average over same distance as eps estimation --> e.g. averaging no of samples = 1024*4
fs <- 1024
fft_length <- 5
seg_length <- fs * fft_length
overlap <- fs * fft_length/2  #seg_length/(fft_length/2)

#nr_segments <- floor(length(b$time_sec)/(seg_length - overlap))

b$lat <- -27.52277  # need to get proper lat!, join with gps data table

p_mid <- c()
N2m_s_av <- c()
seg_st <- 1
seg_en <- seg_st + seg_length
b_i <- b[seg_st:seg_en, ]
while(length(b$profile_nr) > (seg_st + seg_length)){
#for(i in 1:nr_segments){
  attach(b_i)
  grav <- gsw_grav(lat)
  N2m_s <- (grav[2:length(grav)]/T1_s[2:length(T1_s)] * 
              abs((T1_s[2:length(T1_s)] - T1_s[1:length(T1_s)-1]))/    # since T is decreasing with depth I take absolute value of diff
              (Pressure_dbar[2:length(Pressure_dbar)]-Pressure_dbar[1:length(Pressure_dbar)-1]))
  p_mid <- c(p_mid,mean(Pressure_dbar))
  N2m_s_av <- c(N2m_s_av,mean(N2m_s))
  detach(b_i)
  seg_st <- seg_st + (seg_length-overlap)
  seg_en <- seg_st + seg_length
  b_i <- b[seg_st:seg_en, ]
}

# plot(N2m_s_av,p_mid)


# now make it a function!!!





