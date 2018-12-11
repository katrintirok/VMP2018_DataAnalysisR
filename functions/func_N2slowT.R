
func_N2slowT <- function(profnum,fs=128,diss_length=5,overlap=diss_length/2){
  # function to calculate N^2 from slow Temperature measurements for a given profile
  # takes:
  # profnum = nr of profile to test; essential, no default
  # fs = sampling frequency; defaults to 128 (sampling freq slow sensors (Temp,Cond) for VMP profiles at Sodwana Bay in 2018)
  # diss_length = length of segment for averaging in seconds (name as in odas function to calculate epsilon)
  # overlap = overlap of averaging segments in seconds
  
  
  # --- load libraries ---
  library(dplyr)
  library(dbplyr)
  library(gsw)
  
  # --- read data ---
  #connect data base SQLite_VMP
  vmp_profiles <- DBI::dbConnect(RSQLite::SQLite(), "data/SQLite_VMP.sqlite")
  # read out data from VMP_profiles_slowData_cleaned table for profile given as input to function
  vmp <- tbl(vmp_profiles, "VMP_profiles_slowData_cleaned")
  gps <- tbl(vmp_profiles, 'VMP_profiles_info_GPS_clean')
  vmp_df <- vmp %>%
    filter(profile_nr == profnum) %>%
    # to get latitude for profile, join with gps table
    inner_join(., gps, by = 'profile_nr') %>%
    # choose wich variables needed for further calculations
    select(Pressure_dbar, Temperature_degC, lat) %>%
    # collect data from database to put into tibble
    collect()
  # disconnect from database
  DBI::dbDisconnect(vmp_profiles)
  

  # ---------------------------------------------------------
  # calculate N and N^2 from slow temperature profiles, 
  # i.e. sort temperature profile like normally density profile
  
  # --- 1) sort profiles for decreasing temperature ---
  b <- vmp_df
  # sort pressure increasing
  b <- b[order(b$Pressure_dbar, decreasing=F),]
  # make subset with pressure and temp to be sorted for temp
  b_T <- select(b,Pressure_dbar,Temperature_degC)   #pressure, T
  # sort Temp decreasing!, in subsets
  b_Ts <- b_T[order(b_T$Temperature_degC, decreasing=T),]
  # add sorted Temp column to dataframe b  
  b$T_s <- b_Ts$Temperature_degC
  # add sorted Temp pressure to calculate Thorpe's scales for Temp
  b$pressure_Ts <- b_Ts$Pressure_dbar
  b$tsc_T <- b$pressure_Ts - b$Pressure_dbar

  # --- 2) calculate N and N^2 ---
  # calculate N as dT/dP --> gradient of Temp over pressure
  # calculate per time step and then average over same distance as eps estimation --> e.g. averaging no of samples = 1024*4
  
  # set length of segment to average over and overlap of segments; 
  # fs, diss_length and overlap are inputs into the fuction with default values of 1024, 5, and 2.5
  seg_length <- fs * diss_length
  overlap_length <- fs * overlap

  d <- data.frame()
  seg_st <- 1
  seg_en <- seg_st + seg_length - 1
  b_i <- b[seg_st:seg_en, ]
  while(length(b$Pressure_dbar) >= (seg_st + seg_length - 1)){
    attach(b_i)
    grav <- gsw_grav(lat)
    N2m_s_T <- (grav[2:length(grav)]/T_s[2:length(T_s)] * 
                abs((T_s[2:length(T_s)] - T_s[1:length(T_s)-1]))/    # since T is decreasing with depth I take absolute value of diff
                (Pressure_dbar[2:length(Pressure_dbar)]-Pressure_dbar[1:length(Pressure_dbar)-1]))
    N2i <- data.frame(profile_nr=profnum, pressure=mean(Pressure_dbar), 
                      N2_s_T=mean(N2m_s_T), tsc_T=sqrt(mean(b_i$tsc_T^2)))
    detach(b_i)
    d <- rbind(d, N2i)
    seg_st <- seg_st + (seg_length-overlap_length)
    seg_en <- seg_st + seg_length - 1
    b_i <- b[seg_st:seg_en, ]
  }
  
  # return table d with average values of N2 and Thorpe's scale and pressure for profnum
  return(d)
  
} # end of function