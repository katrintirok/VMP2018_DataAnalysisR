
func_LEfastT <- function(profnum,fs=1024,diss_length=5,overlap=diss_length/2){
  # function to calculate the Ellison scale from fast Temperature measurements for a given profile
  # takes:
  # profnum = nr of profile to test; essential, no default
  # fs = sampling frequency; defaults to 1024 (sampling freq for VMP profiles at Sodwana Bay an 2018)
  # diss_length = length of segment for averaging in seconds (name as in odas function to calculate epsilon)
  # overlap = overlap of averaging segments in seconds
  
  
  # --- load libraries ---
  library(dplyr)
  library(dbplyr)
  library(gsw)
  
  # --- read data ---
  #connect data base SQLite_VMP
  vmp_profiles <- DBI::dbConnect(RSQLite::SQLite(), "data/SQLite_VMP.sqlite")
  # read out data from VMP_profiles_fastData table for profile given as input to function
  vmp <- tbl(vmp_profiles, "VMP_profiles_fastData")
  gps <- tbl(vmp_profiles, 'VMP_profiles_info_GPS_clean')
  vmp_df <- vmp %>%
    filter(profile_nr == profnum) %>%
    # to get latitude for profile, join with gps table
    inner_join(., gps, by = 'profile_nr') %>%
    # choose wich variables needed for further calculations
    select(Pressure_dbar, T1_degC, T2_degC, lat) %>%
    # collect data from database to put into tibble
    collect()
  # disconnect from database
  DBI::dbDisconnect(vmp_profiles)
  
  # ---------------------------------------------------------
  # calculate LE from fast temperature profiles, 

  # --- 1) sort profiles for decreasing temperature ---
  b <- vmp_df
  # sort pressure increasing
  b <- b[order(b$Pressure_dbar, decreasing=F),]
  # make subset with pressure and temp to be sorted for temp
  b_T1 <- select(b,Pressure_dbar,T1_degC)   #pressure, T1
  b_T2 <- select(b, Pressure_dbar, T2_degC) #pressure, T2
  # sort T1 and T2 decreasing!, in subsets
  b_T1s <- b_T1[order(b_T1$T1_degC, decreasing=T),]
  b_T2s <- b_T2[order(b_T2$T2_degC, decreasing=T),]
  # add sorted T1 and T2 columns to dataframe b  
  b$T1_s <- b_T1s$T1_degC
  b$T2_s <- b_T2s$T2_degC
  # add sorted T1/T2 pressure to calculate Thorpe's scales for T1/T2
  b$pressure_T1s <- b_T1s$Pressure_dbar
  b$pressure_T2s <- b_T2s$Pressure_dbar
  b$tsc1 <- b$pressure_T1s - b$Pressure_dbar
  b$tsc2 <- b$pressure_T2s - b$Pressure_dbar

  # --- 2) calculate LE --> rms of individual LE values over diss_length ---
  # calculate N as dT/dP --> gradient of Temp over pressure
  # calculate per time step and then average over same distance as eps estimation --> e.g. averaging no of samples = 1024*4
  
  # set length of segment to average over and overlap of segments; 
  # fs, diss_length and overlap are inputs into the fuction with default values of 1024, 5, and 2.5
  seg_length <- fs * diss_length
  overlap_length <- fs * overlap

  d <- data.frame()
  seg_st <- 1
  seg_en <- seg_st + seg_length - 1
  seg_mid <- seg_st + seg_length/2 - 1
  b_i <- b[seg_st:seg_en, ]
  while(length(b$Pressure_dbar) >= (seg_st + seg_length - 1)){
    attach(b_i)
    Tdev1 <- abs(T1_degC - T1_s)
    Tdev2 <- abs(T2_degC - T2_s)
    LE1i <- ifelse(Tdev1 == 0, 0, Tdev1/abs(tsc1))
    LE2i <- ifelse(Tdev2 == 0, 0, Tdev2/abs(tsc2))
    # calculate deltaT over range of L_T!!!, from mid of diss_length section
    # 1. calculate L_T over section
    tsc_T1=sqrt(mean(b_i$tsc1^2))
    tsc_T2=sqrt(mean(b_i$tsc2^2))
    # 2. get measurements over length of L_T (I need the falling speed for that?)
    delta1 <- c(b$Pressure_dbar[seg_mid] - tsc_T1/2, b$Pressure_dbar[seg_mid] + tsc_T1/2)
    delta2 <- c(b$Pressure_dbar[seg_mid] - tsc_T2/2, b$Pressure_dbar[seg_mid] + tsc_T2/2)
    deltaT1s <- T1_s[Pressure_dbar == min(Pressure_dbar[Pressure_dbar >= delta1[1]])] - 
      T1_s[Pressure_dbar == max(Pressure_dbar[Pressure_dbar <= delta1[2]])]
    deltaT2s <- T2_s[Pressure_dbar == min(Pressure_dbar[Pressure_dbar >= delta2[1]])] - 
      T2_s[Pressure_dbar == max(Pressure_dbar[Pressure_dbar <= delta2[2]])]
    LEi <- data.frame(profile_nr=profnum, pressure=mean(Pressure_dbar), 
                      LE_T1=sqrt(mean(LE1i^2)), LE_T2=sqrt(mean(LE2i^2)),
                      tsc_T1=sqrt(mean(b_i$tsc1^2)), tsc_T2=sqrt(mean(b_i$tsc2^2)),
                      LEb_T1=sqrt(mean(Tdev1^2))/(abs(deltaT1s)/sqrt(mean(b_i$tsc1^2))), 
                      LEb_T2=sqrt(mean(Tdev2^2))/(abs(deltaT2s)/sqrt(mean(b_i$tsc2^2))))
    detach(b_i)
    d <- rbind(d, LEi)
    seg_st <- seg_st + (seg_length-overlap_length)
    seg_en <- seg_st + seg_length - 1
    seg_mid <- seg_st + seg_length/2 - 1
    b_i <- b[seg_st:seg_en, ]
  }
  
  # return table d with average values of N2 and Thorpe's scale and pressure for profnum
  return(d)
  
} # end of function