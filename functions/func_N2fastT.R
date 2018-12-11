
func_N2fastT <- function(profnum,fs=1024,diss_length=5,overlap=diss_length/2){
  # function to calculate N^2 from fast Temperature measurements for a given profile
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
  # calculate N and N^2 from fast temperature profiles, 
  # i.e. sort temperature profile like normally density profile
  
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
  b$tsc_T1 <- b$pressure_T1s - b$Pressure_dbar
  b$tsc_T2 <- b$pressure_T2s - b$Pressure_dbar

  # --- 2) calculate N and N^2 ---
  # calculate N as dT/dP --> gradient of Temp over pressure
  # calculate per time step and then average over same distance as eps estimation --> e.g. averaging no of samples = 1024*4
  
  # set length of segment to average over and overlap of segments; 
  # fs, diss_length and overlap are inputs into the fuction with default values of 1024, 5, and 2.5
  seg_length <- fs * diss_length
  overlap_length <- fs * overlap

  d <- data.frame()
  seg_st <- 1
  seg_en <- seg_st + seg_length
  b_i <- b[seg_st:seg_en, ]
  while(length(b$Pressure_dbar) >= (seg_st + seg_length)){
    attach(b_i)
    grav <- gsw_grav(lat)
    N2m_s_T1 <- (grav[2:length(grav)]/T1_s[2:length(T1_s)] * 
                abs((T1_s[2:length(T1_s)] - T1_s[1:length(T1_s)-1]))/    # since T is decreasing with depth I take absolute value of diff
                (Pressure_dbar[2:length(Pressure_dbar)]-Pressure_dbar[1:length(Pressure_dbar)-1]))
    N2m_s_T2 <- (grav[2:length(grav)]/T2_s[2:length(T2_s)] * 
                   abs((T2_s[2:length(T2_s)] - T2_s[1:length(T2_s)-1]))/    # since T is decreasing with depth I take absolute value of diff
                   (Pressure_dbar[2:length(Pressure_dbar)]-Pressure_dbar[1:length(Pressure_dbar)-1]))
    N2i <- data.frame(profile_nr=profnum, pressure=mean(Pressure_dbar), 
                      N2_s_T1=mean(N2m_s_T1), N2_s_T2=mean(N2m_s_T2),
                      tsc_T1=sqrt(mean(b_i$tsc_T1^2)), tsc_T2=sqrt(mean(b_i$tsc_T2^2)))
    detach(b_i)
    d <- rbind(d, N2i)
    seg_st <- seg_st + (seg_length-overlap_length)
    seg_en <- seg_st + seg_length
    b_i <- b[seg_st:seg_en, ]
  }
  
  # return table d with average values of N2 and Thorpe's scale and pressure for profnum
  return(d)
  
} # end of function