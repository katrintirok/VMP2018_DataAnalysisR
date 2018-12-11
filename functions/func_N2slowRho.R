
func_N2slowRho <- function(profnum,fs=128,diss_length=5,overlap=diss_length/2){
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
    select(Pressure_dbar, Temperature_degC, Conductivity_mS_per_cm, lat) %>%
    # collect data from database to put into tibble
    collect()
  # disconnect from database
  DBI::dbDisconnect(vmp_profiles)
  
  # --------------------------------------------------------
  # calculate density from temp and conductivity
  # --------------------------------------------------------
  # -----------------------------------------------------------
  # --- calculate sea water chars for average data ----------
  # -----------------------------------------------------------
  C <- vmp_df$Conductivity_mS_per_cm
  t <- vmp_df$Temperature_degC
  p <- vmp_df$Pressure_dbar
  
  # practical salinity using conductivity in ...?
  SP = gsw_SP_from_C(C,t,p)
    # absolute salinity
  #average latitude for vmp profiles = -27.52277 and av long = 32.7259
  long <- 32.7259
  lat <- -27.52277
  SA = gsw_SA_from_SP(SP,p,long,lat)
    # conservative temperature from in-situ temperature
  CT = gsw_CT_from_t(SA,t,p)
    # in-situ density
  rho = gsw_rho(SA,CT,p)
    # potential density
  p_ref = 0      #[dbar] --> =0bar (next would be 100bar=1000dbar, but max p of 300dbar closer to 0
  rho_pot = gsw_rho(SA,CT,p_ref)
  
  vmp_df$rho_pot <- rho_pot
  vmp_df$CT <- CT
  vmp_df$SA <- SA
  vmp_df$rho <- rho
  
  rm(SP,SA,CT,rho,rho_pot, lat, long, C, p, t, p_ref)
  

  
  # ---------------------------------------------------------
  # calculate N and N^2 from slow density profiles, 
  # --------------------------------------------------------
  # --- 1) sort profiles for indreasing density ---
  b <- vmp_df
  # sort pressure increasing
  b <- b[order(b$Pressure_dbar, decreasing=F),]
  # make subset with pressure and density to be sorted for dens
  b_Rp <- select(b,Pressure_dbar,rho_pot, SA, CT)   #pressure, rho
  b_R <- select(b, Pressure_dbar, rho)
  # sort rho increasing!, in subsets
  b_Rps <- b_Rp[order(b_Rp$rho_pot, decreasing=F),]
  b_Rs <- b_R[order(b_R$rho, decreasing = F), ]
  # add sorted Temp column to dataframe b  
  b$Rp_s <- b_Rps$rho_pot
  b$CT_s <- b_Rps$CT
  b$SA_s <- b_Rps$SA
  b$R_s <- b_Rs$rho
  # add sorted Temp pressure to calculate Thorpe's scales for density
  b$pressure_Rps <- b_Rps$Pressure_dbar
  b$pressure_Rs <- b_Rs$Pressure_dbar
  b$tsc_Rp <- b$pressure_Rps - b$Pressure_dbar
  b$tsc_R <- b$pressure_Rs - b$Pressure_dbar
  

  # --- 2) calculate N and N^2 ---
  # calculate N as dT/dP --> gradient of density over pressure
  # calculate per time step and then average over same distance as eps estimation --> e.g. averaging no of samples = 128*5
  
  # set length of segment to average over and overlap of segments; 
  # fs, diss_length and overlap are inputs into the fuction with default values of 128, 5, and 2.5
  seg_length <- fs * diss_length
  overlap_length <- fs * overlap

  d <- data.frame()
  seg_st <- 1
  seg_en <- seg_st + seg_length - 1
  b_i <- b[seg_st:seg_en, ]
  while(length(b$Pressure_dbar) >= (seg_st + seg_length)){
    attach(b_i)
    grav <- gsw_grav(lat)
    N2m_s_Rp <- (grav[2:length(grav)]/Rp_s[2:length(Rp_s)] * 
                abs((Rp_s[2:length(Rp_s)] - Rp_s[1:length(Rp_s)-1]))/    
                (Pressure_dbar[2:length(Pressure_dbar)]-Pressure_dbar[1:length(Pressure_dbar)-1]))
    N2m_s_R <- (grav[2:length(grav)]^2 + 
                  abs((R_s[2:length(R_s)] - R_s[1:length(R_s)-1]))/    
                  (Pressure_dbar[2:length(Pressure_dbar)]-Pressure_dbar[1:length(Pressure_dbar)-1]))
    N2gsw_s <- gsw_Nsquared(SA_s, CT_s, Pressure_dbar,lat)
    N2i <- data.frame(profile_nr=profnum, pressure=mean(Pressure_dbar), 
                      N2_s_Rp=mean(N2m_s_Rp), N2_s_R=mean(N2m_s_R), 
                      N2gsw=mean(N2gsw_s$N2), pgsw=mean(N2gsw_s$p_mid),
                      tsc_Rp=sqrt(mean(b_i$tsc_Rp^2)), tsc_R=sqrt(mean(b_i$tsc_R^2)))
    detach(b_i)
    d <- rbind(d, N2i)
    seg_st <- seg_st + (seg_length-overlap_length)
    seg_en <- seg_st + seg_length - 1
    b_i <- b[seg_st:seg_en, ]
  }
  
  # return table d with average values of N2 and Thorpe's scale and pressure for profnum
  return(d)
  
} # end of function