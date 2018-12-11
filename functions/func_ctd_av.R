# function to average ctd data over diss_length

func_ctd_av <- function(profdata,fs=1024/8,diss_length=5,overlap=diss_length/2){
 
  library(dplyr)
  
  seg_length <- fs * diss_length   # fs for slow data, i.e. CTD data, is 128 (fast data fs = 1024)
  overlap_length <- fs * overlap
  
  d <- data.frame()
  seg_st <- 1
  seg_en <- seg_st + seg_length - 1
  profdata_i <- profdata[seg_st:seg_en, ] %>%
    select(Pressure_dbar, profile_nr, Temperature_degC, SP, rho)
  
  while(length(profdata$Pressure_dbar) >= (seg_st + seg_length - 1)){
    attach(profdata_i)
      Tav <- mean(Temperature_degC, na.rm = T)
      SPav <- mean(SP, na.rm = T)
      rhoav <- mean(rho, na.rm = T)
      pmid <- mean(Pressure_dbar)
      d_i <- data.frame(profile_nr=profile_nr[1], pressure=pmid, Temp_degC = Tav, SP = SPav, rho = rhoav)
    detach(profdata_i)
      
    d <- rbind(d, d_i)
    
    seg_st <- seg_st + (seg_length-overlap_length)
    seg_en <- seg_st + seg_length - 1
    profdata_i <- profdata[seg_st:seg_en, ]
    
  } # end of while loop
  
  # return table d with average values of Temp and SP and pressure for profnum
  return(d)
  
} # end of function
  