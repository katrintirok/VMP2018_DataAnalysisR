# function for plots of profiles over fine bathymetry

func_VMPprofilesPlot <- function(plotdata, plotvar, figtitle = 'plotvar', vartitle = 'plotvar'){
  ###
  # plotdata are dataset that has values to plot
  # plotvar is variable to plot, e.g. eps or log10(sqrt(N...))
  # figtitle is part title of figure
  # vartitle is title of colourbar
  
  # add plotvar to plotdata
  plotdata$plotvar <- plotvar
  
  # join plotdata with gps data to plot over longitude  
  # read gps data
  gps <- read_csv('data_processed/VMP_profiles_info_GPS_clean.csv') %>%
    mutate(sample_day = factor(day(datetime_start_YMDHMS_UTC), levels=c(29,30,31,1), labels=c('Day1','Day2','Day3','Day4'))) %>%
    select(c('profile_nr', 'sample_day', 'lat', 'long'))
  # join with plotdata
  vmp_pldata_gps <- inner_join(plotdata, gps, by = 'profile_nr')
  
  # define average latitude per day to plot per latitude not per day
  vmp_pldata_gps_l <- vmp_pldata_gps %>%
    group_by(sample_day) %>%
    summarise(lat_av = factor(round(mean(lat),2), 
                              levels = c('-27.48', '-27.51', '-27.53', '-27.56'))) %>%
    inner_join(vmp_pldata_gps, by = 'sample_day')
  
  pl.data <- vmp_pldata_gps_l
  
  
  # plot all values over depth
  # plot_all <- ggplot(data = pl.data, mapping = aes(x = plotvar, y = pressure, col = long)) +
  #   geom_point(alpha = 0.3) +
  #   scale_y_reverse(limits=c(500,0)) +
  #   #scale_x_continuous(breaks = seq(-2.5, 0, by = 0.5), limits = c(-2.5, 0),
  #   #                   label = c('', expression(10^-2), '', expression(10^-1), '', expression(10^0))) + 
  #   labs(x = vartitle,
  #        y = 'depth (m)',
  #        title = paste('all profiles of', figtitle),
  #        subtitle = 'shades of blue represent longitude') +
  #   theme_bw()
  
  
  # -------------------------------------------------------------------------------------
  # --- plotting 'colour' profiles for N (sqrt of N2m_s --> N^2 manual from sorted profiles of temperature)
  # -------------------------------------------------------------------------------------
  # define colours for plotting plotvar
  topW <- quantile(pl.data$plotvar, probs=c(0.75)) + IQR(pl.data$plotvar)
  botW <- quantile(pl.data$plotvar, probs=c(0.25)) - IQR(pl.data$plotvar)
  top <- ifelse(topW>max(pl.data$plotvar), max(pl.data$plotvar), topW)  
  bot <- ifelse(botW<min(pl.data$plotvar), min(pl.data$plotvar), botW)  
  
  #make factor variable for colours
  var <- pl.data$plotvar
  colf <- ifelse(is.na(var),NaN,
                 ifelse(var<bot,bot,
                        ifelse(var>bot & var<=top,var,
                               top)))
  pl.data$colf <- colf
  
  #---------------------------------------------
  # plot plotvar with spread between profiles
  # ---------------------------------------------------
  pl.data_diff <- pl.data %>%
    group_by(sample_day, long) %>%
    summarise(mplotvar = mean(plotvar)) %>%
    mutate(longpl = long) %>%
    mutate(diff = c(NA,diff(longpl, lag = 1))) %>%
    select(sample_day, long, mplotvar, longpl, diff)
  
  mindiff <- 0.002
  while(sum(round(pl.data_diff$diff,3)<mindiff, na.rm = T) != 0){
    pl.data_diff$longpl <- ifelse(!is.na(pl.data_diff$diff) & pl.data_diff$diff < mindiff, 
                                  pl.data_diff$longpl + mindiff - pl.data_diff$diff, 
                                  pl.data_diff$longpl)
    pl.data_diff <- pl.data_diff %>%
      mutate(diff = c(NA,diff(longpl, lag = 1)))
  }
  
  # now join long1 again with datj_lat using sample-day and long as keys
  pl.data_spread <- inner_join(pl.data, pl.data_diff, by=c('sample_day','long')) %>%
    select(-diff, mplotvar)
  
  ## plot
  plotP <- ggplot(data = pl.data_spread, mapping = aes(x = longpl, y = pressure)) +
    facet_grid(lat_av ~ .)
  plotPa <- plotP + 
    geom_point(mapping = aes(colour = colf), pch = 15, size = 1.2) +
    scale_y_reverse() +
    scale_color_gradientn(colours = femmecol(200), na.value = 'transparent',
                          guide = guide_colorbar(title=vartitle)) +#,
#                          limits=c(min(pl.data$colf),max(pl.data$colf)), 
#                          breaks=seq(from = signif(min(pl.data$colf),2)-ceil(abs(min(pl.data$colf)-signif(min(pl.data$colf),2))), 
#                                     to = signif(max(pl.data$colf),2) + ceil(abs(max(pl.data$colf)-signif(max(pl.data$colf),2))), 
#                                     by = 5*ceil(abs(min(pl.data$colf)-signif(min(pl.data$colf),2)))), 
#                          label = c('0.63', '0.10','0.16', '0.25')) +
    theme_bw()
  
  
  # ---------------------------------------------------------------------------------------
  ### add bathymetry to plots ---
  #----------------------------------------------------------------------------------------
  # use combined bathy for lines
  comb_bathy <- read_csv('data_processed/BathyCombLong_latav.csv')
  # change level -27.54 to -27.54 for lat_av (for consistency with levels from ohter data sets)
  comb_bathy$lat_av <- factor(round(comb_bathy$lat_av,2)) 
  levels(comb_bathy$lat_av) <- c(levels(comb_bathy$lat_av), '-27.53')
  comb_bathy$lat_av[comb_bathy$lat_av == '-27.54'] <- '-27.53'
  # replace NA values in fine bathy with gebco bathy 
  comb_bathy$fine_bathy[is.na(comb_bathy$fine_bathy)] <- comb_bathy$gebco_bathy[is.na(comb_bathy$fine_bathy)]
  bathy_line <- comb_bathy
  
  # add points for echosound depth at measured profiles check with measured depth
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
  
  bathy_points <- bathy_la %>%
    inner_join(vmp_gps, by='profile_nr') %>%
    select(sample_day, profile_nr, lat_av, lon, bathy, depth_m_bathymetry.x, depth_m_echosound)
  
  
  # plto profiles with bathymentry
  plotPB <- plotPa + 
    # fine bathy
    geom_line(data = bathy_line, aes(x=lon, y = abs(fine_bathy)), size=0.2, col='gray') +
    # depth from profiles
    geom_point(data = bathy_points, aes(x = lon, y = depth_m_bathymetry.x), col = 'black', size = 0.6) + 
    labs(x = 'longitude', y = 'depth (m)',
         title=paste(figtitle, 'profiles for individual sample days from north (top) to south (bottom)'),
         subtitle = 'profiles stretched out to avoid overlaying,
         lines show bathymetry with points at positions of profiles') +
    facet_grid(lat_av ~ .)
  
  return(plotPB)
} # end function
