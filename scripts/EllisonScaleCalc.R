library(dplyr)
library(dbplyr)
library(ggplot2)
library(gsw)
library(tidyverse)

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

# sort profiles for temperature
b <- vmp53_df
# sort pressure increasing
b <- b[order(b$Pressure_dbar, decreasing=F),]
#2nd add sorted T1 column a
b_t <- select(b,Pressure_dbar,T1_degC,T2_degC)   #pressure, T1, T2
# sort T1 decreasing!
b_t1s <- b_t[order(b_t$T1_degC, decreasing=T),]
b_t2s <- b_t[order(b_t$T2_degC, decreasing=T),]
# add sorted T1 to b  
b$T1_s <- b_t1s$T1_degC
b$T2_s <- b_t2s$T2_degC
b$pressure1_s <- b_t1s$Pressure_dbar
b$pressure2_s <- b_t2s$Pressure_dbar
#b$tsc <- b$pressure1_s - b$Pressure_dbar
head(b)


seg_length <- 1024 * 0.5

seg_st <- 1
seg_en <- seg_st + seg_length -1
b_i <- b[seg_st:seg_en, ]

c <- b_i %>%
  select(profile_nr,Pressure_dbar, time_sec,T1_degC, T2_degC, T1_s, T2_s) %>%
  gather(key=Temp_var, -c(Pressure_dbar,time_sec,profile_nr), value=T_degC)

p1 <- ggplot(data = c) + 
  geom_line(aes(y=T_degC, x=Pressure_dbar, col=Temp_var)) +
  scale_x_reverse() +
  coord_flip()


# fit linear model through sorted profile and get rsq value
md1 <- filter(c, Temp_var == 'T1_s')
mod1 <- lm(Pressure_dbar ~ T_degC, md1)
smod1 <- summary(mod1)
rsq <- smod1$r.squared


# calculate N as dT/dP --> gradient of Temp over pressure
# calculate per time step and then average over same distance as eps estimation --> e.g. averaging no of samples = 1024*4
# model data
md <- b

fs <- 1024
fft_length <- 0.5
seg_length <- fs * fft_length
overlap <- fs * fft_length/2  #seg_length/(fft_length/2)

#nr_segments <- floor(length(b$time_sec)/(seg_length - overlap))

rsq <- data.frame(p_mid = NA, rsqT1 = NA, rsqT2 = NA, rsqT1b = NA, rsqT2b = NA)
seg_st <- 1
seg_en <- seg_st + seg_length - 1
md_i <- md[seg_st:seg_en, ]
count <- 1
while(length(md$profile_nr) > (seg_st + seg_length - 1)){
  #for(i in 1:nr_segments){
  mod1 <- summary(lm(T1_s ~ Pressure_dbar, md_i))
  mod2 <- summary(lm(T2_s ~ Pressure_dbar, md_i))
  # do for sorted T per segment
  md_i_t <- select(md_i,Pressure_dbar,T1_degC,T2_degC)   #pressure, T1, T2
  # sort T1 decreasing!
  md_i_t1s <- md_i_t[order(md_i_t$T1_degC, decreasing=T),]
  md_i_t2s <- md_i_t[order(md_i_t$T2_degC, decreasing=T),]
  md_i$T1b_s <- md_i_t1s$T1_degC
  md_i$T2b_s <- md_i_t2s$T2_degC
  mod1b <- summary(lm(T1b_s ~ Pressure_dbar, md_i))
  mod2b <- summary(lm(T2b_s ~ Pressure_dbar, md_i))
  rsq_i <- data.frame(p_mid = mean(md_i$Pressure_dbar), 
                      rsqT1 = mod1$r.squared, 
                      rsqT2 = mod2$r.squared,
                      rsqT1b = mod1b$r.squared,
                      rsqT2b = mod2b$r.squared)
  rsq <- rbind(rsq, rsq_i)
  # set new segment start/end
  seg_st <- seg_st + (seg_length-overlap)
  seg_en <- seg_st + seg_length - 1
  md_i <- md[seg_st:seg_en, ]
  count <- count + 1
}
rsq <- rsq[-1,]

pl.data <- gather(rsq, key = var, -p_mid, value = rsq)
pl.data$rcat <- ifelse(pl.data$var %in% c('rsqT1','rsqT2'), 'entire profile sorted', 'segment sorted')
ggplot(data = pl.data) + 
  geom_line(aes(y = rsq, x = p_mid, col = var)) +
  scale_x_reverse() +
  coord_flip() +
  theme_bw() +
  facet_grid(. ~ rcat)
  



