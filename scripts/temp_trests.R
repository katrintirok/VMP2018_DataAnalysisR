# import fast data for tests with temperature

library(R.matlab)
library(tidyverse)
library(ggplot2)
library(stats4)  # for mle 


### Reading in the data
## original fast temperature measurements
temp_fast <- read_csv('data/VMP_profiles_fast_032.csv')
gradT1_diff <- c(NA,temp_fast$T1_degC[2:length(temp_fast$T1_degC)] - 
  temp_fast$T1_degC[1:length(temp_fast$T1_degC)-1])

# plot temp and temp_gradient over time for different fft_length (0.5, 1, 2)
fs_fast <- 1024  # [1/s]--> sampling frequency
fft_length <- 2  # [s]
seg_length <- fft_length * fs_fast   # length of segment 
nr_plots <- floor(length(temp_fast$gradT1)/seg_length)

slope <- c()
intercept <- c()
slope_mt <- c()
intercept_mt <- c()
for(i in seq(1,nr_plots)){
    seg_st <- (i-1)*seg_length+1
    seg_en <- i*seg_length
  gradT1_i <- temp_fast$gradT1[seg_st:seg_en]
  T1_i <- temp_fast$T1_degC[seg_st:seg_en]
  # calculate linear model and slope over data
 lm_i <- lm(gradT1_i ~ seq(1,seg_length)) 
 slope[i] <- lm_i$coefficients[2]
 intercept[i] <- lm_i$coefficients[1]
 # remove trend
 #T1_i_mt <- T1_i - (slope[i]*seq(1,seg_length) + intercept[i])
 gradT1_i_mt <- gradT1_i - (slope[i]*seq(1,seg_length)+intercept[i])
 # check again after removing trend
 lm_i_mt <- lm(gradT1_i_mt ~ seq(1,seg_length))
 slope_mt[i] <- lm_i_mt$coefficients[2]
 intercept_mt[i] <- lm_i_mt$coefficients[1]
}

hist(slope)
plot(seq(1,nr_plots), slope)
plot(seq(1,nr_plots), intercept)
plot(seq(1,nr_plots), slope_mt)

# plot gradient and model and detrended gradient
plot(seq(1,seg_length),gradT1_i,type='l') 
lines(seq(1,seg_length), lm_i$coefficients[2]*seq(1,seg_length)+lm_i$coefficients[1], col='red')
lines(seq(1,seg_length), gradT1_i_mt, col='blue')

#
plot(seq(1,seg_length),gradT1_i_mt,type='l') 
lines(seq(1,seg_length), lm_i_mt$coefficients[2]*seq(1,seg_length)+lm_i_mt$coefficients[1], col='red')

# test with real temperature
plot(seq(1,seg_length),T1_i,type='l') 
#lines(seq(1,seg_length), lm_i$coefficients[2]*seq(1,seg_length)+lm_i$coefficients[1], col='red')
#plot(seq(1,seg_length), T1_i_mt, col='blue')
9/0
# for fft_length = 2 
# max at i=29
# check i=24 --> dangerous, introduces 'new' partial trend?


plot(seq(1,seg_length),T1_i)


# temperature spectra
diss <- readMat('data/Diss_032_fft2.mat')
diss_fieldnames <- unlist(readMat('data/diss_fieldnames_fft2.mat'))
# spectrum energy values
# spec 3D array
spec1 <- diss[[1]][[47]][[1]]
sp_gradT1 <- spec1[,1,]
sp_gradT2 <- spec1[,2,]

# temperature average over spec steps
P_sp <- diss[[1]][[14]][5,]
T1_sp <- diss[[1]][[14]][7,]
T2_sp <- diss[[1]][[14]][9,]

test <- fft(gradT1_i)
plot(test)
