# read in grad_T data and spectra for further processing

# sample form profile 032

library(R.matlab)
library(tidyverse)
library(ggplot2)


# read in data
# spectrum energy values
spec1 <- readMat('data/gradT_spectra.mat')
# spec 3D array
spec1 <- spec1[[1]]
gradT1 <- spec1[,1,]
gradT2 <- spec1[,2,]
rm(spec1)

# wavenumber matrix
waven1 <- readMat('data/wavenums.mat')
# wavenumber 2D matrix with each column for one depth segment
waven1 <- waven1[[1]]

# frequency matrix
freq <- readMat('data/freqs.mat')
freq <- freq[[1]]

# correction
# read Diss_032 for additional variables
diss <- readMat('data/Diss_032.mat')

mean_speed <- diss[[1]][[16]]
pressure <- diss[[1]][[18]]
eps <- diss[[1]][[1]]
rm(diss)



# --------------------------------------------------------
#--- frequency response correction
# --------------------------------------------------------
F_0 <- 25*sqrt(mean_speed)  # frequency response of thermistor after Vachon and Lueck
tau_therm <- 2*pi*F_0 / sqrt(sqrt(2) - 1)   # time constant for double-pole response
tau_therm <- 1 / tau_therm

#correction:
gradT1_c <- matrix(data=NA, nrow=length(freq[,1]), ncol=length(freq[1,]))
for(index in 1:length(freq[1,])){
  gradT1_c[,index] <- gradT1[,index] * (1 + (2*pi*tau_therm[index]*freq[,index])^2)^2
 #from matlab: P_gradT1.* (1 + (2*pi*tau_therm[index]*F).^2).^2;'])
}

# Convert spectra to wavenumber spectra
for(index in 1:length(scalar_vector_names)
junk = ['P_' scalar_vector_names{index}];
eval([junk ' = ' junk '.* mean_speed;'])
end

P_gradT1_c <- matrix(NA, nrow = length(gradT1_c[,1]), ncol=length(gradT1_c[1,]))
for(index in 1:length(gradT1_c[,1])){
  P_gradT1_c[index,] <- gradT1_c[index,] * mean_speed
}

plot(log10(waven1[,3]), log10(gradT1_c[,3]), type='l', ylim=c(-8,0))
lines(log10(waven1[,1]), log10(gradT1[,1]), col='red')
lines(log10(waven1[,4]), log10(P_gradT1_c[,4]), col='blue')

# -----------------------------------------------------------------------
# --- fit model to spectra with chi as parameter estimated with MLE 
# -----------------------------------------------------------------------
eps1 <- eps[1,]

# model to fit:
# psi_gradT(k) = C_T * chi * eps^-1/3 * k^1/3
# psi_gadT --> corrected energy spectrum
# C_T = Obukhov-Corrsin universal constant with values between 0.3 and 0.5, 0.4 recommended
# k = wavenumber

# plot model for some chi values
C_T <- 0.4
chi <- 10^(-5)
k <- waven1[,1]
psi <- C_T * chi * eps1[1]^(-1/3) * k^(1/3)
plot(log10(k), log10(psi), type = 'l')

lines(log10(k), log10(psi), col='green')


# test with nasmyth spectrum of epsilon
#  phi_M(k) =  2*pi^4/3 * A_i * C * eps^2/3 * k^1/3
# A_i = 1.33*A = 1.33*(18/55)
# C = 1.5
eps <- 10^(-7)
phi_M <- 2*pi^(4/3)*1.33*(18/55)*1.5*eps^(2/3)*k^1/3
plot(log10(k), log10(phi_M), type='l')



# ----------------------------------------------
# --- try MLE fitting ----
# ---------------------------------------------
library(stats4)

segment <- 90

#--------------
C_T <- 0.4
chi <- 10^(-5)
k <- waven1[,segment]
psi <- C_T * chi * eps1[segment]^(-1/3) * k^(1/3)

mind <- length(waven1[waven1[,segment]<=70,segment]) # index for subsetting model values
y <- P_gradT1_c[1:mind,segment]   # temp-gradient spectrum --> P_gradT1_c
x <- waven1[1:mind,segment] 

#likelihood function definition
LL <- function(chi) {
  # Find residuals
  R = y - C_T * chi * eps1[segment]^(-1/3) * x^(1/3)
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  R = suppressWarnings(dnorm(R, log = TRUE))
  # Sum the log likelihoods for all of the data points
  -sum(R)
}

#model fit
fit <- suppressWarnings(mle(LL, start = list( chi=10^(-7))
                            ,nobs = length(y),  method='L-BFGS-B'))
summary(fit) 

# calculate model
spec_model <-  C_T * fit@coef[1] * eps1[segment]^(-1/3) * x^(1/3)


# plot data and model
pl_data <- data.frame(k_data=waven1[,segment], spec_data=P_gradT1_c[,segment])
pl_model <- data.frame(k_model = x[-1], spec_model = spec_model[-1])
ggplot() + 
  geom_point(data=pl_data, aes(x=k_data, y = spec_data)) + 
  geom_line(data=pl_model, aes(x=k_model, y=spec_model), color='red') +
  scale_y_continuous(trans='log10', limits=c(10^-9,10^-2)) +
  scale_x_log10()


# Questions:
# which range of wavenumbers to fit model to? --> 70 cpm?
# data spectrum doesnt really increase?
# model doesn't seem to fit data very well, why?, do I miss some more transformation of spectrum?
# do I have to convert from cpm to rad/m?

# check trend in temperature gradient over segment!
# to resolve lower wave numbers we need long enough segment, might need to detrend over segment


