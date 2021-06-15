#Misha Khan
#BANA 288 Predictive Analytics
#Spring 2021

#Homework #6: ARIMA Models

################################################################################
################################################################################
################################################################################

#1. Generate 200 observations of the following series.  In each case, plot the 
#series generated.  Also, graph the autocorrelation and partial auto-correlation 
#functions.  Finally, in each case, comment on how the pattern in the auto- and 
#partial auto-correlation functions are related to the series generated.  
#Hint:  Use the “rnorm” function in R for part a and build the other series from this.
library("fpp2")

################################################################################

#a. White noise
set.seed(123456)
parta <- ts(data.frame(rnorm(200)))
#Plot
autoplot(parta)
#Autocorrelation
Acf(parta)
# Partial Autocorrelation
Pacf(parta)

#Interpretation: Looking at ACF graph, there's no auto correlation because the data is white noise
#WN is a series of random generated values from a normal distribution
################################################################################

#b. AR(1) with parameter, phi1 = 0.6
partb <- ts(data.frame(matrix(rep(0), 200, 1)))
partb[1, 1] <- parta[1]
for (i in 2:200) {
  partb[i,1] <- 0.6 * partb[i-1, 1] + parta[i]
}
# Plot
autoplot(partb)
# Auto-correlation
Acf(partb)
# Partial Auto-correlation
Pacf(partb)

#Interpretation: ACF - There is autocorrelation present in lags 1-4 
#PACF (consider residuals) - Only for lag 1 there is a sig correlation followed
#by correlations that are not sig so it shows a autoregressive pattern
################################################################################

#c.AR(2) with parameters, phi1 = 0.6 and phi2 = 0.3
partc <- ts(data.frame(matrix(rep(0), 200, 1)))
partc[1, 1] <- parta[1]
partc[2, 1] <- parta[2]
for (i in 3:200) {
  partc[i, 1] <- 0.6 * partc[i-1, 1] + 0.3 * partc[i-2, 1] + parta[i]
}
# Plot
autoplot(partc)
# Auto-correlation
Acf(partc)
# Partial Auto-correlation
Pacf(partc)

#ACF - Autocorrelation present in lags 1-14 (data is highly autocorrelated)
#PACF - Shows 2 significant lags which makes sense because we are running a AR(2) model
#(there's an additional AR parameter added therefore sig correlation with res in lag 1 and 2)
#then following with correlations that are not sig so it shows a autoregressive pattern
################################################################################

#d. AR(2) with parameters, phi1 = 0.8 and phi2 = – 0.3
partd <- ts(data.frame(matrix(rep(0), 200, 1)))
partd[1, 1] <- parta[1]
partd[2, 1] <- parta[2]
for (i in 3:200) {
  partd[i, 1] <- 0.8 * partd[i-1, 1] + (-0.3) * partd[i-2, 1] + parta[i]
}
# Plot
autoplot(partd)
# Auto-correlation
Acf(partd)
# Partial Auto-correlation
Pacf(partd)

#ACF - Autocorrelation present lag 1-2
#PACF - Shows 2 significant lags which makes sense because we are running a AR(2) model 
#pattern is autoregressive 
################################################################################

#e. MA(1) with parameter, phi1 = 0.6
parte <- ts(data.frame(matrix(rep(0), 200, 1)))
parte[1, 1] <- parta[1]
for (i in 2:200) {
  parte[i,1] <- parta[i] + 0.6 * parta[i-1]
}
# Plot
autoplot(parte)
# Auto-correlation
Acf(parte)
# Partial Auto-correlation
Pacf(parte)

#ACF tells us the q term for MA model
#ACF - Autocorrelation present in lag 1 which makes sense because our model is MA(1)
#PACF - Shows 2 significant lags and at lag 3 there is a slight significance
#There is a high spike at the first lag then followed by a positive negative
#wave pattern. This shows us that there is a MA term in the data

################################################################################

#f. ARMA(1,1) with parameters phi1 = 0.5 and theta1 = 0.4
partf <- ts(data.frame(matrix(rep(0), 200, 1)))
partf[1, 1] <- parta[1]
for (i in 2:200) {
  partf[i,1] <- (0.5 * partf[i-1, 1]) + (0.4 * parta[i-1]) + parta[i]
}
# Plot
autoplot(partf)
# Auto-correlation
Acf(partf)
# Partial Auto-correlation
Pacf(partf)

#ACF - Lag 1-3 show autocorrelation (decreasing shows autoregressive, ACF tells us order for MA)
#PACF - We can see a MA term (go up and down) in the first 2 lags (PACF tells us order for AR and if there is a MA term)
#PACF - significant correlations at the lag 1 and 2 followed by correlations which arent sig (showing an autoregressive pattern) 
#which makes sense because its an ARMA model.
################################################################################

#g. ARIMA(1,1,1) with parameters, phi1 = 0.5 and theta1 = 0.4
partg <- partf
partg[1,1] <- parta[1]
for (i in 2:199) {
  partg[i, 1] <- partg[i-1, 1] + partf[i]
}
# Plot
autoplot(partg)
# Auto-correlation
Acf(partg)
# Partial Auto-correlation
Pacf(partg)

#ACF - Autocorrelation is very significant, highly autoregressive pattern
#PACF - significant correlations from lag 1-3, followed by correlations 
#that are not sig, showing autoregressive pattern
################################################################################

#h. ARIMA(1,1,1)(0,1,0)[4] with parameters, phi1 = 0.5 and theta1 = 0.4
parth <- ts(partg, frequency = 4)
for (i in 5:200){
  parth[i] <- partg[i] + parth[i-4]
}
# Plot
autoplot(parth)
# Auto-correlation
Acf(parth)
# Partial Auto-correlation
Pacf(parth)

#(0,1,0) seasonality

#ACF - Autocorrelation is very significant, highly autoregressive pattern
#PACF - significant correlations from lag 1, followed by correlations 
#that are not sig, showing autoregressive pattern
################################################################################
################################################################################
################################################################################

#2. For the US Quarterly GDP data, complete the following steps

################################################################################

#a.  Set up the data set for analysis.  If necessary, find a suitable Box-Cox 
#transformation for the data set
#neg lambda = log trans (take log of the series to stabilize the variance of the series)
#pos lamdba = box cox

dat <- read.csv("~/Desktop/SPRING 2021/Predictive/Homework 6/hw6_USGDP.csv")
dat.ts <- ts(dat[,2], frequency = 4, start = 1947)

autoplot(dat.ts)
Acf(dat.ts)
Pacf(dat.ts)

#Boxcox for autocorrelation
#H0: Data is uncorrelated in time
#Ha: Data is correlated in time
Box.test(dat.ts, lag = 1) #Low p value so we reject H0, data is correlated
Box.test(dat.ts, lag = 4) #Low p value so we reject H0, data is correlated

lambda = BoxCox.lambda(dat.ts) #lambda = 0.2351377
dat.ts.tran = BoxCox(dat.ts, lambda)
autoplot(dat.ts.tran)

################################################################################

#b. Fit a suitable ARIMA model 
mod1 <- auto.arima(dat.ts.tran)
summary(mod1)
#Model Order: ARIMA(3,1,2)(0,1,2)[4] 
#AIC: 20.94
#RMSE: 0.2406697

################################################################################

#c. Try 3 other ARIMA models by experimenting with the model orders, p and q.

mod2 <- Arima(dat.ts.tran, order = c(0, 1, 2), seasonal = c(0, 1, 2)) #p from 3 to 0
summary(mod2)
#AIC= 35.47
#RMSE = 0.2495725
mod3 <- Arima(dat.ts.tran, order = c(3, 1, 0),  seasonal = c(0, 1, 2)) #q from 2 to 0
summary(mod3)
#AIC = 36.02
#RMSE =  0.2489254
mod4 <- Arima(dat.ts.tran, order = c(0, 1, 0), seasonal = c(0, 1, 2)) #p and q to 0
summary(mod4)
#AIC = 52.98
#RMSE = 0.2589859

################################################################################

#d. Choose the preferred model and check the residual diagnostics.  Report observations.
res.1 <- residuals(mod1)
ggtsdisplay(res.1)

checkresiduals(mod1)
#P is high, do not reject H0 
#Data is uncorrelated
#No longer any significant lags

################################################################################

#e. Produce forecasts for the next two years.  Do not forget to transform the 
#data back to the original form, if necessary!

#We want FC in terms of original data so transform it back
mod.1fc = auto.arima(dat.ts.tran, lambda = lambda) # ARIMA(3,1,2)(0,1,2)[4] # specifying lambda converts back to original scale
forecast1 <- forecast(mod.1fc, h = 8) #quarterly data, so 8 quarters for 2 years

autoplot(forecast1) + 
  xlab("Quarter") + 
  ylab("GDP")

################################################################################

#f.Compare the results with those obtained from running ets() on the non-transformed series.
mod5 <- ets(dat.ts)
accuracy(mod5)

forecast2 <-  forecast(mod5, h = 8)

autoplot(forecast2) + 
  xlab("Quarter") + 
  ylab("GDP")

#AIC: 7045.018
#RMSE: 17998.28

################################################################################
################################################################################
################################################################################

#3.Repeat the previous question using the US single family home sales data.

################################################################################
#a.  Set up the data set for analysis.  If necessary, find a suitable Box-Cox 
#transformation for the data set

dat2 <- read.csv("~/Desktop/SPRING 2021/Predictive/Homework 6/hw6_one_family_homes.csv")
dat2.ts <- ts(dat2[,2], frequency = 12, start = 1963)

autoplot(dat2.ts)
Acf(dat2.ts)
Pacf(dat2.ts)

Box.test(dat2.ts, lag = 1) #Low p value so we reject H0, data is correlated
Box.test(dat2.ts, lag = 12) #Low p value so we reject H0, data is correlated

lambda = BoxCox.lambda(dat2.ts) #lambda = 0.5930056, no transformation required because not a sig different
dat.ts.tran2 = BoxCox(dat2.ts, lambda)
autoplot(dat.ts.tran2)

################################################################################

#b. Fit a suitable ARIMA model 
mod3.1 <- auto.arima(dat.ts.tran2)
summary(mod3.1)
#Model Order: ARIMA(2,1,3)(1,0,2)[12]
#AIC: 3525.71
#RMSE: 3.114723

################################################################################

#c. Try 3 other ARIMA models by experimenting with the model orders, p and q.

mod3.2 <- Arima(dat.ts.tran2, order = c(0,1,3), seasonal = c(1,0,2)) #p from 2 to 0
summary(mod3.2)
#AIC = 3530.93 
#RMSE = 3.136143
mod3.3 <- Arima(dat.ts.tran2, order = c(2,1,0),  seasonal = c(1,0,2)) #q from 3 to 0
summary(mod3.3)
#AIC = 3528.98
#RMSE = 3.136242
mod3.4 <- Arima(dat.ts.tran2, order = c(0,1,0), seasonal = c(1,0,2)) #p and q to 0
summary(mod3.4)
#AIC = 3563.12
#RMSE = 3.224937

################################################################################

#d. Choose the preferred model and check the residual diagnostics.  Report observations.
res3.1 <- residuals(mod3.1)
ggtsdisplay(res3.1)

checkresiduals(mod3.1)
#p-value = 0.04
#P is low,reject H0 
#Data is correlated, not a good model

################################################################################

#e. Produce forecasts for the next two years.  Do not forget to transform the 
#data back to the original form, if necessary!

#We want FC in terms of original data so transform it back
mod.2fc = auto.arima(dat.ts.tran2, lambda = lambda) # ARIMA(3,1,2)(0,1,2)[4] # specifying lambda converts back to original scale
forecast2 <- forecast(mod.2fc, h = 24) #monthly data, so 12 months for 2 years

autoplot(forecast2) + 
  xlab("Quarter") + 
  ylab("GDP")

################################################################################

#f.Compare the results with those obtained from running ets() on the non-transformed series.
mod3.5 <- ets(dat2.ts)
accuracy(mod3.5)

forecast3.2 <-  forecast(mod3.5, h = 24)

autoplot(forecast3.2) + 
  xlab("Quarter") + 
  ylab("GDP")

#RMSE: 45.04