library(forecast)
library(tseries)
library(corrplot)

council <- read.csv('~/desktop/Project4-Data/council.mth.data.csv', header = TRUE)
national <- read.csv('~/desktop/Project4-Data/natl.yr.mth.data.csv', header = TRUE)

### National data--------------------------------------------------------------
# Application and Assessment
natl.hl.app <- ts(national$Homeless.app, start = c(2002,4), end = c(2019,3), frequency = 12)
plot(natl.hl.app, xlab = 'Time', ylab = 'National applications')

natl.hl.ass <- ts(national$Homeless.assess, start = c(2002,4), end = c(2019,3), frequency = 12)
plot(natl.hl.ass)

# plot the series
par(mar=c(6,4,4,4))
plot(natl.hl.app, col = 'blue', ylab = '', main = '')
lines(natl.hl.ass, col = 'red')
legend('topright', legend = c('application', 'assessment'), 
       col = c('blue', 'red'),lty=1,lwd=2,cex=0.6)

# correlation between natl.app and natl.ass
cor.app.ass <- cor(national$Homeless.app, national$Homeless.assess, method = 'spearman')
# 0.9358529

# Model for natl.app
tsdisplay(natl.hl.app)
# Decomposing seasonal model
natl.app.components <- decompose(natl.hl.app)
natl.app.components1 <- decompose(natl.hl.app, type = 'multiplicative')
autoplot(natl.app.components1) + xlab('Time')
ggtsdisplay(natl.app.components$random)
ggtsdisplay(natl.app.components1$random)

natl.app.components$seasonal
xaxt <- c('4', '5', '6', '7', '8', '9', '10', '11', '12', '1', '2', '3')
plot(natl.app.components$figure,type='b',xlab='',xaxt='n') # plot seasonal variation
axis(1, at=seq(1:12), labels = xaxt)

natl.app.components$trend
plot(natl.app.components$trend)
axis(1,2002:2019)

natl.app.components$random
autoplot(natl.app.components) + xlab('Time')
ggtsdisplay(natl.app.components$random)
LB.natl.app <- NULL
for (i in 1:20) {
  LB.natl.app[i] <- Box.test(na.omit(natl.app.components$random),lag = i, 
                          type = 'Ljung-Box')$p.value
}
plot(LB.natl.app, type = 'b', ylim = c(0, 1))
abline(h = 0.05)
# not white noise

# Differencing
natl.hl.app %>% diff() %>% ggtsdisplay()
natl.hl.app %>% diff(lag=12) %>% ggtsdisplay()
natl.hl.app %>% diff(lag=12) %>% diff() %>% ggtsdisplay()

# Augmnented Dickey-Fuller Test P-value < 0.05, stationary
adf.test(natl.hl.app) # p-value = 0.1437
natl.hl.app %>% diff(lag=12) %>% adf.test() # p-value = 0.3414 
natl.hl.app %>% diff(lag=12) %>% diff() %>% adf.test() # p-value < 0.01, stationary

# Overall Seasonal Arima model
# d=1 and D=1
fit <- auto.arima(natl.hl.app, trace = TRUE) # ARIMA(2,1,0)(2,1,1)[12]
Arima.natl.app <- arima(natl.hl.app, order = c(2,1,0), 
                        seasonal = list(order=c(2,1,1), period=12))
print(Arima.natl.app) # AIC = 2669.88
# Coefficients:
#           ar1      ar2    sar1     sar2     sma1
#       -0.7683  -0.4077  0.3061  -0.1605  -0.8018
# s.e.   0.0684   0.0695  0.1122   0.0950   0.1031
plot(Arima.natl.app$residuals)
tsdisplay(Arima.natl.app$residuals)
checkresiduals(Arima.natl.app) # p-value = 0.01334 < 0.05 
res.natl.app <- NULL
for (i in 1:20) {
  res.natl.app[i] <- Box.test(na.omit(Arima.natl.app$residuals),lag = i, 
                             type = 'Ljung-Box')$p.value
}
plot(res.natl.app, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not good, especially when lag > 8

# Arima(2,1,0)(0,1,1)[12]
Arima.natl.app1 <- arima(natl.hl.app, order = c(2,1,0), 
                            seasonal = list(order=c(0,1,1), period=12)) # AIC = 2679.41
print(Arima.natl.app1)
# Coefficients:
#           ar1      ar2     sma1
#       -0.8314  -0.4793  -0.6859
# s.e.   0.0638   0.0641   0.0774
tsdisplay(Arima.natl.app1$residuals) # ACF: 3 spikes; PACF: 5 spikes
checkresiduals(Arima.natl.app1) # p-value = 4.167e-05
res.natl.app1 <- NULL
for (i in 1:20) {
  res.natl.app1[i] <- Box.test(na.omit(Arima.natl.app1$residuals),lag = i, 
                              type = 'Ljung-Box')$p.value
}
plot(res.natl.app1, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # good when lag = 3; not good, especially when lag > 8

# Arima(2,1,0)(0,1,2)[12]
Arima.natl.app2 <- arima(natl.hl.app, order = c(2,1,0), 
                         seasonal = list(order=c(0,1,2), period=12)) # AIC = 2668.73
print(Arima.natl.app2)
# Coefficients:
#           ar1      ar2     sma1     sma2
#       -0.7788  -0.4195  -0.5108  -0.2924
# s.e.   0.0670   0.0677   0.0795   0.0830

tsdisplay(Arima.natl.app2$residuals) # ACF: 2 spikes; PACF: 3 spikes
checkresiduals(Arima.natl.app2) # p-value = 0.01021
res.natl.app2 <- NULL
for (i in 1:20) {
  res.natl.app2[i] <- Box.test(na.omit(Arima.natl.app2$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(res.natl.app2, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # good when lag = 4; not good, especially when lag > 8

# Arima(2,1,0)(1,1,1)[12]
Arima.natl.app3 <- arima(natl.hl.app, order = c(2,1,0), 
                         seasonal = list(order=c(1,1,1), period=12)) # AIC = 2670.43
print(Arima.natl.app3)
# Coefficients:
#           ar1      ar2    sar1     sma1
#       -0.7890  -0.4308  0.3623  -0.9137
# s.e.   0.0659   0.0665  0.1125   0.1153

tsdisplay(Arima.natl.app3$residuals) # ACF: 4 spikes; PACF: 4 spikes
checkresiduals(Arima.natl.app3) # p-value = 0.002739
res.natl.app3 <- NULL
for (i in 1:20) {
  res.natl.app3[i] <- Box.test(na.omit(Arima.natl.app3$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(res.natl.app3, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # good when lag = 4; not good, especially when lag > 8

# Arima(1,1,0)(0,1,2)[12]
Arima.natl.app4 <- arima(natl.hl.app, order = c(1,1,0), 
                         seasonal = list(order=c(0,1,2), period=12)) # AIC = 2701.58
print(Arima.natl.app4)
# Coefficients:
#           ar1     sma1     sma2
#       -0.5432  -0.4806  -0.3988
# s.e.   0.0605   0.0902   0.0841

tsdisplay(Arima.natl.app4$residuals) # ACF: many spikes; PACF: 4 spikes
checkresiduals(Arima.natl.app4) # p-value < 2.2e-16
res.natl.app4 <- NULL
for (i in 1:20) {
  res.natl.app4[i] <- Box.test(na.omit(Arima.natl.app4$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(res.natl.app4, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not good

# Arima(2,1,1)(0,1,1)[12]
Arima.natl.app5 <- arima(natl.hl.app, order = c(2,1,1), 
                         seasonal = list(order=c(0,1,1), period=12)) # AIC = 2679.9
print(Arima.natl.app5)
# Coefficients:
#           ar1      ar2     ma1     sma1
#       -0.9874  -0.5652  0.2061  -0.6817
# s.e.   0.1211   0.0784  0.1469   0.0770

tsdisplay(Arima.natl.app5$residuals) # ACF: 3 spikes; PACF: 5 spikes
checkresiduals(Arima.natl.app5) # p-value = 7.457e-05
res.natl.app5 <- NULL
for (i in 1:20) {
  res.natl.app5[i] <- Box.test(na.omit(Arima.natl.app5$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(res.natl.app5, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # good when lag = 3; not good when lag > 9

# Arima(2,1,1)(0,1,2)[12]
Arima.natl.app6 <- arima(natl.hl.app, order = c(2,1,1), 
                         seasonal = list(order=c(0,1,2), period=12)) # AIC = 2670.08
print(Arima.natl.app6)
# Coefficients:
#           ar1      ar2     ma1     sma1     sma2
#       -0.9337  -0.5032  0.1895  -0.5131  -0.2798
# s.e.   0.1649   0.0985  0.1943   0.0790   0.0828

tsdisplay(Arima.natl.app6$residuals) # ACF: 2 spikes; PACF: 3 spikes
checkresiduals(Arima.natl.app6) # p-value = 0.01094
res.natl.app6 <- NULL
for (i in 1:20) {
  res.natl.app6[i] <- Box.test(na.omit(Arima.natl.app6$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(res.natl.app6, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # good when lag = 4; not good, espercially when lag > 8

# Arima(2,1,1)(1,1,1)[12]
Arima.natl.app7 <- arima(natl.hl.app, order = c(2,1,1), 
                         seasonal = list(order=c(1,1,1), period=12)) # AIC = 2671.55
print(Arima.natl.app7)
# Coefficients:
#           ar1      ar2     ma1    sar1     sma1
#       -0.9512  -0.5185  0.2020  0.3503  -0.8982
# s.e.   0.1512   0.0915  0.1796  0.1119   0.1034

tsdisplay(Arima.natl.app7$residuals) # ACF: 2 spikes; PACF: 3 spikes
checkresiduals(Arima.natl.app7) # p-value = 0.003854
res.natl.app7 <- NULL
for (i in 1:20) {
  res.natl.app7[i] <- Box.test(na.omit(Arima.natl.app7$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(res.natl.app7, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not good, espercially when lag > 8

# Arima(1,1,1)(0,1,1)[12]
Arima.natl.app8 <- arima(natl.hl.app, order = c(1,1,1), 
                         seasonal = list(order=c(0,1,2), period=12)) # AIC = 2672.42
print(Arima.natl.app8)
# Coefficients:
#           ar1      ma1     sma1     sma2
#       -0.2032  -0.6070  -0.5015  -0.3470
# s.e.   0.0967   0.0774   0.0834   0.0807

tsdisplay(Arima.natl.app8$residuals) # ACF: 7 spikes; PACF: 3 spikes
checkresiduals(Arima.natl.app8) # p-value = 1.151e-07
res.natl.app8 <- NULL
for (i in 1:20) {
  res.natl.app8[i] <- Box.test(na.omit(Arima.natl.app8$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(res.natl.app8, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not good when lag > 3

# Arima(2,1,2)(0,1,1)[12]
Arima.natl.app9 <- arima(natl.hl.app, order = c(2,1,2), 
                         seasonal = list(order=c(0,1,1), period=12)) # AIC = 2680.5
print(Arima.natl.app9)
# Coefficients:
#           ar1      ar2     ma1      ma2     sma1
#       -1.0277  -0.4712  0.1993  -0.2387  -0.6844
# s.e.   0.1423   0.1038  0.1534   0.1885   0.0796

tsdisplay(Arima.natl.app9$residuals) # ACF: 6 spikes; PACF: 4 spikes
checkresiduals(Arima.natl.app9) # p-value = 4.157e-06
res.natl.app9 <- NULL
for (i in 1:20) {
  res.natl.app9[i] <- Box.test(na.omit(Arima.natl.app9$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(res.natl.app9, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not good when lag > 10

# Arima(2,1,2)(0,1,2)[12]
Arima.natl.app10 <- arima(natl.hl.app, order = c(2,1,2), 
                         seasonal = list(order=c(0,1,2), period=12)) # AIC = 2668.63
print(Arima.natl.app10)
# Coefficients:
#           ar1      ar2     ma1      ma2     sma1     sma2
#       -1.0359  -0.3642  0.2396  -0.3586  -0.5049  -0.3029
# s.e.   0.1619   0.1067  0.1692   0.1684   0.0807   0.0820

tsdisplay(Arima.natl.app10$residuals) # ACF: 4 spikes; PACF: 3 spikes
checkresiduals(Arima.natl.app10) # p-value = 0.002156
res.natl.app10 <- NULL
for (i in 1:20) {
  res.natl.app10[i] <- Box.test(na.omit(Arima.natl.app10$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(res.natl.app10, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not very good when lag > 13

# Arima(2,1,2)(1,1,1)[12]
Arima.natl.app11 <- arima(natl.hl.app, order = c(2,1,2), 
                          seasonal = list(order=c(1,1,1), period=12)) # AIC = 2670.17
print(Arima.natl.app11)
# Coefficients:
#           ar1      ar2     ma1      ma2    sar1     sma1
#       -1.0324  -0.3706  0.2240  -0.3636  0.3873  -0.9358
# s.e.   0.1542   0.1076  0.1611   0.1674  0.1132   0.1444

tsdisplay(Arima.natl.app11$residuals) # ACF: 3 spikes; PACF: 3 spikes
checkresiduals(Arima.natl.app11) # p-value = 0.0003732
res.natl.app11 <- NULL
for (i in 1:20) {
  res.natl.app11[i] <- Box.test(na.omit(Arima.natl.app11$residuals),lag = i, 
                                type = 'Ljung-Box')$p.value
}
plot(res.natl.app11, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not good when lag > 12

# Arima(1,1,2)(0,1,2)[12]
Arima.natl.app12 <- arima(natl.hl.app, order = c(1,1,2), 
                          seasonal = list(order=c(0,1,2), period=12)) # AIC = 2691.39
print(Arima.natl.app12)
# Coefficients:
#           ar1      ma1     ma2     sma1     sma2
#       -0.0518  -0.7642  0.1207  -0.5023  -0.3455
# s.e.   0.3024   0.2962  0.2089   0.0834   0.0812

tsdisplay(Arima.natl.app12$residuals) # ACF: 7 spikes; PACF: 3 spikes
checkresiduals(Arima.natl.app12) # p-value = 2.807e-07
res.natl.app12 <- NULL
for (i in 1:20) {
  res.natl.app12[i] <- Box.test(na.omit(Arima.natl.app12$residuals),lag = i, 
                                type = 'Ljung-Box')$p.value
}
plot(res.natl.app12, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not good when lag > 3

# Arima(2,1,3)(0,1,1)[12]
Arima.natl.app13 <- arima(natl.hl.app, order = c(2,1,3), 
                          seasonal = list(order=c(0,1,1), period=12)) # AIC = 2639.57
print(Arima.natl.app13)
# Coefficients:
#           ar1      ar2     ma1     ma2      ma3     sma1
#       -1.1574  -0.9978  0.4870  0.2919  -0.6325  -0.6232
# s.e.   0.0047   0.0026  0.0592  0.0666   0.0585   0.0780

tsdisplay(Arima.natl.app13$residuals) # ACF: 1 spikes; PACF: 1 spikes
checkresiduals(Arima.natl.app13) # p-value = 0.02265
res.natl.app13 <- NULL
for (i in 1:20) {
  res.natl.app13[i] <- Box.test(na.omit(Arima.natl.app13$residuals),lag = i, 
                                type = 'Ljung-Box')$p.value
}
plot(res.natl.app13, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # good
ts.plot(natl.hl.app)
lines(fitted(Arima.natl.app13),col='red')
accuracy(Arima.natl.app13)
#                     ME     RMSE      MAE        MPE     MAPE      MASE        ACF1
# Training set -12.44684 217.4173 162.9222 -0.1720616 4.261302 0.3482124 -0.04096962
pred13 <- forecast(Arima.natl.app13, h = 12)
autoplot(pred13, xlab = 'Time', ylab = 'NAtional application')

# Arima(2,1,3)(0,1,2)[12]
Arima.natl.app14 <- arima(natl.hl.app, order = c(2,1,3), 
                          seasonal = list(order=c(0,1,2), period=12)) # AIC = 2636.47
print(Arima.natl.app14)
# Coefficients:
#           ar1      ar2     ma1     ma2      ma3     sma1     sma2
#       -1.1568  -0.9972  0.4869  0.2901  -0.6336  -0.5265  -0.1794
# s.e.   0.0052   0.0032  0.0581  0.0656   0.0576   0.0787   0.0801

tsdisplay(Arima.natl.app14$residuals) # ACF: 0 spikes; PACF: 0 spikes
checkresiduals(Arima.natl.app14) # p-value = 0.364
res.natl.app14 <- NULL
for (i in 1:20) {
  res.natl.app14[i] <- Box.test(na.omit(Arima.natl.app14$residuals),lag = i, 
                                type = 'Ljung-Box')$p.value
}
plot(res.natl.app14, type = 'b', ylim = c(0, 1), ylab='Ljung-Box test p-values')
abline(h = 0.05) # good
ts.plot(natl.hl.app)
lines(fitted(Arima.natl.app14),col='red')
accuracy(Arima.natl.app14)
#                    ME     RMSE      MAE        MPE     MAPE      MASE        ACF1
# Training set -15.7918 214.1478 161.2002 -0.2608363 4.215399 0.3445319 -0.03403412
pred14 <- forecast(Arima.natl.app14, h = 12)
autoplot(pred14, xlab = 'Time', ylab = 'NAtional application')

# Arima(2,1,3)(1,1,1)[12]
Arima.natl.app15 <- arima(natl.hl.app, order = c(2,1,3), 
                          seasonal = list(order=c(1,1,1), period=12)) # AIC = 2636.72
print(Arima.natl.app15)
# Coefficients:
#           ar1      ar2     ma1     ma2      ma3    sar1     sma1
#       -1.1566  -0.9972  0.4824  0.2860  -0.6375  0.2712  -0.8058
# s.e.   0.0053   0.0033  0.0583  0.0657   0.0577  0.1263   0.0971

tsdisplay(Arima.natl.app15$residuals) # ACF: 0 spikes; PACF: 0 spikes
checkresiduals(Arima.natl.app15) # p-value = 0.3182
res.natl.app15 <- NULL
for (i in 1:20) {
  res.natl.app15[i] <- Box.test(na.omit(Arima.natl.app15$residuals),lag = i, 
                                type = 'Ljung-Box')$p.value
}
plot(res.natl.app15, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # good

# Arima(1,1,3)(0,1,2)[12]
Arima.natl.app16 <- arima(natl.hl.app, order = c(1,1,3), 
                          seasonal = list(order=c(0,1,2), period=12)) # AIC = 2671.92
print(Arima.natl.app16)
# Coefficients:
#           ar1      ma1      ma2     ma3     sma1     sma2
#       -0.7546  -0.0152  -0.5469  0.1875  -0.4896  -0.3310
# s.e.   0.1272   0.1353   0.0925  0.0644   0.0821   0.0795

tsdisplay(Arima.natl.app16$residuals) # ACF: 8 spikes; PACF: 3 spikes
checkresiduals(Arima.natl.app16) # p-value = 5.505e-05
res.natl.app16 <- NULL
for (i in 1:20) {
  res.natl.app16[i] <- Box.test(na.omit(Arima.natl.app16$residuals),lag = i, 
                                type = 'Ljung-Box')$p.value
}
plot(res.natl.app16, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not good when lag > 8

# select Arima(2,1,3)(0,1,2)[12]; Arima(2,1,3)(1,1,1)[12]; Arima(2,1,3)(0,1,1)[12];
# Arima(2,1,2)(0,1,2)[12]; Arima(2,1,0)(0,1,2)[12]; Arima(2,1,1)(0,1,2)[12];
# Arima(2,1,0)(1,1,1)[12]; Arima(2,1,1)(1,1,1)[12]

# Fit the model using data 2002-2016; predict 2017-2019, calculate RMSE
rmse <- function(y,f) {
  sqrt(mean((y-f)^2))
}
train.set <- ts(natl.hl.app[1:189], start = c(2002,4),end = c(2017,12), frequency = 12)

# Arima(2,1,3)(0,1,2)[12]
Arima.natl.app14.1 <- arima(train.set, order = c(2,1,3), 
                            seasonal = list(order=c(0,1,2), period=12))
print(Arima.natl.app14.1)
pred14.1 <- forecast(Arima.natl.app14.1, 15)
autoplot(pred14.1,xlab='Time',ylab='National application')
rmse(natl.hl.app[190:204], pred14.1$mean) # 214.3625

# Arima(2,1,3)(1,1,1)[12]
Arima.natl.app15.1 <- arima(train.set, order = c(2,1,3), 
                            seasonal = list(order=c(1,1,1), period=12))
print(Arima.natl.app15.1)
pred15.1 <-  forecast(Arima.natl.app15.1, 15)
autoplot(pred15.1,xlab='Time',ylab='National application')
rmse(natl.hl.app[190:204], pred15.1$mean) # 223.0082

# Arima(2,1,3)(0,1,1)[12]
Arima.natl.app13.1 <- arima(train.set, order = c(2,1,3), 
                            seasonal = list(order=c(0,1,1), period=12))
print(Arima.natl.app13.1)
pred13.1 <- forecast(Arima.natl.app13.1, 15)
autoplot(pred13.1,xlab='Time',ylab='National application')
rmse(natl.hl.app[190:204], pred13.1$mean) # 205.6526

# Arima(2,1,2)(0,1,2)[12]
Arima.natl.app10.1 <- arima(train.set, order = c(2,1,2), 
                            seasonal = list(order=c(0,1,2), period=12))
print(Arima.natl.app10.1)
pred10.1 <- forecast(Arima.natl.app10.1, 15)
autoplot(pred10.1,xlab='Time',ylab='National application')
rmse(natl.hl.app[190:204], pred10.1$mean) # 215.7522

# Arima(2,1,0)(0,1,2)[12]
Arima.natl.app2.1 <- arima(train.set, order = c(2,1,0), 
                            seasonal = list(order=c(0,1,2), period=12))
print(Arima.natl.app2.1)
pred2.1 <- forecast(Arima.natl.app2.1, 15)
autoplot(pred2.1,xlab='Time',ylab='National application')
rmse(natl.hl.app[190:204], pred2.1$mean) # 226.6404

# Arima(2,1,1)(0,1,2)[12]
Arima.natl.app6.1 <- arima(train.set, order = c(2,1,1), 
                           seasonal = list(order=c(0,1,2), period=12))
print(Arima.natl.app6.1)
pred6.1 <-  forecast(Arima.natl.app6.1, 15)
autoplot(pred6.1,xlab='Time',ylab='National application')
rmse(natl.hl.app[190:204], pred6.1$mean) # 231.0967

# Arima(2,1,0)(1,1,1)[12]
Arima.natl.app3.1 <- arima(train.set, order = c(2,1,0), 
                           seasonal = list(order=c(1,1,1), period=12))
print(Arima.natl.app3.1)
pred3.1 <- forecast(Arima.natl.app3.1, 15)
autoplot(pred3.1,xlab='Time',ylab='National application')
rmse(natl.hl.app[190:204], pred3.1$mean) # 236.7149

# Arima(2,1,1)(1,1,1)[12]
Arima.natl.app7.1 <- arima(train.set, order = c(2,1,1), 
                           seasonal = list(order=c(1,1,1), period=12))
print(Arima.natl.app7.1)
pred7.1 <-  forecast(Arima.natl.app7.1, 15)
autoplot(pred7.1,xlab='Time',ylab='National application')
rmse(natl.hl.app[190:204], pred7.1$mean) # 244.2696


# d=0 and D=1
# Arima(3,0,0)(1,1,0)[12]
Natl.app.arima1 <- arima(natl.hl.app, order = c(3,0,0), 
                          seasonal = list(order=c(1,1,0), period=12)) # AIC = 2711.6
print(Natl.app.arima1)
# Coefficients:
#          ar1     ar2     ar3     sar1
#       0.1336  0.3062  0.4181  -0.2399
# s.e.  0.0657  0.0622  0.0683   0.0779

tsdisplay(Natl.app.arima1$residuals) # ACF: 7 spikes; PACF: 5 spikes
checkresiduals(Natl.app.arima1) # p-value = 1.804e-06
Natl.app.res1 <- NULL
for (i in 1:20) {
  Natl.app.res1[i] <- Box.test(na.omit(Natl.app.arima1$residuals),lag = i, 
                                type = 'Ljung-Box')$p.value
}
plot(Natl.app.res1, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not good when lag > 8

# Arima(3,0,1)(1,1,0)[12]
Natl.app.arima2 <- arima(natl.hl.app, order = c(3,0,1), 
                         seasonal = list(order=c(1,1,0), period=12)) # AIC = 2710.85
print(Natl.app.arima2)
# Coefficients:
#           ar1     ar2     ar3     ma1     sar1
#       -0.0938  0.3792  0.5172  0.2803  -0.2446
# s.e.   0.1261  0.0695  0.0728  0.1443   0.0774

tsdisplay(Natl.app.arima2$residuals) # ACF: 6 spikes; PACF: 4 spikes
checkresiduals(Natl.app.arima2) # p-value = 4.224e-06
Natl.app.res2 <- NULL
for (i in 1:20) {
  Natl.app.res2[i] <- Box.test(na.omit(Natl.app.arima2$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(Natl.app.res2, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not good when lag > 8

# Arima(3,0,1)(1,1,1)[12]
Natl.app.arima3 <- arima(natl.hl.app, order = c(3,0,1), 
                         seasonal = list(order=c(1,1,1), period=12)) # AIC = 2684.87
print(Natl.app.arima3)
# Coefficients:
#          ar1     ar2     ar3     ma1    sar1     sma1
#       0.0215  0.4184  0.5105  0.2147  0.3506  -0.8865
# s.e.  0.1518  0.0850  0.0871  0.1767  0.1107   0.0983

tsdisplay(Natl.app.arima3$residuals) # ACF: 2 spikes; PACF: 2 spikes
checkresiduals(Natl.app.arima3) # p-value = 0.003454
Natl.app.res3 <- NULL
for (i in 1:20) {
  Natl.app.res3[i] <- Box.test(na.omit(Natl.app.arima3$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(Natl.app.res3, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not very good when lag > 8


# Arima(3,0,1)(2,1,0)[12]
Natl.app.arima4 <- arima(natl.hl.app, order = c(3,0,1), 
                         seasonal = list(order=c(2,1,0), period=12)) # AIC = 2702.44
print(Natl.app.arima4)
# Coefficients:
#           ar1     ar2     ar3     ma1     sar1     sar2
#       -0.0480  0.3872  0.5274  0.2366  -0.3213  -0.2539
# s.e.   0.1261  0.0707  0.0763  0.1460   0.0795   0.0763

tsdisplay(Natl.app.arima4$residuals) # ACF: 4 spikes; PACF: 2 spikes
checkresiduals(Natl.app.arima4) # p-value = 0.0002043
Natl.app.res4 <- NULL
for (i in 1:20) {
  Natl.app.res4[i] <- Box.test(na.omit(Natl.app.arima4$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(Natl.app.res4, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not very good when lag > 8

# Arima(3,0,1)(0,1,2)[12]
Natl.app.arima5 <- arima(natl.hl.app, order = c(3,0,1), 
                         seasonal = list(order=c(0,1,2), period=12)) # AIC = 2683.42
print(Natl.app.arima5)
# Coefficients:
#          ar1     ar2     ar3     ma1     sma1     sma2
#       0.0344  0.4178  0.4967  0.2072  -0.5032  -0.2764
# s.e.  0.1624  0.0893  0.0923  0.1880   0.0809   0.0817

tsdisplay(Natl.app.arima5$residuals) # ACF: 2 spikes; PACF: 2 spikes
checkresiduals(Natl.app.arima5) # p-value = 0.009512
Natl.app.res5 <- NULL
for (i in 1:20) {
  Natl.app.res5[i] <- Box.test(na.omit(Natl.app.arima5$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(Natl.app.res5, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not very good when lag > 8 (around 0.05)

# Arima(3,0,2)(0,1,2)[12]
Natl.app.arima6 <- arima(natl.hl.app, order = c(3,0,2), 
                         seasonal = list(order=c(0,1,2), period=12)) # AIC = 2682.36
print(Natl.app.arima6)
# Coefficients:
#           ar1     ar2     ar3     ma1      ma2     sma1     sma2
#       -0.0467  0.6596  0.3613  0.2419  -0.3546  -0.5047  -0.3059
# s.e.   0.1639  0.1691  0.1063  0.1713   0.1721   0.0818   0.0826

tsdisplay(Natl.app.arima6$residuals) # ACF: 2 spikes; PACF: 2 spikes
checkresiduals(Natl.app.arima6) # p-value = 0.001531
Natl.app.res6 <- NULL
for (i in 1:20) {
  Natl.app.res6[i] <- Box.test(na.omit(Natl.app.arima6$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(Natl.app.res6, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not very good when lag > 12 (around 0.05)

# Arima(3,0,3)(0,1,2)[12]
Natl.app.arima7 <- arima(natl.hl.app, order = c(3,0,3), 
                         seasonal = list(order=c(0,1,2), period=12)) # AIC = 2650.41
print(Natl.app.arima7)
# Coefficients:
#           ar1     ar2     ar3     ma1     ma2      ma3     sma1     sma2
#       -0.1685  0.1460  0.9853  0.4908  0.2950  -0.6293  -0.5255  -0.1846
# s.e.   0.0116  0.0127  0.0110  0.0594  0.0676   0.0592   0.0791   0.0801

tsdisplay(Natl.app.arima7$residuals) # ACF: 0 spikes; PACF: 0 spikes
checkresiduals(Natl.app.arima7) # p-value = 0.2728
accuracy(Natl.app.arima7)
#                     ME     RMSE      MAE        MPE     MAPE      MASE        ACF1
# Training set -13.22538 213.7546 161.4125 -0.3134545 4.189413 0.3449857 -0.02894595
qqnorm(Natl.app.arima7$residuals)
qqline(Natl.app.arima7$residuals)
Natl.app.res7 <- NULL
for (i in 1:20) {
  Natl.app.res7[i] <- Box.test(na.omit(Natl.app.arima7$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(Natl.app.res7, type = 'b', ylim = c(0, 1), ylab = 'Ljung-Box test p-values')
abline(h = 0.05) # good
pred7 <- forecast(Natl.app.arima7, 24)
plot(pred7,xlab='Time',ylab='National applications')
lines(fitted(Natl.app.arima7),col='red')
legend('topright',cex=0.6, legend=c('Raw data','Fitted series','Forecast'),
       col=c('black','red','blue'),lty = 1,lwd=2)

Natl.app.arima7.1 <- arima(train.set, order = c(3,0,3), 
                           seasonal = list(order=c(0,1,2), period=12))
print(Natl.app.arima7.1)
pred7.1 <- forecast(Natl.app.arima7.1, 15)
autoplot(pred7.1,xlab='Time',ylab='National application')
rmse(natl.hl.app[190:204], pred7.1$mean)  # 224.3869


# Arima(3,0,3)(2,1,0)[12]
Natl.app.arima8 <- arima(natl.hl.app, order = c(3,0,3), 
                         seasonal = list(order=c(2,1,0), period=12)) # AIC = 2661.6
print(Natl.app.arima8)
# Coefficients:
#           ar1     ar2     ar3     ma1     ma2      ma3     sar1     sar2
#       -0.1808  0.1349  0.9738  0.4945  0.3239  -0.6113  -0.4084  -0.2664
# s.e.   0.0172  0.0194  0.0166  0.0674  0.0765   0.0671   0.0749   0.0753

tsdisplay(Natl.app.arima8$residuals) # ACF: 2 spikes; PACF: 1 spikes
checkresiduals(Natl.app.arima8) # p-value = 0.02052
Natl.app.res8 <- NULL
for (i in 1:20) {
  Natl.app.res8[i] <- Box.test(na.omit(Natl.app.arima8$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(Natl.app.res8, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # good
accuracy(Natl.app.arima8)
#                     ME     RMSE      MAE        MPE     MAPE      MASE        ACF1
# Training set -10.84836 221.6412 166.4251 -0.2853876 4.286193 0.3556991 -0.03074023

Natl.app.arima8.1 <- arima(train.set, order = c(3,0,3), 
                           seasonal = list(order=c(2,1,0), period=12))
print(Natl.app.arima8.1)
pred8.1 <- forecast(Natl.app.arima8.1, 15)
autoplot(pred8.1,xlab='Time',ylab='National application')
rmse(natl.hl.app[190:204], pred8.1$mean) # 190.5363
ts.plot(natl.hl.app)
lines(fitted(Natl.app.arima8),col='red')
pred8 <- forecast(Natl.app.arima8, h=12)
autoplot(pred8, xlab = 'Time', ylab = 'National application')

# Arima(3,0,3)(1,1,1)[12]
Natl.app.arima9 <- arima(natl.hl.app, order = c(3,0,3), 
                         seasonal = list(order=c(1,1,1), period=12)) # AIC = 2650.56
print(Natl.app.arima9)
# Coefficients:
#           ar1     ar2     ar3     ma1     ma2      ma3    sar1     sma1
#       -0.1676  0.1465  0.9861  0.4850  0.2888  -0.6350  0.2842  -0.8166
# s.e.   0.0118  0.0128  0.0109  0.0598  0.0677   0.0594  0.1291   0.1000

tsdisplay(Natl.app.arima9$residuals) # ACF: 0 spikes; PACF: 0 spikes
checkresiduals(Natl.app.arima9) # p-value = 0.2426
Natl.app.res9 <- NULL
for (i in 1:20) {
  Natl.app.res9[i] <- Box.test(na.omit(Natl.app.arima9$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(Natl.app.res9, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # good
accuracy(Natl.app.arima9)
#                     ME     RMSE      MAE        MPE     MAPE      MASE        ACF1
# Training set -13.54908 213.5949 161.7132 -0.3306099 4.195217 0.3456283 -0.02677534

Natl.app.arima9.1 <- arima(train.set, order = c(3,0,3), 
                           seasonal = list(order=c(1,1,1), period=12))
print(Natl.app.arima9.1)
pred9.1 <- forecast(Natl.app.arima9.1, 15)
autoplot(pred9.1,xlab='Time',ylab='National application')
rmse(natl.hl.app[190:204], pred9.1$mean) # 191.7471
ts.plot(natl.hl.app)
lines(fitted(Natl.app.arima9),col='red')
pred9 <- forecast(Natl.app.arima9, h=12)
autoplot(pred9, xlab = 'Time', ylab = 'National application')



## Assessment rate (assessment/application)------------------------------------
# weight = 1/4
adj.app <- natl.hl.app
for (i in 2:length(national[,1])) {
  adj.app[i] <- (1/4) * natl.hl.app[i-1] + (3/4) * natl.hl.app[i]
}
adj.app
assess.rate <- natl.hl.ass/adj.app
ggtsdisplay(assess.rate)

# weight = 1/3
adj.app1 <- natl.hl.app
for (i in 2:length(national[,1])) {
  adj.app1[i] <- (1/3) * natl.hl.app[i-1] + (2/3) * natl.hl.app[i]
}
adj.app1
assess.rate1 <- natl.hl.ass/adj.app1
ggtsdisplay(assess.rate1)

# plot two series of assess.rates
cbind('Ass.rate (w = 1/4)' = assess.rate,
      'Ass.rate (w = 1/3)' = assess.rate1) %>%
  autoplot(facets=TRUE) + xlab('Time') + ylab('')

# Normality test
shapiro.test(assess.rate)  
# W = 0.99365, p-value = 0.5333 > 0.1, cannot reject Normality
shapiro.test(assess.rate1) 
# W = 0.9946, p-value = 0.6771 > 0.1, cannot reject Normality

test.assess <- data.frame(ass.ra = c(assess.rate,assess.rate1),
                          category = c(rep('A',length(assess.rate)),
                                       rep('B',length(assess.rate1))))
head(test.assess)

# Homogeneity test of variances
bartlett.test(formula=ass.ra~category,data=test.assess)
# p-value = 0.9909 > 0.1, cannot reject the null hypothesis: 
# Homogeneity test of variances

# t-test
t.test(formula = ass.ra ~ category, data = test.assess, paired = FALSE)
#       Welch Two Sample t-test

# data:  ass.ra by category
# t = 0.10849, df = 406, p-value = 0.9137
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.009677017  0.010807506
# sample estimates:
# mean in group A mean in group B 
#       0.7733272       0.7727619 

# p-value = 0.9137 > 0.1, cannot reject the null hypothesis
# assess.rate and assess.rate1 are similar


assess.rate.decompose <- decompose(assess.rate)
plot(assess.rate.decompose)

# Differencing
assess.rate %>% diff() %>% ggtsdisplay()
assess.rate %>% diff(lag=12) %>% ggtsdisplay()
assess.rate %>% diff() %>% diff(lag=12) %>% ggtsdisplay()

# Augmnented Dickey-Fuller Test P-value < 0.05, stationary
adf.test(assess.rate) # p-value = 0.1437
assess.rate %>% diff() %>% adf.test() # p-value < 0.01
assess.rate %>% diff(lag=12) %>% adf.test() # p-value < 0.01

auto.arima(assess.rate, trace = TRUE) # ARIMA(0,1,1)(2,0,0)[12]

assess.rate.arima1 <- Arima(assess.rate, order = c(0,1,1), 
                         seasonal = list(order=c(2,0,0), period=12))
checkresiduals(assess.rate.arima1) # p-value = 0.0005158
res.assess <- NULL
for (i in 1:20) {
  res.assess[i] <- Box.test(na.omit(assess.rate.arima1$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(res.assess, type = 'b', ylim = c(0, 1))
abline(h = 0.05)

assess.rate.arima1$coef
#     ma1    sar1    sar2
# -0.8792  0.3685  0.2442

assess.rate.arima2 <- Arima(assess.rate, order = c(2,0,0), 
                           seasonal = list(order=c(1,1,1), period=12), include.drift = TRUE)
checkresiduals(assess.rate.arima2) # p-value = 9.385e-08

assess.rate.arima2.1 <- Arima(assess.rate, order = c(2,0,1), 
                            seasonal = list(order=c(1,1,1), period=12), include.drift = TRUE)
checkresiduals(assess.rate.arima2.1) # p-value = 0.0007191

assess.rate.arima2.2 <- Arima(assess.rate, order = c(2,0,2), 
                              seasonal = list(order=c(0,1,1), period=12), include.drift = TRUE)
checkresiduals(assess.rate.arima2.2) # p-value = 0.0001658

assess.rate.arima2.3 <- Arima(assess.rate, order = c(2,0,3), 
                              seasonal = list(order=c(0,1,1), period=12))
checkresiduals(assess.rate.arima2.3) # p-value = 0.0004569

assess.rate.arima2.4 <- Arima(assess.rate, order = c(0,0,3), 
                              seasonal = list(order=c(0,1,1), period=12))
checkresiduals(assess.rate.arima2.4) # p-value = 4.74e-08


ggtsdisplay(assess.rate1)
assess.rate.decompose1 <- decompose(assess.rate1)
plot(assess.rate.decompose1)
# Differencing
assess.rate1 %>% diff() %>% ggtsdisplay()
assess.rate1 %>% diff(lag=12) %>% ggtsdisplay()
assess.rate1 %>% diff() %>% diff(lag=12) %>% ggtsdisplay()

# Augmnented Dickey-Fuller Test P-value < 0.05, stationary
adf.test(assess.rate1) # p-value = 0.1312
assess.rate1 %>% diff() %>% adf.test() # p-value < 0.01
assess.rate1 %>% diff(lag=12) %>% adf.test() # p-value < 0.01

train.set.assess1 <- ts(assess.rate1[1:189], start = c(2002,4),
                        end = c(2017,12), frequency = 12)

auto.arima(assess.rate1,stepwise = FALSE,approximation = FALSE,trace = TRUE)
# # ARIMA(1,1,1)(1,0,1)[12]

auto.arima(assess.rate1, trace = TRUE) # ARIMA(3,1,2)(1,0,1)[12]
# Coefficients:
#           ar1     ar2     ar3      ma1      ma2    sar1     sma1
#       -0.3468  0.0563  0.2197  -0.6792  -0.1737  0.8780  -0.6343
# s.e.   0.2510  0.1110  0.0841   0.2484   0.2017  0.0683   0.1180
assess.rate1.arima <- arima(assess.rate1, order = c(3,1,2), 
                                seasonal = list(order=c(1,0,1), period=12)) # aic = -826.35
checkresiduals(assess.rate1.arima) # p-value = 0.003689
res.assess1 <- NULL
for (i in 1:20) {
  res.assess1[i] <- Box.test(na.omit(assess.rate1.arima$residuals),lag = i, 
                            type = 'Ljung-Box')$p.value
}
plot(res.assess1, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # OK

pred.assess1 <- forecast(assess.rate1.arima, 24)
plot(pred.assess1, xlab = 'Time', ylab='Assessment rates')
lines(fitted(assess.rate1.arima), col='red')
legend('topright',cex=0.6, legend=c('Original','Fitted','Forecast'),
       col=c('black','red','blue'),lty = 1,lwd=2)

# rough predicted assessment numbers in the next two years
rough.assessment  <- pred.assess1$mean * pred7$mean
rough.assessment <- c(2505.354, 2448.114, 2381.981, 2497.343, 2590.823, 2244.818,
                      2674.477, 2178.340, 1555.908, 2496.782, 2306.471, 2583.090, 2395.761, 
                      2322.075, 2435.962, 2405.449, 2462.528, 2310.143, 2511.526, 2098.854,
                      1604.898, 2393.968, 2283.415, 2650.779)
assessment.pred <- c(national$Homeless.assess, rough.assessment)
length(assessment.pred)
assessment.pred <- ts(assessment.pred,start = c(2002,4),end=c(2021,3), frequency = 12)
plot(assessment.pred, ylab = 'National assessments')

assess.rate1.arima1 <- arima(train.set.assess1 , order = c(3,1,2), 
                            seasonal = list(order=c(1,0,1), period=12))
print(assess.rate1.arima1)
pred.assess <- forecast(assess.rate1.arima1, 15)
autoplot(pred.assess,xlab='Time',ylab='Assessment rates')
rmse(assess.rate1[190:204], pred.assess$mean) # 0.03402092

assess.rate1.arima.1 <- arima(assess.rate1, order = c(3,1,2), 
                            seasonal = list(order=c(1,0,0), period=12)) # aic = -816.84
checkresiduals(assess.rate1.arima.1) # p-value = 0.006995
res.assess1.1 <- NULL
for (i in 1:20) {
  res.assess1.1[i] <- Box.test(na.omit(assess.rate1.arima.1$residuals),lag = i, 
                             type = 'Ljung-Box')$p.value
}
plot(res.assess1.1, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # OK

assess.rate1.arima.1.1 <- arima(train.set.assess1, order = c(3,1,2), 
                             seasonal = list(order=c(1,0,0), period=12))
print(assess.rate1.arima.1.1)
pred.assess.1 <- forecast(assess.rate1.arima.1.1, 15)
autoplot(pred.assess.1,xlab='Time',ylab='Assessment rates')
rmse(assess.rate1[190:204], pred.assess.1$mean) # 0.03232903

assess.rate1.arima.2 <- arima(assess.rate1, order = c(3,1,2), 
                              seasonal = list(order=c(2,0,0), period=12)) # aic = -820.72
checkresiduals(assess.rate1.arima.2) # p-value = 0.001321
res.assess1.2 <- NULL
for (i in 1:20) {
  res.assess1.2[i] <- Box.test(na.omit(assess.rate1.arima.2$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(res.assess1.2, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not very good when lags > 16

assess.rate1.arima.2.1 <- arima(train.set.assess1, order = c(3,1,2), 
                                seasonal = list(order=c(2,0,0), period=12))
print(assess.rate1.arima.2.1)
pred.assess.2 <- forecast(assess.rate1.arima.2.1, 15)
autoplot(pred.assess.2,xlab='Time',ylab='Assessment rates')
rmse(assess.rate1[190:204], pred.assess.2$mean) # 0.0315841

assess.rate1.arima.3 <- arima(assess.rate1, order = c(3,1,0), 
                              seasonal = list(order=c(2,0,0), period=12)) # aic = -804.44
checkresiduals(assess.rate1.arima.3) # p-value = 2.375e-05
res.assess1.3 <- NULL
for (i in 1:20) {
  res.assess1.3[i] <- Box.test(na.omit(assess.rate1.arima.3$residuals),lag = i, 
                               type = 'Ljung-Box')$p.value
}
plot(res.assess1.3, type = 'b', ylim = c(0, 1))
abline(h = 0.05) # not good

assess.rate1.arima.3.1 <- arima(train.set.assess1, order = c(3,1,2), 
                                seasonal = list(order=c(2,0,0), period=12))
print(assess.rate1.arima.3.1)
pred.assess.3 <- forecast(assess.rate1.arima.3.1, 15)
autoplot(pred.assess.3,xlab='Time',ylab='Assessment rates')
rmse(assess.rate1[190:204], pred.assess.2$mean) # 0.0315841

assess.rate1.arima.4 <- arima(assess.rate1, order = c(3,1,2), 
                              seasonal = list(order=c(0,0,2), period=12)) # aic = -812.47
checkresiduals(assess.rate1.arima.4) # p-value = 0.002192

assess.rate1.arima.5 <- arima(assess.rate1, order = c(2,1,2), 
                              seasonal = list(order=c(1,0,1), period=12)) # aic = -823.47
checkresiduals(assess.rate1.arima.5) # p-value = 4.79e-05

assess.rate1.arima.6 <- arima(assess.rate1, order = c(1,1,1), 
                              seasonal = list(order=c(1,0,1), period=12)) # aic = -825.11
checkresiduals(assess.rate1.arima.6) # p-value = 6.397e-08
res.assess6 <- NULL
for (i in 1:20) {
  res.assess6[i] <- Box.test(na.omit(assess.rate1.arima.6$residuals),lag = i, 
                             type = 'Ljung-Box')$p.value
}
plot(res.assess6, type = 'b', ylim = c(0, 1))
abline(h = 0.05)

assess.rate1.arima.6.1 <- arima(train.set.assess1, order = c(1,1,1), 
                                seasonal = list(order=c(1,0,1), period=12))
print(assess.rate1.arima.6.1)
pred.assess.6 <- forecast(assess.rate1.arima.6.1, 15)
autoplot(pred.assess.6,xlab='Time',ylab='Assessment rates')
rmse(assess.rate1[190:204], pred.assess.6$mean) # 0.0325296

assess.rate1.arima.7 <- arima(assess.rate1, order = c(0,1,2), 
                              seasonal = list(order=c(1,0,1), period=12)) # aic = -824.57
checkresiduals(assess.rate1.arima.7) # p-value = 1.577e-07

assess.rate1.arima.8 <- arima(assess.rate1, order = c(2,1,1), 
                              seasonal = list(order=c(1,0,1), period=12)) # aic = -823.28
checkresiduals(assess.rate1.arima.8) # p-value = 2.432e-07


### Demographic breakdown------------------------------------------------------
# Correlations between subgroups
cor.natl <- cor(national[, 6:13], method = 'spearman', use = 'pairwise.complete.obs')
corrplot(corr = cor.natl,method='color',order = 'original',
         addCoef.col = 'grey')

Single.Male <- ts(national$Single.Male, start = c(2002,4), end = c(2019,3), frequency = 12)
Single.Female <- ts(national$Single.Female, start = c(2002,4), end = c(2019,3), frequency = 12)
Single.Male.Parent <- ts(national$Single.Male.Parent, start = c(2002,4), end = c(2019,3), frequency = 12)
Single.Female.Parent <- ts(national$Single.Female.Parent, start = c(2002,4), end = c(2019,3), frequency = 12)
Couple.Only <- ts(national$Couple.Only, start = c(2002,4), end = c(2019,3), frequency = 12)
Couple.and.Children <- ts(national$Couple.and.Children, start = c(2002,4), end = c(2019,3), frequency = 12)
Other <- ts(national$Other, start = c(2002,4), end = c(2019,3), frequency = 12)
Other.and.Children <- ts(national$Other.and.Children, start = c(2002,4), end = c(2019,3), frequency = 12)

app.all <- c(sum(Single.Male),sum(Single.Female),sum(Single.Male.Parent),sum(Single.Female.Parent),
sum(Couple.Only),sum(Couple.and.Children),sum(Other),sum(Other.and.Children))
# 349155 169725  38504 147315  35584  40565  17039  15304
per.app <- paste(round(100*app.all/sum(app.all),2),'%')
slice.col <- rainbow(10)
subnames <- c('Single.Male','Single.Female','Single.Male.Parent','Single.Female.Parent',
            'Couple.Only','Couple.and.Children','Other','Other.and.Children')
pie(app.all,labels = per.app,col=slice.col)
legend('topleft',subnames,cex=0.75,fill=slice.col)

# Plots of subgroups
plot(Single.Male, ylim = c(0,2500), ylab='Applications')
lines(Single.Female,col='red')
lines(Single.Male.Parent,col='green')
lines(Single.Female.Parent,col='blue')
lines(Couple.Only,col='yellow')
lines(Couple.and.Children,col='grey')
lines(Other,col='purple')
lines(Other.and.Children,col='light blue')
legend('topright',cex=0.6, legend=c('Single.Male','Single.Female','Single.Male.Parent','Single.Female.Parent',
                                    'Couple.Only','Couple.and.Children','Other','Other.and.Children'),
        col=c('black','red','green','blue','yellow','grey','purple','light blue'),lty = 1,lwd=2)

plot(Single.Male, ylim = c(400,2500),ylab='')
lines(Single.Female,col='red')
legend('topright',cex=0.6, legend=c('Single.Male','Single.Female'),
       col=c('black','red'),lty = 1,lwd=2)

plot(Single.Male.Parent,col='green',ylim = c(50,1200))
lines(Single.Female.Parent,col='blue')
legend('topright',cex=0.6, legend=c('Single.Male.Parent','Single.Female.Parent'),
       col=c('green','blue'),lty = 1,lwd=2)

plot(Couple.Only,col='yellow',ylim=c(0,400),ylab='')
lines(Couple.and.Children,col='grey')
lines(Other,col='purple')
lines(Other.and.Children,col='light blue')
legend('topright',cex=0.6, legend=c('Couple.Only','Couple.and.Children','Other','Other.and.Children'),
       col=c('yellow','grey','purple','light blue'),lty = 1,lwd=2)

## Linear regression
national$Month <- as.factor(national$Month)
str(Month)
national$period <- as.factor(c(rep('Early', 93), rep('Middle', 48), rep('Late', 63)))
T <- 1:204
Single.Male.fit <- lm(Single.Male ~ Month + period + period:Month + period:T, 
                      data = national)
summary(Single.Male.fit) # Adjusted R-squared:  0.8875
AIC(Single.Male.fit) # 2562.067
plot(Single.Male.fit)
ggtsdisplay(Single.Male.fit$residuals)
checkresiduals(Single.Male.fit)
ggtsdisplay(Single.Male.fit$residuals)
(fitted(Single.Male.fit))
plot(fitted(Single.Male.fit),col='red',type = 'l')


Single.Male.fit2 <- lm(Single.Male ~ Month + period + period:T, 
                      data = national)
summary(Single.Male.fit2) # Adjusted R-squared:  0.8924
AIC(Single.Male.fit2)  # 2534.452
plot(Single.Male.fit2)
plot(fitted(Single.Male.fit2),col='red',type = 'l')
ggtsdisplay(Single.Male.fit2$residuals)


Single.Female.fit <- lm(Single.Female ~ Month + period + period:Month + period:T, 
                      data = national)
summary(Single.Female.fit) # Adjusted R-squared:  0.0.9049
AIC(Single.Female.fit) # 2314.303
plot(Single.Female.fit)

Single.Female.fit2 <- lm(Single.Female ~ Month + period + period:T, 
                         data = national)
summary(Single.Female.fit2) # Adjusted R-squared:  0.9045
AIC(Single.Female.fit2) # 2296.685
plot(fitted(Single.Female.fit2),col='red',type = 'l')


Single.Male.Parent.fit <- lm(Single.Male.Parent ~ Month + period + period:Month + period:T, 
                        data = national)
summary(Single.Male.Parent.fit) # Adjusted R-squared:  0.9359
AIC(Single.Male.Parent.fit) # 1839.348
plot(Single.Male.Parent.fit)

Single.Male.Parent.fit2 <- lm(Single.Male.Parent ~ Month + period + period:T, 
                             data = national)
summary(Single.Male.Parent.fit2) # Adjusted R-squared:  0.938
AIC(Single.Male.Parent.fit2) # 1813.915
plot(Single.Male.Parent.fit2)


Single.Female.Parent.fit <- lm(Single.Female.Parent ~ Month + period + period:Month + period:T, 
                             data = national)
summary(Single.Female.Parent.fit) # Adjusted R-squared:  0.9251
AIC(Single.Female.Parent.fit) # 2295.883
plot(Single.Female.Parent.fit)

Single.Female.Parent.fit2 <- lm(Single.Female.Parent ~ Month + period + period:T, 
                               data = national)
summary(Single.Female.Parent.fit2) # Adjusted R-squared:  0.9183
AIC(Single.Female.Parent.fit2) # 2295.055
plot(Single.Female.Parent.fit2)


Couple.Only.fit <- lm(Couple.Only ~ Month + period + period:Month + period:T, 
                               data = national)
summary(Couple.Only.fit) # Adjusted R-squared:  0.8829
AIC(Couple.Only.fit) # 1880.885
plot(Couple.Only.fit)

Couple.Only.fit2 <- lm(Couple.Only ~ factor(Month) + period + period:T, 
                      data = national)
summary(Couple.Only.fit2) # Adjusted R-squared:  0.8778
AIC(Couple.Only.fit2) # 1871.207
plot(Couple.Only.fit2)


Couple.and.Children.fit <- lm(Couple.and.Children ~ Month + period + period:Month + period:T, 
                      data = national)
summary(Couple.and.Children.fit) # Adjusted R-squared:  0.8564
AIC(Couple.and.Children.fit) # 1906.22
plot(fitted(Couple.and.Children.fit),type = 'l')
plot(Couple.and.Children.fit)

Couple.and.Children.fit2 <- lm(Couple.and.Children ~ factor(Month) + period + period:T, 
                              data = national)
summary(Couple.and.Children.fit2) # Adjusted R-squared:  0.8528
AIC(Couple.and.Children.fit2) # 1892.75
plot(Couple.and.Children.fit2)


Other.fit <- lm(Other ~ factor(Month) + period + period:Month + period:T, 
                              data = national)
summary(Other.fit) # 0.6334
AIC(Other.fit) # 1658.197
plot(Other.fit)

Other.fit2 <- lm(Other ~ factor(Month) + period + period:T, 
                data = national)
summary(Other.fit2) # Adjusted R-squared:  0.6358
AIC(Other.fit2) # 1638.429
plot(Other.fit2)


Other.and.Children.fit <- lm(Other.and.Children ~ Month + period + period:Month + period:T, 
                data = national)
summary(Other.and.Children.fit) # Adjusted R-squared:  0.6621
plot(Other.and.Children.fit)

Other.and.Children.fit2 <- lm(Other.and.Children ~ Month + period + period:T, 
                             data = national)
summary(Other.and.Children.fit2) # Adjusted R-squared:  0.675
AIC(Other.and.Children.fit2) # 1610.068
plot(Other.and.Children.fit2)

### Council data---------------------------------------------------------------
table <- table(council$Council)
Council.names <- c(names(table))
pdf('council_applications1')
par(mfrow=c(2,2))
for (i in 1:4) {
  plot(ts(council$Applications[which(council$Council== Council.names[i])],
          start = c(2002,4),frequency = 12), ylab=Council.names[i])
}
dev.off()
pdf('council_applications2')
par(mfrow=c(2,2))
for (i in 5:8) {
  plot(ts(council$Applications[which(council$Council== Council.names[i])],
          start = c(2002,4),frequency = 12), ylab=Council.names[i])
}
dev.off()
pdf('council_applications3')
par(mfrow=c(2,2))
for (i in 9:12) {
  plot(ts(council$Applications[which(council$Council== Council.names[i])],
          start = c(2002,4),frequency = 12), ylab=Council.names[i])
}
dev.off()
pdf('council_applications4')
par(mfrow=c(2,2))
for (i in 13:16) {
  plot(ts(council$Applications[which(council$Council== Council.names[i])],
          start = c(2002,4),frequency = 12), ylab=Council.names[i])
}
dev.off()
pdf('council_applications5')
par(mfrow=c(2,2))
for (i in 17:20) {
  plot(ts(council$Applications[which(council$Council== Council.names[i])],
          start = c(2002,4),frequency = 12), ylab=Council.names[i])
}
dev.off()
pdf('council_applications6')
par(mfrow=c(2,2))
for (i in 21:24) {
  plot(ts(council$Applications[which(council$Council== Council.names[i])],
          start = c(2002,4),frequency = 12), ylab=Council.names[i])
}
dev.off()
pdf('council_applications7')
par(mfrow=c(2,2))
for (i in 25:28) {
  plot(ts(council$Applications[which(council$Council== Council.names[i])],
          start = c(2002,4),frequency = 12), ylab=Council.names[i])
}
dev.off()
pdf('council_applications8')
par(mfrow=c(2,2))
for (i in 29:32) {
  plot(ts(council$Applications[which(council$Council== Council.names[i])],
          start = c(2002,4),frequency = 12), ylab=Council.names[i])
}
dev.off()
par(mfrow=c(1,1))


# application rates: app per 10000 people
council$App.per10000 <- council$Applications / council$pop * 10000
summary(council$App.per10000)

# assessment rates: assess per 10 app
council.app.adj <- council$council.app.adj <- council$Applications
for (i in 1:32) {
  for (j in 2:204) {
    council$council.app.adj[which(council$Council==Council.names[i])][j] <- 
      1/3 * council$Applications[which(council$Council==Council.names[i])][j-1] +
      2/3 * council$Applications[which(council$Council==Council.names[i])][j]
  }
}

council.assess.rate <- council$ass.rate <- council$Assessments/council$council.app.adj
assess.per10app <- council$assess.per10app <- council.assess.rate * 10

# Box plots
length(council[,1]) # 6528
Label <- c(rep('App per 10000 people',6528),rep('Assess per 10 app',6528))
Double_council <- rep(council$Council, 2)
app_ass_value <- c(council$App.per10000,council$assess.per10app)
app_and_ass <- data.frame(Label,Double_council,app_ass_value)
head(app_and_ass)
require(ggplot2)
box <- ggplot(data=app_and_ass,aes(x=Double_council,y=app_ass_value))+geom_boxplot(aes(fill=Label))
box+theme(axis.text.x = element_text(angle = 90,hjust = 1))

## Cluster: council applications
council.app <- matrix(0,nrow = 32,ncol = 204)
for (i in 1:32) {
  council.app[i, ] <- c(council$App.per10000[which(council$Council == Council.names[i])])
}
council.app <- as.data.frame(council.app)
row.names(council.app) <- Council.names
head(council.app,3)
council.app.dist <- dist(scale(council.app))

# method='ward'
council.app.clust_ward <- hclust(council.app.dist,method = 'ward.D') # OK
plot(council.app.clust_ward,hang=-1,cex=0.8)
rect.hclust(council.app.clust_ward,k=4)
rect.hclust(council.app.clust_ward,k=6)

# method='ward.D2'
par(mar=c(4,4,4,4))
council.app.clust_ward2 <- hclust(council.app.dist,method = 'ward.D2') # OK
plot(council.app.clust_ward2,hang=-1,cex=0.8)
rect.hclust(council.app.clust_ward2,k=4)
rect.hclust(council.app.clust_ward2,k=6)
rect.hclust(council.app.clust_ward2,k=7)


app.cluster1 <- council$App.per10000[which(council$Council=='West Dunbartonshire')]
summary(app.cluster1); sd(na.omit(app.cluster1))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.        sd 
#  5.535  11.136  12.833  14.582  18.665  26.802  4.769952 
app.cluster2 <- c(council$App.per10000[which(council$Council=='Clackmannanshire')],
                  council$App.per10000[which(council$Council=='Glasgow City')])
summary(app.cluster2); sd(na.omit(app.cluster2))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.        sd 
#  3.891   8.616  11.858  12.432  15.762  25.427  4.555963  

app.cluster3 <- council$App.per10000[which(council$Council=='Dundee City')]
summary(app.cluster3) ; sd(na.omit(app.cluster3))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.        sd 
#  3.810   7.422   8.776   9.561  11.509  18.712  3.023163  

app.cluster4 <- c(council$App.per10000[which(council$Council=='Angus')],
                  council$App.per10000[which(council$Council=='West Lothian')],
                  council$App.per10000[which(council$Council=='Edinburgh')],
                  council$App.per10000[which(council$Council=='Fife')],
                  council$App.per10000[which(council$Council=='Falkirk')],
                  council$App.per10000[which(council$Council=='Aberdeen City')],
                  council$App.per10000[which(council$Council=='East Lothian')])
summary(app.cluster4) ; sd(na.omit(app.cluster4))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.        sd 
#   2.642   5.889   7.590   7.920   9.602  18.266  2.568984

app.cluster5 <- c(council$App.per10000[which(council$Council=='East Renfrewshire')],
                  council$App.per10000[which(council$Council=='Inverclyde')],
                  council$App.per10000[which(council$Council=='Renfrewshire')],
                  council$App.per10000[which(council$Council=='Aberdeenshire')],
                  council$App.per10000[which(council$Council=='East Dunbartonshire')])
summary(app.cluster5) ; sd(na.omit(app.cluster5))
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.         sd
#  1.101   3.263   4.303   4.588   5.541  10.933  1.755169

app.cluster6 <- c(council$App.per10000[which(council$Council=='Orkney')],
                  council$App.per10000[which(council$Council=='North Ayrshire')],
                  council$App.per10000[which(council$Council=='East Ayrshire')],
                  council$App.per10000[which(council$Council=='Stirling')],
                  council$App.per10000[which(council$Council=='Dumfries & Galloway')],
                  council$App.per10000[which(council$Council=='North Lanarkshire')],
                  council$App.per10000[which(council$Council=='Argyll & Bute')],
                  council$App.per10000[which(council$Council=='Highland')])
summary(app.cluster6) ; sd(na.omit(app.cluster6))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.        sd
#  0.000   4.471   5.902   6.436   8.455  15.406  2.596751

app.cluster7 <- c(council$App.per10000[which(council$Council=='Shetland')],
                 council$App.per10000[which(council$Council=='Midlothian')],
                 council$App.per10000[which(council$Council=='Scottish Borders')],
                 council$App.per10000[which(council$Council=='Perth & Kinross')],
                 council$App.per10000[which(council$Council=='South Ayrshire')],
                 council$App.per10000[which(council$Council=='South Lanarkshire')],
                 council$App.per10000[which(council$Council=='Eilean Siar')],
                 council$App.per10000[which(council$Council=='Moray')])
summary(app.cluster7) ; sd(na.omit(app.cluster7))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.        sd 
#  1.711   4.842   6.105   6.297   7.483  37.633  2.188619


## Cluster: council assessment rates
council.ass <- matrix(0,nrow = 32,ncol = 204)
for (i in 1:32) {
  council.ass[i, ] <- c(council$assess.per10app[which(council$Council == Council.names[i])])
}
council.ass <- as.data.frame(council.ass)
row.names(council.ass) <- Council.names
head(council.ass,3)
council.ass.dist <- dist(scale(council.ass))

# method='ward.D2'
council.ass.clust_ward2 <- hclust(council.ass.dist,method = 'ward.D2') 
plot(council.ass.clust_ward2,hang=-1,cex=0.8)
rect.hclust(council.app.clust_ward2,k=3)
rect.hclust(council.app.clust_ward2,k=4)
rect.hclust(council.app.clust_ward2,k=6)
rect.hclust(council.app.clust_ward2,k=7)

ass.cluster1 <- c(council$assess.per10app[which(council$Council=='Orkney')])
summary(ass.cluster1) ; sd(na.omit(ass.cluster1))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.        sd 
#  0.000   5.477   7.000   7.404   8.889  30.000  3.023537

ass.cluster2 <- c(council$assess.per10app[which(council$Council=='Eilean Siar')],
                  council$assess.per10app[which(council$Council=='Shetland')])
summary(ass.cluster2) ; sd(na.omit(ass.cluster2))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.        sd
#  0.000   5.000   6.667   7.308   9.231  30.000  3.421564

ass.cluster3 <- c(council$assess.per10app[which(council$Council=='Edinburgh')])
summary(ass.cluster3) ; sd(na.omit(ass.cluster3))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.         sd
#  7.027   9.159   9.545   9.520   9.921  10.952  0.6423264

ass.cluster4 <- c(council$assess.per10app[which(council$Council=='Inverclyde')],
                  council$assess.per10app[which(council$Council=='Dundee City')],
                  council$assess.per10app[which(council$Council=='Renfrewshire')],
                  council$assess.per10app[which(council$Council=='Aberdeen City')],
                  council$assess.per10app[which(council$Council=='Moray')],
                  council$assess.per10app[which(council$Council=='Clackmannanshire')],
                  council$assess.per10app[which(council$Council=='Midlothian')])
summary(ass.cluster4); sd(na.omit(ass.cluster4))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.       sd
# 1.250   5.854   7.200   7.196   8.491  16.154  1.952761

ass.cluster5 <- c(council$assess.per10app[which(council$Council=='South Ayrshire')],
                  council$assess.per10app[which(council$Council=='Stirling')],
                  council$assess.per10app[which(council$Council=='West Dunbartonshire')],
                  council$assess.per10app[which(council$Council=='East Dunbartonshire')],
                  council$assess.per10app[which(council$Council=='Scottish Borders')])
summary(ass.cluster5); sd(na.omit(ass.cluster5))
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.        sd
# 1.967   5.870   7.098   7.104   8.333  13.158  1.726087

ass.cluster6 <- c(council$assess.per10app[which(council$Council=='East Ayrshire')],
                  council$assess.per10app[which(council$Council=='Glasgow City')],
                  council$assess.per10app[which(council$Council=='West Lothian')],
                  council$assess.per10app[which(council$Council=='Highland')],
                  council$assess.per10app[which(council$Council=='Argyll & Bute')],
                  council$assess.per10app[which(council$Council=='Aberdeenshire')],
                  council$assess.per10app[which(council$Council=='Fife')],
                  council$assess.per10app[which(council$Council=='Dumfries & Galloway')])
summary(na.omit(ass.cluster6)); sd(na.omit(ass.cluster6))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.       sd
#  3.125   6.818   7.637   7.695   8.571  15.000  1.362031

ass.cluster7 <- c(council$assess.per10app[which(council$Council=='Falkirk')],
                  council$assess.per10app[which(council$Council=='North Lanarkshire')],
                  council$assess.per10app[which(council$Council=='South Lanarkshire')],
                  council$assess.per10app[which(council$Council=='Perth & Kinross')],
                  council$assess.per10app[which(council$Council=='East Lothian')],
                  council$assess.per10app[which(council$Council=='North Ayrshire')],
                  council$assess.per10app[which(council$Council=='Angus')],
                  council$assess.per10app[which(council$Council=='East Renfrewshire')])
summary(na.omit(ass.cluster7)); sd(na.omit(ass.cluster7))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.        sd
#  2.857   6.800   7.714   7.720   8.644  18.889  1.533091








