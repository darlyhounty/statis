library(stabledist)

## SOURCE("fBasics.13A-StableDistribution")

## rsymstb -
" xmpBasics("\nStart: Symmetric Stable Distribuion:  > ")
par(mfcol = c(3, 2), cex = 0.7)
set.seed(1953)
r = rsymstb(n = 1000, alpha = 1.6)
plot(r, type = "l", main = "symstb: alpha = 1.6")
# Plot empirical density and compare with true density:
hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue4")
x = seq(-5, 5, 0.1)
lines(x, dsymstb(x = x, alpha = 1.6))
# Plot df and compare with true df:
plot(sort(r), (1:1000/1000), main = "Probability", col = "steelblue4")
lines(x, psymstb(x, alpha = 1.6))
# Compute quantiles:
qsymstb(psymstb(q = seq(-10, 10, 1), alpha = 1.6), alpha = 1.6)
"
set.seed(1953)
r = rstable(n = 1000, alpha = 1.6, beta = 0.3)
plot(r, type = "l", main = "stable: alpha=1.6 beta=0.3")
# Plot empirical density and compare with true density:
hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue4")
x = seq(-5, 5, 0.4)
lines(x, dstable(x = x, alpha = 1.6, beta = 0.3))
# Plot df and compare with true df:
plot(sort(r), (1:1000/1000), main = "Probability", col = "steelblue4")
lines(x, pstable(q = x, alpha = 1.6, beta = 0.3))
# Compute quantiles:
qstable(pstable(seq(-4, 4, 1), alpha = 1.6, beta = 0.3), 
        alpha = 1.6, beta = 0.3)

set.seed(1953)
r1 = rstable(n = 1000, alpha = 1.6, beta = 0.5)
plot(r1, type = "l", main = "stable: alpha=1.6 beta=0.5")
# Plot empirical density and compare with true density:
hist(r1, n = 25, probability = TRUE, border = "white", col = "steelblue4")
x = seq(-5, 5, 0.4)
lines(x, dstable(x = x, alpha = 1.6, beta = 0.5))
# Plot df and compare with true df:
plot(sort(r1), (1:1000/1000), main = "Probability", col = "steelblue4")
lines(x, pstable(q = x, alpha = 1.6, beta = 0.5))
# Compute quantiles:
qstable(pstable(seq(-4, 4, 1), alpha = 1.6, beta = 0.5), 
        alpha = 1.6, beta = 0.5)

set.seed(1953)
r2 = rstable(n = 1000, alpha = 1.6, beta = 0.7)
plot(r2, type = "l", main = "stable: alpha=1.6 beta=0.7")
# Plot empirical density and compare with true density:
hist(r2, n = 25, probability = TRUE, border = "white", col = "steelblue4")
x = seq(-5, 5, 0.4)
lines(x, dstable(x = x, alpha = 1.6, beta = 0.7))
# Plot df and compare with true df:
plot(sort(r2), (1:1000/1000), main = "Probability", col = "steelblue4")
lines(x, pstable(q = x, alpha = 1.6, beta = 0.7))
# Compute quantiles:
qstable(pstable(seq(-4, 4, 1), alpha = 1.6, beta = 0.7), 
        alpha = 1.6, beta = 0.7)
r1
r1_n2<-jitter(r1,factor=2)
r1_n3<-jitter(r1,factor=3)
r1[0:100]
r1_n2[0:100]
r1_n5[0:100]
plot(r1_n2, type = "l", main = "stable: alpha=1.6 beta=0.3")
hist(r1_n2, n = 25, probability = TRUE, border = "white", col = "steelblue4")
x = seq(-5, 5, 0.4)
lines(x, dstable(x = x, alpha = 1.6, beta = 0.5))
plot(sort(r1_n2), (1:1000/1000), main = "Probability", col = "steelblue4")
lines(x, pstable(q = x, alpha = 1.6, beta = 0.5))
par(mfcol = c(2, 1), cex = 0.7)
plot(r1_n2, type = "l", main = "stable: alpha=1.6 beta=0.3,factor=2")
plot(r1_n3, type = "l", main = "stable: alpha=1.6 beta=0.3,factor=3")

hist(r1_n2, n = 25, probability = TRUE, border = "white", col = "steelblue4")
x = seq(-5, 5, 0.4)
lines(x, dstable(x = x, alpha = 1.6, beta = 0.5))

hist(r1_n3, n = 25, probability = TRUE, border = "white", col = "steelblue4")
x = seq(-5, 5, 0.4)
lines(x, dstable(x = x, alpha = 1.6, beta = 0.5))

library(xts)
library(ggplot2)
library(dplyr)
library(forecast)
tim<-seq(as.Date('2017-01-01'),length=1000,by='day')
data_r1<-xts(r1_n,tim)
data_r1_990<-data_r1[0:990]

data_r1[990:1000]
plot(data_r1)

#arima_r1<-arima(diff(diff(data_r1_990)),order=c(0,2,1))
#checkresiduals(arima_r1)
ets_data<-ets(data_r1_990)
autoplot(forecast(ets_data))
mean(forecast(ets_data)$residuals^2)

auto.arima(data_r1_990)
arima_ts<-arima(data_r1_990, order = c(1,0,2), seasonal = c(order = c(0,1,1)))
autoplot(forecast(arima_ts))
mean(forecast(arima_ts)$residuals^2)

checkresiduals(ets_data)
checkresiduals(arima_ts)
S
t.test(forecast(arima_ts)$residuals,alternative = "two.sided")
t.test(forecast(ets_data)$residuals,alternative = "two.sided")
