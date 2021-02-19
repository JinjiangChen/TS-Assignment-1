bgd = ts(read.table('bgd.txt')$V2)
crime = ts(read.table('crime.txt')$V2)


plot(bgd, main = 'general circulation', ylab = 'quantity of cocaine')
plot(crime, main = 'crime', ylab = 'quantity of cocaine')
Box.test(bgd, type="Ljung-Box", lag=5)
Box.test(crime, type="Ljung-Box", lag=5)
qchisq(0.95, df = 5)

acf(bgd, main = "bgd")
pacf(bgd, main = "bgd")

acf(crime, main = "crime")
pacf(crime, main = "crime")

fit_bgd.ar2 = arima(x=bgd, order=c(2, 0, 0))
fit_crime.ar2 = arima(x=crime, order=c(2, 0, 0))
fit_bgd.ar2
fit_crime.ar2


res_bgd = fit_bgd.ar2$residuals
res_cri = fit_crime.ar2$residuals
acf(res_bgd, main = "bgd")
acf(res_cri, main = "crime")

Box.test(res_bgd, type="Ljung-Box", lag=3)
Box.test(res_cri, type="Ljung-Box", lag=3)
qchisq(0.95, df=3)

plot(res_bgd, main = "bgd")
plot(res_cri, main = "crime")
