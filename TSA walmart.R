#TSA ASSIGNMENT

#Walmart data Weekly Sales Forecast

#Install and load Packages
library(fpp2)
library(forecast)
library(readxl)
install.packages("lubridate")
library(lubridate)
library(dplyr)
library(tseries)

#Data processing
df <- read_excel(file.choose())
df
max(df$Date)
start.date  <- decimal_date(ymd(min(df$Date[3])))
df_ts<-ts(df$Weekly_Sales, start = start.date,frequency = 52)
df_ts
ts.plot(df_ts)
plot(decompose(df_ts))


#training data and testing data
train.df<-slice(df, 1:104)
test.df<-slice(df,105:143)
test.date <- decimal_date(ymd("2012-02-03"))
df_train<-ts(train.df$Weekly_Sales, start = start.date,frequency = 52)
df_test<- ts(test.df$Weekly_Sales, start =test.date, frequency = 52 )
length(df_train)
length(df_test)


#Models

#hw - triple exponential 
HW1 <- HoltWinters(df_train)
summary(HW1)
forecasthw<-forecast(HW1, h =39)
#predict
HW1.pred <- predict(HW1, 39, prediction.interval = TRUE, level=0.95)
#Visually evaluate the fits
plot(df_ts, ylab="Weekly Sales", xlim=c(2010,2013))
lines(forecasthw$fitted, lty=2, col="blue")
lines(HW1.pred[,1], col="red")
#Accuracy
accuracy(forecasthw)


#test for staionarity
st<-kpss.test(df_train)
st$p.value    #no staionarity 
#diffrencing
dts1<-diff(df_train)
ts.plot(dts1)
#testing again for staionarity
st1<-kpss.test(dts1) # staionarity achieved
st1$p.value

#ACF and PACF
Acf(df_train)
Pacf(df_train)

#Auto Arima
aa_m<-auto.arima(df_train)
autoplot(forecast(aa_m))
model_forecast<-forecast(aa_m, level =  c(95), h =39)
model_forecast
plot(model_forecast)

prd<-forecast(aa_m, 39)
plot(prd)

prdd<-as.numeric(prd$mean)
#MAPE
og<-as.numeric(df_test)
mape<-mean(abs((og-prdd)/og))*100
mape
#accuracy
accuracy(aa_m)

#Moving Average
#MA
a<-Arima(df_train,order = c(0,1,2))
ma_m<-df_ts-a$residuals
#visualize
plot(df_train)
lines(ma_m, lty   = 2, col  = 3 )
#accuracy
accuracy(a)