#### Analyzing Coca Cola Earnings ####

#### 1. Loading the libraries ####

library(fBasics)
library(forecast) 

#### 2. Reading the data ####
data<-read.csv("coca_cola_earnings.csv",header=TRUE,sep=";",dec=",")
y<-data[,2] 

#### 3. Plotting the data and looking for stationnarity ####
par(mfrow=c(3,1))
ts.plot(y)
nlags=36   
acf(y,nlags) 
pacf(y,nlags)

## Quarterly data so we know s=4
s=4

## Now 2nd step, we identify the differences:

ndiffs(y, alpha=0.05, test=c("adf")) # regular differences? 

## So d=1

nsdiffs(y,m=s,test=c("ocsb"))  # seasonal differences? 

## D = 1

## Take d=1 and D=1
fit<-arima(y,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=s)) 
fit

#### 4.lotting data after taking the differences ####
ts.plot(fit$residuals)
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags) 

#### 5. Potential Models ####

arima(y,order=c(0,1,0),seasonal=list(order=c(1,1,0),period=s)) 
arima(y,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=s)) 
arima(y,order=c(0,1,0),seasonal=list(order=c(0,1,2),period=s)) 
arima(y,order=c(0,1,18),seasonal=list(order=c(0,1,0),period=s))


#### 6. Running the first model ####

fit<-arima(y,order=c(0,1,0),seasonal=list(order=c(1,1,0),period=s))
fit

ts.plot(fit$residuals) 
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)

Box.test(fit$residuals,lag=24)

## WN residuals and significant parameters so could be potential model


#### 7. A second model based on first ####
fit<-arima(y,order=c(2,1,0),seasonal=list(order=c(1,1,0),period=s))
fit

ts.plot(fit$residuals) 
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)

Box.test(fit$residuals,lag=24)

## WN residuals and on the limit significant parameters so could be potential model


#### 8. Trying 3rd model #####
fit<-arima(y,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=s))
fit

ts.plot(fit$residuals) 
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)

Box.test(fit$residuals,lag=24)
## WN not residuals so not a potential model


#### 8. Running 4th model based on 3rd #####
fit<-arima(y,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=s))
fit

ts.plot(fit$residuals) 
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)

Box.test(fit$residuals,lag=24)

## WN residuals and significant parameters so could be potential model

#### 9. Final model chosen ####
fit<-arima(y,order=c(0,1,0),seasonal=list(order=c(1,1,0),period=s))
fit<-arima(y,order=c(2,1,0),seasonal=list(order=c(1,1,0),period=s))
fit<-arima(y,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=s))

#### 10. Comparing all the models in terms of forecasting: RECURSIVE ####

#### 10.1 arima(y,order=c(0,1,0),seasonal=list(order=c(1,1,0),period=s)) ####
data<-read.csv("coca_cola_earnings.csv",header=TRUE,sep=";",dec=",")
y<-data[,2] 
n<-length(y)
n.estimation<-83 
n.forecasting<-n-n.estimation 
horizontes<-4 

predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
MSFE<-matrix(0,nrow=horizontes,ncol=1)
MAPE<-matrix(0,nrow=horizontes,ncol=1)

for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y[1:(n.estimation-Periods_ahead+i)]; #instead of starting at 1, I start at i because rolling
    fit<-arima(aux.y,order=c(0,1,0),seasonal=list(order=c(1,1,0),period=s)); 
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- y.pred$pred[Periods_ahead];
  }
  error<-real-predicc[,Periods_ahead];
  MSFE[Periods_ahead]<-mean(error^2);
  MAPE[Periods_ahead]<-mean(abs(error/real)) *100;
}
MSFE
MAPE

## Rolling Method:
predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
MSFE<-matrix(0,nrow=horizontes,ncol=1)
MAPE<-matrix(0,nrow=horizontes,ncol=1)

for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y[i:(n.estimation-Periods_ahead+i)]; #instead of starting at 1, I start at i because rolling
    fit<-arima(aux.y,order=c(0,1,0),seasonal=list(order=c(1,1,0),period=s)); 
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- y.pred$pred[Periods_ahead];
  }
  error<-real-predicc[,Periods_ahead];
  MSFE[Periods_ahead]<-mean(error^2);
  MAPE[Periods_ahead]<-mean(abs(error/real)) *100;
}
MSFE
MAPE


#### 10.2 arima(y,order=c(2,1,0),seasonal=list(order=c(1,1,0),period=s)) ####
data<-read.csv("coca_cola_earnings.csv",header=TRUE,sep=";",dec=",")
y<-data[,2] 
n<-length(y)
n.estimation<-83 
n.forecasting<-n-n.estimation 
horizontes<-4 

predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
MSFE<-matrix(0,nrow=horizontes,ncol=1)
MAPE<-matrix(0,nrow=horizontes,ncol=1)

for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y[1:(n.estimation-Periods_ahead+i)]; #instead of starting at 1, I start at i because rolling
    fit<-arima(aux.y,order=c(2,1,0),seasonal=list(order=c(1,1,0),period=s)); 
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- y.pred$pred[Periods_ahead];
  }
  error<-real-predicc[,Periods_ahead];
  MSFE[Periods_ahead]<-mean(error^2);
  MAPE[Periods_ahead]<-mean(abs(error/real)) *100;
}
MSFE
MAPE


#### 10.3 arima(y,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=s)) ####
data<-read.csv("coca_cola_earnings.csv",header=TRUE,sep=";",dec=",")
y<-data[,2] 
n<-length(y)
n.estimation<-83 
n.forecasting<-n-n.estimation 
horizontes<-4 

predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
MSFE<-matrix(0,nrow=horizontes,ncol=1)
MAPE<-matrix(0,nrow=horizontes,ncol=1)

for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y[1:(n.estimation-Periods_ahead+i)]; #instead of starting at 1, I start at i because rolling
    fit<-arima(aux.y,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=s)); 
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- y.pred$pred[Periods_ahead];
  }
  error<-real-predicc[,Periods_ahead];
  MSFE[Periods_ahead]<-mean(error^2);
  MAPE[Periods_ahead]<-mean(abs(error/real)) *100;
}
MSFE
MAPE

#### 11 Model with Logs ####
y<-log(y)

## Plotting the data and looking for stationnarity ##
par(mfrow=c(3,1))
ts.plot(y)
nlags=36   
acf(y,nlags) 
pacf(y,nlags)

## Quarterly data so we know s=4
s=4

## Now 2nd step, we identify the differences:

ndiffs(y, alpha=0.05, test=c("adf")) # regular differences? 

## So d=1

nsdiffs(y,m=s,test=c("ocsb"))  # seasonal differences? 

## D = 1

## Take d=1 and D=1
fit<-arima(y,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=s)) 
fit
ts.plot(fit$residuals) 
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)

Box.test(fit$residuals,lag=24)

## Potential Model 1##
fit<-arima(y,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=s))
fit
ts.plot(fit$residuals) 
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)
Box.test(fit$residuals,lag=24)

## Potential Model 2##
fit<-arima(y,order=c(1,1,0),seasonal=list(order=c(2,1,0),period=s))
fit
ts.plot(fit$residuals) 
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)
Box.test(fit$residuals,lag=24)


#### 12. Running the forecast for the log model ####

#### 12.1 arima(y,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=s)) ####
data<-read.csv("coca_cola_earnings.csv",header=TRUE,sep=";",dec=",")
y<-data[,2] 
n<-length(y)
n.estimation<-83 
n.forecasting<-n-n.estimation
horizontes<-4 

predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)]
MSFE<-matrix(0,nrow=horizontes,ncol=1)
MAPE<-matrix(0,nrow=horizontes,ncol=1)


for (Periods_ahead in 1:horizontes) { 
  for (i in 1:n.forecasting) { 
    aux.y<-y[1:(n.estimation-Periods_ahead+i)]; 
    fit<-arima(log(aux.y),order=c(1,1,0),seasonal=list(order=c(1,1,0),period=s)); 
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead]; 
  MSFE[Periods_ahead]<-mean(error^2);
  MAPE[Periods_ahead]<-mean(abs(error/real)) *100;
}
MSFE
MAPE

#### 12.1 arima(y,order=c(1,1,0),seasonal=list(order=c(2,1,0),period=s)) ####
data<-read.csv("coca_cola_earnings.csv",header=TRUE,sep=";",dec=",")
y<-data[,2] 
n<-length(y)
n.estimation<-83 
n.forecasting<-n-n.estimation
horizontes<-4 

predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)]
MSFE<-matrix(0,nrow=horizontes,ncol=1)
MAPE<-matrix(0,nrow=horizontes,ncol=1)


for (Periods_ahead in 1:horizontes) { 
  for (i in 1:n.forecasting) { 
    aux.y<-y[1:(n.estimation-Periods_ahead+i)]; 
    fit<-arima(log(aux.y),order=c(1,1,0),seasonal=list(order=c(2,1,0),period=s)); 
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- exp(y.pred$pred[Periods_ahead]);
  }
  error<-real-predicc[,Periods_ahead]; 
  MSFE[Periods_ahead]<-mean(error^2);
  MAPE[Periods_ahead]<-mean(abs(error/real)) *100;
}
MSFE
MAPE

