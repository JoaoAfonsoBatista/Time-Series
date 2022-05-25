###Projeto1 ST

library(readxl)
#read where the file is:
dataset_2 <- read_excel("2014-2019 PM10 LisAvLib.xlsx")

dataset <- rbind(dataset_2[c(1:43824),], c(NA, NA), dataset_2[c(43825:52583),])

library(imputeTS)

dataset_complete <- na_seasplit(as.numeric(unlist(dataset[,2])), find_frequency=TRUE)

dataset_complete <- cbind.data.frame(dataset$Data, dataset_complete)
names(dataset_complete)[2] <- "PM"

dataset_dia <- data.frame()
for (j in c(1:(length(dataset_complete[,2])/24))) {
  dia <- substr(dataset_complete[24*j,1], 0, 10)
  if(is.na(dia)){
    dia <- substr(dataset_complete[j*24 - 365*24,1], 0, 10)
    dia <- paste(paste(substr(dia,0,3), "9",sep = ""), substr(dia,5,10),sep = "")
  }
  
  valor <- 0
  for (i in c(1:24)) {
    valor <- valor + dataset_complete[24*(j-1)+i,2]
  }
  dataset_dia <- rbind(dataset_dia, c(dia, valor/24))
}

names(dataset_dia)[1] <- "DATA"

names(dataset_dia)[2] <- "PM"

dataset_dia$PM <- as.numeric(dataset_dia$PM)

dataset_dia2 <- dataset_dia

dataset_dia <- dataset_dia2[c(1:2186),]

####### 365 ################
tseriesx=ts(dataset_dia$PM, start=c(2014,1), end=c(2019,361), frequency=365)
y = stl(tseriesx,s.window = "period")
plot(y)

residuals=y$time.series[,3]
acf(residuals)
pacf(residuals)


library('forecast')
ndiffs(residuals)

auto.arima(residuals, d=0, max.p=5, max.q=5)

#(1,0,1)
library('TSA')
fit = arima(residuals, order = c(1, 0, 1), method = c("CSS"))
tsdiag(fit)

fit.residuals <- rstandard(fit)
Box.test(fit.residuals,lag=1, type="Ljung")

checkresiduals(fit.residuals)

library('snpar')
runs.test(fit.residuals)
shapiro.test(fit.residuals)

############ 7 ###########

#### seasonal, trend
tseriesx2 <- ts(dataset_dia$PM, start=c(1,1), end=c(313,2), frequency=7)
y2 <- stl(tseriesx2,s.window = "period")
plot(y2)


residuals2 <- y2$time.series[,3]
acf(residuals2)
pacf(residuals2)

ndiffs(residuals2)

auto.arima(residuals2, d=0, max.p=5, max.q=5)


# (4,0,2)(1,0,0)[7]
library('TSA')
fit2 = arima(residuals2, order = c(4, 0, 2), seasonal = list(order = c(1,0,0),period = 7), method = c("CSS"))
tsdiag(fit2)

fit2.residuals <- rstandard(fit2)


Box.test(fit2.residuals,lag=1, type="Ljung")


checkresiduals(fit2.residuals, lag.max = 730)
runs.test(fit2.residuals)
shapiro.test(fit2.residuals)


###########forecasting:##########

#the residuals
fore = forecast(residuals,h = 5, level = 0.95,model = fit) 
plot(fore, include = 100)

fore_res = fore$mean

#the seasonality
seasonality = y$time.series[,1]
plot(seasonality)
#(seasonality of the last year)
fore_seasonality = seasonality[1521:1525]

#every year, they are always the same:
seasonality[1521:1525]
seasonality[1156:1160]
seasonality[791:795]
seasonality[426:430]
seasonality[61:65]

#the trend
trend = y$time.series[,2]
plot(trend)
plot(trend[c(2085:2185)])
diff(trend[c(2085:2185)])
diff = -0.03396294
last = trend[2185]
fore_trend = c(last + diff, 
               last + 2 * diff,
               last + 3 * diff,
               last + 4 * diff,
               last + 5 * diff)

#adding all together:

forecast = fore_res + fore_seasonality + fore_trend

lower_intervals = forecast - (fore$mean - fore$lower)
upper_intervals = forecast + (fore$upper - fore$mean)


#the real
real = dataset_dia2[c(2186:2190),2]
x = real - forecast

sqrt(sum(x^2))

#the average decrease of the trend in 2019
sum(diff(trend[c(1831:2186)]))/365


