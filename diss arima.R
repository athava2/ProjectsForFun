##-coding for dissertation
##-install packages
install.packages('quantmod')
install.packages('forecast')
install.packages('ggplot2')
install.packages('quadprog')
install.packages('knitr')
install.packages('e1071')
install.packages('kernlab')

##-run packages---
library(quantmod)
library(kernlab)
library(ggplot2)
library(knitr)
library(e1071)
library(forecast)
library(quadprog)

##-import data into R---
netflix = getSymbols("NFLX", auto.assign=F,
			from = "2014-01-01", to = "2019-01-01")
nfxreal =  getSymbols("NFLX", auto.assign=F,
			from = "2014-01-01", to = "2019-04-30")

##-export data into .csv file and read---
write.csv(netflix, "netflix.csv")
dataset = read.csv('netflix.csv')

##-apply svm model---
regressor = svm(formula = netflix$NFLX.Open ~ .,
                data = dataset,
                type = 'eps-regression', probability = TRUE);regressor

##-apply prediction---
y_pred = predict(regressor, dataset)

##-plot test data---
original = as.ts(nfxreal$NFLX.Open)
plot(as.ts(netflix$NFLX.Open))
chartSeries(netflix, type = "line")
ggtsdisplay(netflix$NFLX.Open)

##-apply ARIMA models---
nfxarima = auto.arima(netflix$NFLX.Open, 
                           stepwise = T, 
                           approximation = F, 
                           trace = T); nfxarima
nfxarima = auto.arima(ts(netflix$NFLX.Open, stepwise = T, approximation = F, 
                           trace = T); nfxarima

##-view ARIMA model---
bestnfxarima = Arima(netflix$NFLX.Open, order = c(1,1,0))
nfxarima2 = Arima(netflix$NFLX.Open, order = c(1,1,1))

##-plot ARIMA model---
plot(forecast(nfxarima, h = 119)
arimpred=forecast(auto.arima(ts(netflix$NFLX.Open, frequency=365)), h=119)
plot(forecast(auto.arima(ts(netflix$NFLX.Open, frequency=365) ,D=1), h=119),
xlab = "Time in Years",
ylab = "Opening Price in $",
main = "ARIMA Forecast Model")

##-apply ETS model---
nfxets = ets(netflix$NFLX.Open)

##-plot ETS model---
etspred = forecast(nfxets, model = ets, h = 119)
plot(forecast(nfxets, model = ets, h = 119))

##-plot model of real data next to models---
par(mfrow = c(2, 2))
plot(original, ylim = c(0, 500),
xlab = "Time in Days",
ylab = "Opening Price in $",
main = "Current Opening Price of Netflix, Inc.")

plot(forecast(auto.arima(ts(netflix$NFLX.Open, frequency=365) ,D=1), h=119),
xaxt = 'n',
ylab = "Opening Price in $",
main = "ARIMA Forecast Model",
ylim = c(0, 500))

plot(forecast(ts(y_pred, frequency = 365), h = 119), 
xaxt = 'n',
ylab = "Opening Price in $",
main = "SVR Model",
ylim = c(0, 500))
	
plot(forecast(nfxets, model = ets, h = 119),
xaxt = 'n',
ylab = "Opening Price in $",
main = "ETS Model",
ylim = c(0,500))

netflix = as.data.frame(netflix)
netflix$Date = rownames(netflix)
netflix$Date = as.Date(netflix$Date)
head(netflix)
mydates = seq.Date(from = as.Date("2015-01-01"), 
                to = as.Date("2019-01-01"), 
                by = 1)

mydates = data.frame(Date = mydates)
mydata = merge(netflix, mydates, by = "Date", all.y = T)

mydata = mydata[5:366,]
mydata = mydata[-(seq(from = 7, to = nrow(mydata), by = 7)),]
mydata = mydata[-(seq(from = 6, to = nrow(mydata), by = 6)),]
mydata = na.locf(mydata)


highestprice = ts(as.numeric(mydata$NFLX.High), 
                    frequency = 5)

par(mfrow = c(2,1))
lowestprice = ts(as.numeric(mydata$NFLX.Low), 
                 frequency = 5)
monthplot(lowestprice, base = median, col.base = "red")
monthplot(highestprice, base = median, col.base = "red")
par(mfrow = c(1,1))

