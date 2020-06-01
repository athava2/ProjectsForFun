##-coding for dissertation
install.packages('quantmod')
install.packages('forecast')
install.packages('ggplot2')
install.packages('quadprog')
library(quantmod)
netflix = getSymbols("NFLX", auto.assign=F,
			from = "2015-01-01", to = "2019-01-01")

plot(as.ts(netflix$NFLX.Open))
chartSeries(netflix, type = "line")

library(forecast)
library(ggplot2)
library(quadprog)

ggtsdisplay(netflix$NFLX.Open)

nfxarima = auto.arima(netflix$NFLX.Open, 
                           stepwise = T, 
                           approximation = F, 
                           trace = T); nfxarima
nfxarima2 = Arima(netflix$NFLX.Open, order = c(1,1,1))
plot(forecast(nfxarima, h = 20))

nfxets = ets(netflix$NFLX.Open)
plot(forecast(nfxets, h = 20))

netflix = as.data.frame(netflix)
netflix$Date = rownames(netflix)
netflix$Date = as.Date(netflix$Date)
head(novartis)
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

par(mfrow = c(1,2))
lowestprice = ts(as.numeric(mydata$NFLX.Low), 
                 frequency = 5)
monthplot(lowestprice, base = median, col.base = "red")
monthplot(highestprice, base = median, col.base = "red")
par(mfrow = c(1,1))

