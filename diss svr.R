install.packages('kernlab')
install.packages('ggplot2')
install.packages('knitr')
install.packages('quantmod')
install.packages('e1071')

library(kernlab)
library(ggplot2)
library(knitr)
library(quantmod)
library(e1071)

write.csv(netflix, "netflix.csv")
dataset = read.csv('netflix.csv')
regressor = svm(formula = netflix$NFLX.Open ~ .,
                data = dataset,
                type = 'eps-regression', probability = TRUE);regressor

y_pred = predict(regressor, dataset)
ggplot() +
  geom_point(aes(x = dataset$X, y = netflix$NFLX.Open),
             colour = 'red') +
  geom_line(aes(x = dataset$X, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('SVR Model') +
  xlab('Day') +
  ylab('Opening Price')

plot(forecast(auto.arima(ts(y_pred,frequency=365),D=1),h=365))
plot(forecast(ts(y_pred, frequency = 365), h = 119))	



x_grid = seq(min(dataset$X), max(dataset$X), 1)

ggplot() +
  geom_point(aes(x = netflix$X, y = netflix$NFLX.Open),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('SVR Model') +
  xlab('Day') +
  ylab('Opening Price')





mainWD <- "C:/Users/athav/Documents/Uni/SVM_NFLX"
dirRawData <- "C:/Users/athav/Documents/Uni/SVM_NFLX/RawData"
dirRdata <- "C:/Users/athav/Documents/Uni/SVM_NFLX/RData"
dirROutput <- "C:/Users/athav/Documents/Uni/SVM_NFLX/GraphicOutput"
dirText <- "C:/Users/athav/Documents/Uni/SVM_NFLX/TextOutput"
nflx.df <- read.csv("netflix.csv", na.strings=c("",".","NULL"))
flowdata <- as.matrix(nflx.df)
