library(R6)
library(forecast)
library(ggplot2)

TimeSeries <- R6Class(
  "TimeSeries",

  public = list(
    initialize = function(list, start, end, frequency) {
      private$dataset = ts(list, start, end, frequency)
    },

    boxTest = function(type = "Ljung-Box") {
      test <- Box.test(private$dataset, type = type)

      if (test$p.value < 0.05) {
        print("This time series is stationary.")
      } else {
        print("This time series is not stationary.")
      }

      print(test)
    },

    fit = function() {
      private$modelArima <- auto.arima(private$dataset)
      private$modelExponential <- ets(private$dataset)
      private$modelBats <- bats(private$dataset)
      private$modelTBats <- tbats(private$dataset)

      # nnetar() -> Neural Network Time Series Forecasts
      private$modelRecurrentNeuralNetwork <- nnetar(
        private$dataset,
        repeats = 1000,
        lambda = BoxCox.lambda(private$dataset)
      )

      private$datasetMAPE <- c(
        arima = accuracy(private$modelArima)[,'MAPE'],
        exponential = accuracy(private$modelExponential)[,'MAPE'],
        bats = accuracy(private$modelBats)[,'MAPE'],
        tbats = accuracy(private$modelTBats)[,'MAPE'],
        neural = accuracy(private$modelRecurrentNeuralNetwork)[,'MAPE']
      )
    },

    forecast = function(period) {
      min <- which.min(private$datasetMAPE)

      model <- switch(
        names(min),
        arima = private$modelArima,
        exponential = private$modelExponential,
        bats = private$modelBats,
        tbats = private$modelTBats,
        neural = private$modelRecurrentNeuralNetwork
      )

      private$forecastResult <- forecast(model, period)

      summary(private$forecastResult);
    },

    plot = function(title, xLabel, yLabel) {
      autoplot(
        private$dataset,
        main = title,
        xlab = xLabel,
        ylab = yLabel
      )
    },

    plotMAPE = function(title, yLabel, col = "light blue") {
      barplot(
        private$datasetMAPE,
        main = title,
        col = col,
        ylab = yLabel
      )
    },

    plotForecast = function(title, xLabel, yLabel) {
      autoplot(
        private$forecastResult,
        main = title,
        xlab = xLabel,
        ylab = yLabel
      )
    }
  ),

  private = list(
    dataset = NULL,

    modelArima = NULL,
    modelExponential = NULL,
    modelBats = NULL,
    modelTBats = NULL,
    modelRecurrentNeuralNetwork = NULL,

    # Mean Absolute Percentage Error
    datasetMAPE = NULL,

    forecastResult = NULL
  )
)

# reading dataset
dataset = read.table('sales.csv', header=TRUE, sep=";")

# converting the entire column 'sale' to numeric
dataset$sale <- as.numeric(dataset$sale)

# creating a new instance of TimeSeries
timeSeries <- TimeSeries$new(dataset$sale, c(2010, 1), c(2016, 12), 12)

# ljung-box 
# p-value < 0.05 = serie is stationary
timeSeries$boxTest()

timeSeries$plot("Time Series - Product Sales", "Period", "Sale")

# arima, ets, bats, tbats, nnetar
timeSeries$fit()

# plot accuracy of the models 
timeSeries$plotMAPE("Mean Absolute Percentage Error (MAPE)", "MAPE")

# forecasting of 6 periods
timeSeries$forecast(6)

# plot the forecast result
timeSeries$plotForecast("Product Sales Forecast", "Period", "Sale")