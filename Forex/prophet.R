setwd("C:/Users/ASUS/Desktop")
rm(list = ls())
pacman::p_load(pacman, rio, dplyr, ggplot2, magrittr, prophet)

df <- import(file = "EURUSD1440.csv")
dfH <- import(file = "EURUSD60.csv")
jml_awal <- length(dfH$V1) - length(df$V1) + 1
jml_akhir <- length(dfH$V1)
dfH1 <- dfH[jml_awal : jml_akhir, ]

data <- cbind.data.frame(ds = df$V1, y = dfH1$V2)
data$ds <- as.Date(data$ds)
m = prophet(data)
future = make_future_dataframe(m, periods = 20)
forecast = predict(m, future)


dyplot.prophet(m, forecast)