library(dplyr)
library(tidyverse)
library(forecast)
library(writexl)

atm_raw <- "https://raw.githubusercontent.com/JDO-MSDS/Data-624/refs/heads/main/Project%201/data/ATM624Data%20-%20ATM%20Data.csv"

atm <- read.csv(atm_raw)

glimpse(atm)
summary(atm)

atm$DATE <- as.Date(atm$DATE, origin = "1899-12-30")

head(atm)
unique(atm$ATM)

atm <- atm |>
  filter(ATM != "")

unique(atm$ATM)

atm |> count(ATM)

atm_list <- split(atm, atm$ATM)

atm_list <- lapply(atm_list, function(df) {
  df %>%
    filter(!is.na(Cash)) %>%
    arrange(DATE)
})

atm_ts <- lapply(atm_list, function(df) {
  ts_data <- ts(df$Cash, frequency = 7)
  na.interp(ts_data)
})


par(mfrow = c(2,2)) 

for(name in names(atm_ts)) {
  plot(atm_ts[[name]],
       main = paste("Cash Withdrawals -", name),
       ylab = "Cash",
       xlab = "Time",
       col = "red")
}

par(mfrow = c(2,2))

for(name in names(atm_ts)) {
  acf(atm_ts[[name]],
      main = paste("ACF -", name))
}

par(mfrow = c(2,2))

for(name in names(atm_ts)) {
  pacf(atm_ts[[name]],
       main = paste("PACF -", name))
}


atm1_arima <- auto.arima(atm_ts$ATM1, seasonal = TRUE)
summary(atm1_arima)


checkresiduals(atm1_arima)

atm1_ets <- ets(atm_ts$ATM1)
summary(atm1_ets)


checkresiduals(atm1_ets)



atm1_arima_fc <- forecast(atm1_arima, h = 31)
atm1_ets_fc   <- forecast(atm1_ets, h = 31)


plot(atm1_arima_fc, main = "ATM1 ARIMA Forecast")
plot(atm1_ets_fc, main = "ATM1 ETS Forecast")


atm2_arima <- auto.arima(atm_ts$ATM2, seasonal = TRUE)
summary(atm2_arima)
```
The model select by `auto.arima()` for ATM2 is ARIMA(2,0,2)(0,1,2)[7] 


checkresiduals(atm2_arima)


atm2_ets <- ets(atm_ts$ATM2)
summary(atm2_ets)


checkresiduals(atm2_ets)


atm2_arima_fc <- forecast(atm2_arima, h = 31)
atm2_ets_fc   <- forecast(atm2_ets, h = 31)

plot(atm2_arima_fc, main = "ATM2 ARIMA Forecast")
plot(atm2_ets_fc, main = "ATM2 ETS Forecast")


atm1_train <- head(atm_ts$ATM1, length(atm_ts$ATM1) - 7)
atm1_test  <- tail(atm_ts$ATM1, 7)



atm2_train <- head(atm_ts$ATM2, length(atm_ts$ATM2) - 7)
atm2_test  <- tail(atm_ts$ATM2, 7)


atm1_arima_train <- auto.arima(atm1_train, seasonal = TRUE)
atm1_ets_train   <- ets(atm1_train)

atm1_arima_pred <- forecast(atm1_arima_train, h = 31)
atm1_ets_pred   <- forecast(atm1_ets_train, h = 31)


atm2_arima_train <- auto.arima(atm2_train, seasonal = TRUE)
atm2_ets_train   <- ets(atm2_train)

atm2_arima_pred <- forecast(atm2_arima_train, h = 31)
atm2_ets_pred   <- forecast(atm2_ets_train, h = 31)


accuracy(atm1_arima_pred, atm1_test)
accuracy(atm1_ets_pred, atm1_test)


accuracy(atm2_arima_pred, atm2_test)
accuracy(atm2_ets_pred, atm2_test)


atm3_mean <- meanf(atm_ts$ATM3, h = 31)
plot(atm3_mean, main = "ATM3 Mean Forecast")



atm4_log <- log(atm_ts$ATM4 + 1)
plot(atm4_log, main = "ATM4 Log-Transformed")



atm4_arima <- auto.arima(atm4_log, seasonal = TRUE)
summary(atm4_arima)
# residuals
checkresiduals(atm4_arima)


atm4_ets <- ets(atm4_log)
summary(atm4_ets)

checkresiduals(atm4_ets)


atm4_arima_fc <- forecast(atm4_arima, h = 31)
atm4_ets_fc   <- forecast(atm4_ets, h = 31)


atm4_arima_fc$mean  <- exp(atm4_arima_fc$mean) - 1
atm4_arima_fc$lower <- exp(atm4_arima_fc$lower) - 1
atm4_arima_fc$upper <- exp(atm4_arima_fc$upper) - 1
atm4_ets_fc$mean   <- exp(atm4_ets_fc$mean) - 1
atm4_ets_fc$lower <- exp(atm4_ets_fc$lower) - 1
atm4_ets_fc$upper <- exp(atm4_ets_fc$upper) - 1



plot(atm4_arima_fc, main = "ATM4 ARIMA Forecast")
plot(atm4_ets_fc, main = "ATM4 ETS Forecast")


atm4_arima <- auto.arima(atm_ts$ATM4, lambda = 0)
atm4_ets   <- ets(atm_ts$ATM4, lambda = 0)


atm4_arima_fc <- forecast(atm4_arima, h = 31)
atm4_ets_fc   <- forecast(atm4_ets, h = 31)


plot(atm4_arima_fc, main = "ATM4 ARIMA Forecast")
plot(atm4_ets_fc, main = "ATM4 ETS Forecast")

atm4_train <- head(atm_ts$ATM4, length(atm_ts$ATM4) - 7)
atm4_test  <- tail(atm_ts$ATM4, 7)



atm4_arima_train <- auto.arima(atm4_train, lambda = 0)
atm4_ets_train   <- ets(atm4_train, lambda = 0)


atm4_arima_pred <- forecast(atm4_arima_train, h = 31)
atm4_ets_pred   <- forecast(atm4_ets_train, h = 31)


accuracy(atm4_arima_pred, atm4_test)
accuracy(atm4_ets_pred, atm4_test)


atm1_final <- forecast(atm1_ets, h = 31)
atm2_final <- forecast(atm2_ets, h = 31)
atm3_final <- meanf(atm_ts$ATM3, h = 31)
atm4_final <- forecast(atm4_ets, h = 31)

may_dates <- seq(as.Date("2010-05-01"), as.Date("2010-05-31"), by = "day")

forecast_df <- data.frame(
  DATE = may_dates,
  ATM1 = as.numeric(atm1_final$mean),
  ATM2 = as.numeric(atm2_final$mean),
  ATM3 = as.numeric(atm3_final$mean),
  ATM4 = as.numeric(atm4_final$mean)
)



write_xlsx(forecast_df, "ATM_May_2010_Forecast.xlsx")


