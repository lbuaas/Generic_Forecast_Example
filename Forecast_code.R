# Generalized_Forecast_Project


#######################################
# The purpose of this code is to generalize
# an example of a forecasting code that I write 
# in my day to day role. 
#
# In this code we will forecast out daily phone
# orders, then use this as a variable to help
# forecast daily calls. This will help to create
# shifts for the call center.
#######################################

# Load Libraries
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(lubridate)) install.packages("lubridate")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(timetk)) install.packages("timetk")
if(!require(reshape2)) install.packages("reshape2")
if(!require(Boruta)) install.packages("Boruta")
if(!require(imputeTS)) install.packages("imputeTS")
if(!require(modeltime)) install.packages("modeltime")
if(!require(parsnip)) install.packages("parsnip")
if(!require(recipes)) install.packages("recipes")
if(!require(workflows)) install.packages("workflows")
if(!require(readxl)) install.packages("readxl")
if(!require(rsample)) install.packages("rsample")
if(!require(glmnet)) install.packages("glmnet")
if(!require(zoo)) install.packages("zoo")

# Load in Raw Data
df_calls <- read_xlsx("data/Calls_data.xlsx") %>%
  mutate(Japan_Date = ymd(Japan_Date),
         Day = format(Day, "%A")) %>%
  select(Japan_Date, Day, Number_of_Incoming_Calls) 

names(df_calls) <- c("Date", "Day", "Calls")

df_calls <- df_calls %>%
  mutate(Day = as.factor(Day))

df_promos <- read_xlsx("data/Promotion_forecast_data.xlsx", sheet = "Promos") %>%
  mutate(Date = ymd(Date),
         Promo_1 = as.integer(Promo_1),
         Promo_2 = as.integer(Promo_2),
         Promo_3 = as.integer(Promo_3),
         Promo_4 = as.integer(Promo_4),
         Promo_5 = as.integer(Promo_5),
         Promo_6 = as.integer(Promo_7),
         Promo_7 = as.integer(Promo_7),
         Promo_8 = as.integer(Promo_8),
         Promo_9 = as.integer(Promo_9),
         Promo_10 = as.integer(Promo_10),
         Promo_11 = as.integer(Promo_11),
         Promo_12 = as.integer(Promo_12)) %>%
  select(-Promo_12) 

###################################################################

# Combines data
df_calls <- df_calls %>%
  mutate(monthYear = format(Date, "%m-%Y")) %>%
  left_join(df_promos %>%
              mutate(monthYear = format(Date, "%m-%Y")) %>%
              select(-Date),
            by = "monthYear") %>%
  select(-monthYear) 


# Adds Holiday Column
df_calls_1 <- df_calls %>%
  filter(Date <= as.Date("2024-07-31")) %>%
  mutate(Holiday = ifelse(Calls == 0 , 1, 0)) 

holiday_2024 <- read_excel("data/Holiday_data.xlsx") %>%
  select(1,2)

df_calls_2 <- left_join(df_calls %>% filter(Date > as.Date("2024-07-31")),
                        holiday_2024, 
                        by = "Date") %>%
  mutate(Holiday = ifelse(Break == 1 | Day %in% c("Saturday","Sunday"), 1, 0)) %>%
  select(-Break)

df_calls <- rbind(df_calls_1,df_calls_2) %>%
  replace_na(list(Holiday = 0))

rm(df_calls_1,df_calls_2,holiday_2024)

###################################################################

# Orders Forecast #

# Loads Orders and changes entry date/time to Japanese time
df_orders <- rbind(
  read_csv("data/Orders_2020.csv") %>%
    mutate(Date_us = as_datetime(paste(entry_date, entry_time)),
           Date_Japan = as_datetime(Date_us + hours(15))) %>%
    mutate(Date_Japan = as_date(Date_Japan)) %>%
    filter(!(order_entry_init %in% c("*AS", "*WB"))) %>%
    group_by(Date_Japan) %>%
    summarise(order_count = n()),
  read_csv("data/Orders_2021.csv") %>%
    mutate(Date_us = as_datetime(paste(entry_date, entry_time)),
           Date_Japan = as_datetime(Date_us + hours(15))) %>%
    mutate(Date_Japan = as_date(Date_Japan)) %>%
    filter(!(order_entry_init %in% c("*AS", "*WB"))) %>%
    group_by(Date_Japan) %>%
    summarise(order_count = n()),
  read_csv("data/Orders_2022.csv") %>%
    mutate(Date_us = as_datetime(paste(entry_date, entry_time)),
           Date_Japan = as_datetime(Date_us + hours(15))) %>%
    mutate(Date_Japan = as_date(Date_Japan)) %>%
    filter(!(order_entry_init %in% c("*AS", "*WB"))) %>%
    group_by(Date_Japan) %>%
    summarise(order_count = n()),
  read_csv("data/Orders_2023.csv") %>%
    mutate(Date_us = as_datetime(paste(entry_date, entry_time)),
           Date_Japan = as_datetime(Date_us + hours(15))) %>%
    mutate(Date_Japan = as_date(Date_Japan)) %>%
    filter(!(order_entry_init %in% c("*AS", "*WB"))) %>%
    group_by(Date_Japan) %>%
    summarise(order_count = n()),
  read_csv("data/Orders_2024.csv") %>%
    mutate(Date_us = as_datetime(paste(entry_date, entry_time)),
           Date_Japan = as_datetime(Date_us + hours(15))) %>%
    mutate(Date_Japan = as_date(Date_Japan)) %>%
    filter(!(order_entry_init %in% c("*AS", "*WB"))) %>%
    group_by(Date_Japan) %>%
    summarise(order_count = n())
)

# Combines orders and promo data
df_orders <- left_join(df_calls %>% 
                         select(-Calls),
                       df_orders %>%
                         rename("Date" = "Date_Japan"),
                       by = "Date") %>%
  filter(Holiday == 0) %>%
  select(-Holiday) %>%
  filter(Date >= "2020-01-05")


df_calls <- df_calls %>% 
  filter(Holiday == 0) %>%
  select(-Holiday)

head(df_calls)


#######################################
# ORDERS FORECAST
#######################################

### Discovery ###

# Graphs orders over time
df_orders %>%
  filter(Date < Sys.Date()) %>%
  ggplot(aes(Date, order_count)) +
  geom_line() + 
  geom_point()+
  geom_smooth() +
  labs(title = "Calls - Japan",
       x = "",
       y = "") + 
  theme_classic()

# Looks at detrended data
# STL Decomposition
detrended_orders <- df_orders %>%
  drop_na()
stl_decomposition <- stl(ts(detrended_orders$order_count, frequency = 365.25), s.window = "periodic")
trend_component <- stl_decomposition$time.series[, "trend"]
detrended_orders$detrended_stl <- detrended_orders$order_count - trend_component

# Visualize
ggplot(detrended_orders, aes(Date, detrended_stl)) +
  geom_line() + 
  geom_point() +
  geom_smooth() +
  labs(title = "STL Detrended Calls - Japan",
       x = "",
       y = "") + 
  theme_classic()

# Plot ACF (Autocorrelation) and PACF (Partial Autocorrelation) with more lags
acf(detrended_orders$detrended_stl, lag.max = 730, xlab="Lag (years)")
pacf(detrended_orders$detrended_stl, lag.max = 730, xlab="Lag (years)")

rm(detrended_orders,stl_decomposition,trend_component)

# Looks at variable correlation
df_orders %>%
  drop_na() %>%
  select(-Date, -Day) %>%
  cor() %>%
  round(2)

# Creates splits to create initial time series models
lag_df_orders <- df_orders %>%
  arrange(Date) %>%
  filter(Date <= (Sys.Date() + days(90))) %>% # Only shows data 3 months from the date the code is ran
  mutate(Orders_lag1 = lag(order_count,1)) %>% # Adds one working day's worth of lag
  filter(Date > "2020-01-06") 

# Calculates weekly lag. If unavailable interpolates value of weekly lag
week_lag <- df_orders %>%
  select(Date,order_count) %>%
  mutate(next_week = Date + days(7)) %>%
  filter(next_week >= "2019-01-01") %>%
  select(next_week,order_count) %>%
  rename("Orders_lag_week" = "order_count")

lag_df_orders <- left_join(lag_df_orders,week_lag, by = c("Date" = "next_week")) %>%
  filter(Date > "2020-01-10") %>%
  mutate(Orders_lag_week = ifelse(Orders_lag_week == 0, NA, Orders_lag_week),
         Orders_lag_week = na.approx(Orders_lag_week, na.rm = FALSE)) 

# Creates data splits
Splits <- initial_time_split(lag_df_orders %>%
                               drop_na(), prop = 0.85)

Splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(Date, order_count,.interactive = TRUE)


### Modeling ###

# Creates reciepes and fits time series models
recipe_spec <- recipe(order_count ~ ., training(Splits)) %>%
  step_timeseries_signature(Date) %>%
  step_rm(matches("(hour)|(minute)|(second)|(am.pm)|(xts)|(iso)|(lbl)")) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) 

recipe_spec_2 <- recipe(order_count ~ ., training(Splits)) %>%
  step_timeseries_signature(Date) %>%
  step_fourier(Date, period = 12, K = 2) %>%
  step_rm(matches("(hour)|(minute)|(second)|(am.pm)|(xts)|(iso)|(lbl)")) %>%
  # step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) 

recipe_spec_3 <- recipe(order_count ~ ., training(Splits)) %>%
  step_timeseries_signature(Date) %>%
  step_fourier(Date, period = 12, K = 3) %>%
  step_rm(matches("(hour)|(minute)|(second)|(am.pm)|(xts)|(iso)|(lbl)")) %>%
  # step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

###########################
# Recipe 1
###########################

### ML Models
# Elastic Net
fit_glmnet <- linear_reg(penalty = 0.1, mixture = 0.5) %>%
  set_engine("glmnet")
fit_glmnet <- workflow() %>%
  add_model(fit_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(Date)) %>%
  fit(training(Splits))

# Random Forest
fit_rf<- workflow() %>%
  add_model(rand_forest("regression") %>% set_engine("ranger")) %>%
  add_recipe(recipe_spec %>% step_rm(Date)) %>%
  fit(training(Splits))

# Gradient Boost
fit_xgb <- workflow() %>%
  add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
  add_recipe(recipe_spec %>% step_rm(Date)) %>%
  fit(training(Splits))

# Prophet Boost Hybrid Model
fit_prophet_boost <- workflow() %>%
  add_model(prophet_boost("regression", seasonality_yearly = TRUE) %>% set_engine("prophet_xgboost")) %>%
  add_recipe(recipe_spec) %>%
  fit(training(Splits))

# Arima Boost
fit_arima_boosted <- workflow() %>%
  add_model(arima_boost("regression") %>% set_engine("auto_arima_xgboost")) %>%
  add_recipe(recipe_spec) %>%
  fit(training(Splits))

###########################
### Non ML Moels
# Exponential Smoothing
fit_ets <- workflow() %>%
  add_model(exp_smoothing() %>% set_engine("ets")) %>%
  add_recipe(recipe_spec) %>%
  fit(training(Splits))

# Prophet
fit_prophet <- workflow() %>%
  add_model(prophet_reg() %>% set_engine("prophet")) %>%
  add_recipe(recipe_spec) %>%
  fit(training(Splits))


###########################
# Recipe 2
###########################

### ML Models
# Elastic Net
fit_glmnet_2 <- linear_reg(penalty = 0.1, mixture = 0.5) %>%
  set_engine("glmnet")
fit_glmnet_2 <- workflow() %>%
  add_model(fit_glmnet_2) %>%
  add_recipe(recipe_spec_2 %>% step_rm(Date)) %>%
  fit(training(Splits))

# Random Forest
fit_rf_2<- workflow() %>%
  add_model(rand_forest("regression") %>% set_engine("ranger")) %>%
  add_recipe(recipe_spec_2 %>% step_rm(Date)) %>%
  fit(training(Splits))

# Gradient Boost
fit_xgb_2 <- workflow() %>%
  add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
  add_recipe(recipe_spec_2 %>% step_rm(Date)) %>%
  fit(training(Splits))

# Prophet Boost Hybrid Model
fit_prophet_boost_2 <- workflow() %>%
  add_model(prophet_boost("regression", seasonality_yearly = TRUE) %>% set_engine("prophet_xgboost")) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(Splits))

# Arima Boost
fit_arima_boosted_2 <- workflow() %>%
  add_model(arima_boost("regression") %>% set_engine("auto_arima_xgboost")) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(Splits))

###########################
### Non ML Moels
# Exponential Smoothing
fit_ets_2 <- workflow() %>%
  add_model(exp_smoothing() %>% set_engine("ets")) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(Splits))

# Prophet
fit_prophet_2 <- workflow() %>%
  add_model(prophet_reg() %>% set_engine("prophet")) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(Splits))


###########################
# Recipe 3
###########################

### ML Models
# Elastic Net
fit_glmnet_3 <- linear_reg(penalty = 0.1, mixture = 0.5) %>%
  set_engine("glmnet")
fit_glmnet_3 <- workflow() %>%
  add_model(fit_glmnet_3) %>%
  add_recipe(recipe_spec_3 %>% step_rm(Date)) %>%
  fit(training(Splits))

# Random Forest
fit_rf_3 <- workflow() %>%
  add_model(rand_forest("regression") %>% set_engine("ranger")) %>%
  add_recipe(recipe_spec_3 %>% step_rm(Date)) %>%
  fit(training(Splits))

# Gradient Boost
fit_xgb_3 <- workflow() %>%
  add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
  add_recipe(recipe_spec_3 %>% step_rm(Date)) %>%
  fit(training(Splits))

# Prophet Boost Hybrid Model
fit_prophet_boost_3 <- workflow() %>%
  add_model(prophet_boost("regression", seasonality_yearly = TRUE) %>% set_engine("prophet_xgboost")) %>%
  add_recipe(recipe_spec_3) %>%
  fit(training(Splits))

# Arima Boost
fit_arima_boosted_3 <- workflow() %>%
  add_model(arima_boost("regression") %>% set_engine("auto_arima_xgboost")) %>%
  add_recipe(recipe_spec_3) %>%
  fit(training(Splits))

###########################
### Non ML Moels
# Exponential Smoothing
fit_ets_3 <- workflow() %>%
  add_model(exp_smoothing() %>% set_engine("ets")) %>%
  add_recipe(recipe_spec_3) %>%
  fit(training(Splits))

# Prophet
fit_prophet_3 <- workflow() %>%
  add_model(prophet_reg() %>% set_engine("prophet")) %>%
  add_recipe(recipe_spec_3) %>%
  fit(training(Splits))

###########################

### Add fitted model to a Model Table
models_tbl <-
  modeltime_table(
    fit_glmnet,
    fit_glmnet_2,
    fit_glmnet_3,
    fit_rf,
    fit_rf_2,
    fit_rf_3,
    fit_xgb,
    fit_xgb_2,
    fit_xgb_3,
    fit_prophet_boost,
    fit_prophet_boost_2,
    fit_prophet_boost_3,
    fit_arima_boosted,
    fit_arima_boosted_2,
    fit_arima_boosted_3,
    fit_ets,
    fit_ets_2,
    fit_ets_3,
    fit_prophet,
    fit_prophet_2,
    fit_prophet_3
  )
###########################

rm(
  fit_glmnet,
  fit_glmnet_2,
  fit_glmnet_3,
  fit_rf,
  fit_rf_2,
  fit_rf_3,
  fit_xgb,
  fit_xgb_2,
  fit_xgb_3,
  fit_prophet_boost,
  fit_prophet_boost_2,
  fit_prophet_boost_3,
  fit_arima_boosted,
  fit_arima_boosted_2,
  fit_arima_boosted_3,
  fit_ets,
  fit_ets_2,
  fit_ets_3,
  fit_prophet,
  fit_prophet_2,
  fit_prophet_3,
  recipe_spec,
  recipe_spec_2,
  recipe_spec_3
)

### Calibration
calibration_table <- models_tbl %>%
  modeltime_calibrate(new_data = testing(Splits))

# Plots time series models
calibration_table %>%
  modeltime_forecast(
    new_data = testing(Splits),
    actual_data = lag_df_orders %>%
      drop_na()
  ) %>%
  plot_modeltime_forecast(.interactive = TRUE)

# Shows metrics
calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

# Creates "final" df to visualize variables the model uses
df_orders_final <- recipe(order_count ~ ., df_orders) %>%
  step_timeseries_signature(Date) %>%
  step_rm(matches("(hour)|(minute)|(second)|(am.pm)|(xts)|(iso)|(lbl)")) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  prep() %>%
  juice()

head(df_orders_final)


### Creates Time Series Model ###

# Finds the min rmse model that is not overfitting
min_rmse_model <- calibration_table %>%
  modeltime_accuracy() %>%
  filter(rsq < 0.97) %>%
  filter(rmse == min(rmse)) %>%
  select(.model_id) %>%
  pull()

#####
# Forecast
#####

actual_orders_data <- lag_df_orders %>%
  filter(!(is.na(order_count)))

new_data <- lag_df_orders %>%
  filter(is.na(order_count)) %>%
  filter(!(is.na(Orders_lag1)))

# Defines the number of days out you forecast (60 = 60 days)
forecast_horizon <- 60

# Writes forecasting function incorporating lags
generate_forecast <- function(min_rmse_model, actual_orders_data, lag_df_orders, 
                              new_data, forecast_horizon) {
  
  forecast_results <- actual_orders_data
  
  modeltime_actual_data_format <- actual_orders_data %>% 
    mutate(
      .index = Date,
      .value = order_count,
      .conf_lo = NA,
      .conf_hi = NA,
      .key = "actual",
      .model_desc = "ACTUAL",
      .model_id = NA
    ) %>%
    select(.index, .value, .conf_lo, .conf_hi, .key, .model_desc, .model_id)
  
  lag_df_orders <- lag_df_orders
  
  for (i in 1:forecast_horizon) {
    forecast <- calibration_table %>%
      filter(.model_id %in% min_rmse_model) %>%
      modeltime_refit(actual_orders_data) %>%
      modeltime_forecast(
        new_data = new_data,
        actual_data = forecast_results
      )
    
    # Extract the forecasted value
    forecasted_value <- forecast %>%
      filter(.key == "prediction") %>%
      select(.index, .value) %>%
      slice_tail(n = 1) %>%
      pull(.value)
    
    # Extract the .conf_lo value
    forecasted_lo <- forecast %>%
      filter(.key == "prediction") %>%
      select(.index, .conf_lo) %>%
      slice_tail(n = 1) %>%
      pull(.conf_lo)
    
    # Extract the .conf_hi value
    forecasted_hi <- forecast %>%
      filter(.key == "prediction") %>%
      select(.index, .conf_hi) %>%
      slice_tail(n = 1) %>%
      pull(.conf_hi)
    
    # Add the forecasted value to the results
    next_date <- lag_df_orders %>%
      filter(is.na(order_count)) %>%
      filter(Date == min(Date)) %>%
      pull(Date)
    next_row <- data.frame(Date = next_date,
                           order_count = forecasted_value,
                           Orders_lag1 = NA,
                           Orders_lag_week = NA)
    
    forecast_results <- bind_rows(forecast_results, next_row)
    
    #Creates a separate forecast table in the Modeltime format
    Luke_forecast <- data.frame(
      .index = next_date,
      .value = forecasted_value,
      .conf_lo = forecasted_lo,
      .conf_hi = forecasted_hi,
      .key = "prediction",
      .model_desc = calibration_table %>%
        filter(.model_id %in% min_rmse_model) %>%
        pull(.model_desc),
      .model_id = min_rmse_model
    )
    
    modeltime_actual_data_format <- bind_rows(modeltime_actual_data_format,Luke_forecast)
    
    # Update lag_df_unique for next iteration
    lag_df_orders <- lag_df_orders %>%
      mutate(
        order_count = if_else(Date == next_date, forecasted_value, order_count),
        Orders_lag1 = lag(order_count,1)
      ) %>%
      select(-Orders_lag_week)
    
    # Adds week lag variable
    week_lag <- lag_df_orders %>%
      select(Date,order_count) %>%
      mutate(next_week = Date + days(7)) %>%
      select(next_week, order_count) %>%
      rename("Orders_lag_week" = "order_count")
    
    lag_df_orders <- left_join(lag_df_orders,week_lag, by = c("Date" = "next_week")) %>%
      mutate(Orders_lag_week = ifelse(Orders_lag_week == 0, NA, Orders_lag_week),
             Orders_lag_week = na.approx(Orders_lag_week, na.rm = FALSE),
             order_count = as.integer(order_count),
             Orders_lag1 = as.integer(Orders_lag1))
    
    # Updates forecast_results
    forecast_results <- lag_df_orders %>%
      filter(!(is.na(order_count))) 
    
    new_data <- lag_df_orders %>%
      filter(is.na(order_count)) %>%
      filter(!(is.na(Orders_lag1)))
  }
  
  return(modeltime_actual_data_format)
}

# Generates forecast
forecast_results <- generate_forecast(min_rmse_model, actual_orders_data, lag_df_orders, 
                                      new_data, forecast_horizon)

forecast_results %>%
  plot_modeltime_forecast(.interactive = FALSE)

# Takes forecasted data and joins it to the original orders data
df_orders_variable <- rbind(
  df_orders %>% 
    select(Date,order_count) %>%
    drop_na() %>%
    mutate(.conf_lo_orders = NA,
           .conf_hi_orders = NA),
  forecast_results %>%
    rename("Date" = ".index") %>%
    filter(.key %in% "prediction") %>%
    select(-.model_id, -.key) %>%
    rename("order_count"= ".value",
           ".conf_lo_orders" = ".conf_lo",
           ".conf_hi_orders" = ".conf_hi") %>%
    mutate(Date = ymd(Date)) %>%
    select(-.model_desc) %>%
    select(Date,order_count,.conf_lo_orders,.conf_hi_orders)
) %>%
  mutate(order_count = as.integer(order_count))

# Joins the orders_count data to the calls df
df_calls <- left_join(df_calls %>%
                        filter(Date >= min(df_orders_variable$Date)),
                      df_orders_variable %>%
                        select(Date,order_count),
                      by = "Date")

rm(actual_orders_data,df_orders_final,new_data,calibration_table,models_tbl,df_orders,lag_df_orders)


#######################################
# CALLS FORECAST
#######################################

### Discovery ###

# Visualizes calls
df_calls %>%
  filter(Date < Sys.Date()) %>%
  ggplot(aes(Date, Calls)) +
  geom_line() + 
  geom_point()+
  geom_smooth() +
  labs(title = "Calls - Japan",
       x = "",
       y = "") + 
  theme_classic()

# STL Decomposition on calls
detrended_calls <- df_calls %>%
  drop_na()
stl_decomposition <- stl(ts(detrended_calls$Calls, frequency = 365), s.window = "periodic")
trend_component <- stl_decomposition$time.series[, "trend"]
detrended_calls$detrended_stl <- detrended_calls$Calls - trend_component

# Visualize
ggplot(detrended_calls, aes(Date, detrended_stl)) +
  geom_line() + 
  geom_point() +
  geom_smooth() +
  labs(title = "STL Detrended Calls - Japan",
       x = "",
       y = "") + 
  theme_classic()

# Plot ACF (Autocorrelation) and PACF (Partial Autocorrelation) with more lags
acf(detrended_calls$detrended_stl, lag.max = 730, xlab="Lag (years)")
pacf(detrended_calls$detrended_stl, lag.max = 730, xlab="Lag (years)")

rm(detrended_calls,stl_decomposition,trend_component)

# Checks correlations
df_calls %>%
  drop_na() %>%
  select(-Date, -Day) %>%
  cor() %>%
  round(2)

# Prepares data splits
lag_df_calls <- df_calls %>%
  arrange(Date) %>%
  filter(Date <= (Sys.Date() + days(90))) %>% # Only shows data 3 months from the date the code is ran
  mutate(Calls_lag1 = lag(Calls,1)) %>% 
  filter(Date > "2020-01-06") 

# Weekly lag removed as it lowered machine learning metrics

###########################################################

#week_lag <- df_calls %>%
#  select(Date,Calls) %>%
#  mutate(next_week = Date + days(7)) %>%
#  filter(next_week >= "2019-01-01") %>%
#  select(next_week,Calls) %>%
#  rename("Calls_lag_week" = "Calls")

#lag_df_calls <- left_join(lag_df_calls,week_lag, by = c("Date" = "next_week")) %>%
#  filter(Date > "2019-01-10") %>%
#  mutate(Calls_lag_week = ifelse(Calls_lag_week == 0, NA, Calls_lag_week),
#         Calls_lag_week = na.approx(Calls_lag_week, na.rm = FALSE)) 

###########################################################

# Splits data
Splits <- initial_time_split(lag_df_calls %>%
                               drop_na(), prop = 0.85)

Splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(Date, Calls,.interactive = TRUE)


### Modeling ###

# Model pre-processing
recipe_spec <- recipe(Calls ~ ., training(Splits)) %>%
  step_timeseries_signature(Date) %>%
  step_rm(matches("(hour)|(minute)|(second)|(am.pm)|(xts)|(iso)|(lbl)")) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) 

recipe_spec_2 <- recipe(Calls ~ ., training(Splits)) %>%
  step_timeseries_signature(Date) %>%
  step_fourier(Date, period = 12, K = 2) %>%
  step_rm(matches("(hour)|(minute)|(second)|(am.pm)|(xts)|(iso)|(lbl)")) %>%
  # step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) 

recipe_spec_3 <- recipe(Calls ~ ., training(Splits)) %>%
  step_timeseries_signature(Date) %>%
  step_fourier(Date, period = 12, K = 3) %>%
  step_rm(matches("(hour)|(minute)|(second)|(am.pm)|(xts)|(iso)|(lbl)")) %>%
  # step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

###########################
# Recipe 1
###########################

### ML Models
# Elastic Net
fit_glmnet <- linear_reg(penalty = 0.1, mixture = 0.5) %>%
  set_engine("glmnet")
fit_glmnet <- workflow() %>%
  add_model(fit_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(Date)) %>%
  fit(training(Splits))

# Random Forest
fit_rf<- workflow() %>%
  add_model(rand_forest("regression") %>% set_engine("ranger")) %>%
  add_recipe(recipe_spec %>% step_rm(Date)) %>%
  fit(training(Splits))

# Gradient Boost
fit_xgb <- workflow() %>%
  add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
  add_recipe(recipe_spec %>% step_rm(Date)) %>%
  fit(training(Splits))

# Prophet Boost Hybrid Model
fit_prophet_boost <- workflow() %>%
  add_model(prophet_boost("regression", seasonality_yearly = TRUE) %>% set_engine("prophet_xgboost")) %>%
  add_recipe(recipe_spec) %>%
  fit(training(Splits))

# Arima Boost
fit_arima_boosted <- workflow() %>%
  add_model(arima_boost("regression") %>% set_engine("auto_arima_xgboost")) %>%
  add_recipe(recipe_spec) %>%
  fit(training(Splits))

###########################
### Non ML Moels
# Exponential Smoothing
fit_ets <- workflow() %>%
  add_model(exp_smoothing() %>% set_engine("ets")) %>%
  add_recipe(recipe_spec) %>%
  fit(training(Splits))

# Prophet
fit_prophet <- workflow() %>%
  add_model(prophet_reg() %>% set_engine("prophet")) %>%
  add_recipe(recipe_spec) %>%
  fit(training(Splits))


###########################
# Recipe 2
###########################

### ML Models
# Elastic Net
fit_glmnet_2 <- linear_reg(penalty = 0.1, mixture = 0.5) %>%
  set_engine("glmnet")
fit_glmnet_2 <- workflow() %>%
  add_model(fit_glmnet_2) %>%
  add_recipe(recipe_spec_2 %>% step_rm(Date)) %>%
  fit(training(Splits))

# Random Forest
fit_rf_2<- workflow() %>%
  add_model(rand_forest("regression") %>% set_engine("ranger")) %>%
  add_recipe(recipe_spec_2 %>% step_rm(Date)) %>%
  fit(training(Splits))

# Gradient Boost
fit_xgb_2 <- workflow() %>%
  add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
  add_recipe(recipe_spec_2 %>% step_rm(Date)) %>%
  fit(training(Splits))

# Prophet Boost Hybrid Model
fit_prophet_boost_2 <- workflow() %>%
  add_model(prophet_boost("regression", seasonality_yearly = TRUE) %>% set_engine("prophet_xgboost")) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(Splits))

# Arima Boost
fit_arima_boosted_2 <- workflow() %>%
  add_model(arima_boost("regression") %>% set_engine("auto_arima_xgboost")) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(Splits))

###########################
### Non ML Moels
# Exponential Smoothing
fit_ets_2 <- workflow() %>%
  add_model(exp_smoothing() %>% set_engine("ets")) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(Splits))

# Prophet
fit_prophet_2 <- workflow() %>%
  add_model(prophet_reg() %>% set_engine("prophet")) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(Splits))


###########################
# Recipe 3
###########################

### ML Models
# Elastic Net
fit_glmnet_3 <- linear_reg(penalty = 0.1, mixture = 0.5) %>%
  set_engine("glmnet")
fit_glmnet_3 <- workflow() %>%
  add_model(fit_glmnet_3) %>%
  add_recipe(recipe_spec_3 %>% step_rm(Date)) %>%
  fit(training(Splits))

# Random Forest
fit_rf_3 <- workflow() %>%
  add_model(rand_forest("regression") %>% set_engine("ranger")) %>%
  add_recipe(recipe_spec_3 %>% step_rm(Date)) %>%
  fit(training(Splits))

# Gradient Boost
fit_xgb_3 <- workflow() %>%
  add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
  add_recipe(recipe_spec_3 %>% step_rm(Date)) %>%
  fit(training(Splits))

# Prophet Boost Hybrid Model
fit_prophet_boost_3 <- workflow() %>%
  add_model(prophet_boost("regression", seasonality_yearly = TRUE) %>% set_engine("prophet_xgboost")) %>%
  add_recipe(recipe_spec_3) %>%
  fit(training(Splits))

# Arima Boost
fit_arima_boosted_3 <- workflow() %>%
  add_model(arima_boost("regression") %>% set_engine("auto_arima_xgboost")) %>%
  add_recipe(recipe_spec_3) %>%
  fit(training(Splits))

###########################
### Non ML Moels
# Exponential Smoothing
fit_ets_3 <- workflow() %>%
  add_model(exp_smoothing() %>% set_engine("ets")) %>%
  add_recipe(recipe_spec_3) %>%
  fit(training(Splits))

# Prophet
fit_prophet_3 <- workflow() %>%
  add_model(prophet_reg() %>% set_engine("prophet")) %>%
  add_recipe(recipe_spec_3) %>%
  fit(training(Splits))

###########################

### Add fitted model to a Model Table
models_tbl <-
  modeltime_table(
    fit_glmnet,
    fit_glmnet_2,
    fit_glmnet_3,
    fit_rf,
    fit_rf_2,
    fit_rf_3,
    fit_xgb,
    fit_xgb_2,
    fit_xgb_3,
    fit_prophet_boost,
    fit_prophet_boost_2,
    fit_prophet_boost_3,
    fit_arima_boosted,
    fit_arima_boosted_2,
    fit_arima_boosted_3,
    fit_ets,
    fit_ets_2,
    fit_ets_3,
    fit_prophet,
    fit_prophet_2,
    fit_prophet_3
  )
###########################

rm(
  fit_glmnet,
  fit_glmnet_2,
  fit_glmnet_3,
  fit_rf,
  fit_rf_2,
  fit_rf_3,
  fit_xgb,
  fit_xgb_2,
  fit_xgb_3,
  fit_prophet_boost,
  fit_prophet_boost_2,
  fit_prophet_boost_3,
  fit_arima_boosted,
  fit_arima_boosted_2,
  fit_arima_boosted_3,
  fit_ets,
  fit_ets_2,
  fit_ets_3,
  fit_prophet,
  fit_prophet_2,
  fit_prophet_3,
  recipe_spec,
  recipe_spec_2,
  recipe_spec_3
)

### Calibration
calibration_table <- models_tbl %>%
  modeltime_calibrate(new_data = testing(Splits))

# Plots models
calibration_table %>%
  modeltime_forecast(
    new_data = testing(Splits),
    actual_data = lag_df_calls %>%
      drop_na()
  ) %>%
  plot_modeltime_forecast(.interactive = TRUE)

# Shows model metrics
calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )


### Final Model selection and fitting ###

# Finds the min RMSE model that is not overfitting
min_rmse_model <- calibration_table %>%
  modeltime_accuracy() %>%
  filter(rsq < 0.97) %>%
  filter(rmse == min(rmse)) %>%
  select(.model_id) %>%
  pull()

#####
# Forecast
#####

actual_call_data <- lag_df_calls %>%
  filter(!(is.na(Calls)))

new_data <- lag_df_calls %>%
  filter(is.na(Calls)) %>%
  filter(!(is.na(Calls_lag1)))

# Defines how many days out you want to forecast (60 = 60 days)
forecast_horizon <- 60

generate_forecast <- function(min_rmse_model, actual_call_data, lag_df_calls, 
                              new_data, forecast_horizon) {
  
  forecast_results <- actual_call_data
  
  modeltime_actual_data_format <- actual_call_data %>% 
    mutate(
      .index = Date,
      .value = Calls,
      .conf_lo = NA,
      .conf_hi = NA,
      .key = "actual",
      .model_desc = "ACTUAL",
      .model_id = NA
    ) %>%
    select(.index, .value, .conf_lo, .conf_hi, .key, .model_desc, .model_id)
  
  lag_df_calls <- lag_df_calls
  
  for (i in 1:forecast_horizon) {
    forecast <- calibration_table %>%
      filter(.model_id %in% min_rmse_model) %>%
      modeltime_refit(actual_call_data) %>%
      modeltime_forecast(
        new_data = new_data,
        actual_data = forecast_results
      )
    
    # Extract the forecasted value
    forecasted_value <- forecast %>%
      filter(.key == "prediction") %>%
      select(.index, .value) %>%
      slice_tail(n = 1) %>%
      pull(.value)
    
    # Extract the .conf_lo value
    forecasted_lo <- forecast %>%
      filter(.key == "prediction") %>%
      select(.index, .conf_lo) %>%
      slice_tail(n = 1) %>%
      pull(.conf_lo)
    
    # Extract the .conf_hi value
    forecasted_hi <- forecast %>%
      filter(.key == "prediction") %>%
      select(.index, .conf_hi) %>%
      slice_tail(n = 1) %>%
      pull(.conf_hi)
    
    # Add the forecasted value to the results
    next_date <- lag_df_calls %>%
      filter(is.na(Calls)) %>%
      filter(Date == min(Date)) %>%
      pull(Date)
    next_row <- data.frame(Date = next_date,
                           Calls = forecasted_value,
                           Calls_lag1 = NA)
    
    forecast_results <- bind_rows(forecast_results, next_row)
    
    #Creates a separate forecast table in the Modeltime format
    Luke_forecast <- data.frame(
      .index = next_date,
      .value = forecasted_value,
      .conf_lo = forecasted_lo,
      .conf_hi = forecasted_hi,
      .key = "prediction",
      .model_desc = calibration_table %>%
        filter(.model_id %in% min_rmse_model) %>%
        pull(.model_desc),
      .model_id = min_rmse_model
    )
    
    modeltime_actual_data_format <- bind_rows(modeltime_actual_data_format,Luke_forecast)
    
    # Update lag_df_unique for next iteration
    lag_df_calls <- lag_df_calls %>%
      mutate(
        Calls = if_else(Date == next_date, forecasted_value, Calls),
        Calls_lag1 = lag(Calls,1)
      ) #%>%
    #select(-Calls_lag_week)
    
    # Week lag previously removed
    
    ###########################################################
    
    # Adds week lag variable
    #week_lag <- lag_df_calls %>%
    #select(Date,Calls) %>%
    #mutate(next_week = Date + days(7)) %>%
    #select(next_week, Calls) %>%
    #rename("Calls_lag_week" = "Calls")
    
    #lag_df_calls <- left_join(lag_df_calls,week_lag, by = c("Date" = "next_week")) %>%
    #mutate(Calls_lag_week = ifelse(Calls_lag_week == 0, NA, Calls_lag_week),
    #Calls_lag_week = na.approx(Calls_lag_week, na.rm = FALSE))
    
    ###########################################################
    
    # Updates forecast_results
    forecast_results <- lag_df_calls %>%
      filter(!(is.na(Calls)))
    
    new_data <- lag_df_calls %>%
      filter(is.na(Calls)) %>%
      filter(!(is.na(Calls_lag1)))
  }
  
  return(modeltime_actual_data_format)
}

# Generates forecast
forecast_results <- generate_forecast(min_rmse_model, actual_call_data, lag_df_calls, 
                                      new_data, forecast_horizon)

# Plots forecast results
forecast_results %>%
  plot_modeltime_forecast(.interactive = FALSE)

model_name <- calibration_table %>%
  modeltime_accuracy() %>%
  filter(rsq < 0.97) %>%
  filter(rmse == min(rmse)) %>%
  select(.model_desc) %>%
  pull()

# Creates exportable df
Export <- forecast_results %>%
  rename("Date" = ".index") %>%
  filter(.key %in% "prediction") %>%
  select(-.model_id, -.key) %>%
  rename("Incoming_Calls_Forecast"= ".value") %>%
  mutate(Date = ymd(Date)) %>%
  select(-.model_desc) %>%
  select(Date,Incoming_Calls_Forecast,.conf_lo,.conf_hi)

# Adds orders forecast to the export file
Export <- Export %>%
  left_join(df_orders_variable,
            by = "Date")


write_csv(Export, "forecasts/Calls_Forecast.csv")