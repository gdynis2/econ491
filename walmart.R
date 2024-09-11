library(tidyverse)
library(stats)
library(ggplot2)
library(readr)
library(dplyr)
library(reshape2)

df_cal <- read.csv('econ491/calendar.csv', stringsAsFactors = FALSE)
df_cal$date = as.character(df_cal$date)
df_sales_train <- read.csv('econ491/sales_train_validation.csv', stringsAsFactors = FALSE)
df_prices <- read.csv('econ491/sell_prices.csv', stringsAsFactors = FALSE)

start_date <- as.Date("2011-01-29")
num_days <- 1913
date_sequence <- seq.Date(from = start_date, by = "day", length.out = num_days)
d_columns <- grep("^d_", colnames(df_sales_train), value = TRUE)
colnames(df_sales_train)[colnames(df_sales_train) %in% d_columns] <- as.character(date_sequence)

list_id_vars <- c('item_id', 'dept_id', 'cat_id', 'store_id', 'state_id')
df_melted_sales <- melt(df_sales_train, id.vars = list_id_vars, variable.name = 'd', value.name = 'sales')
names(df_melted_sales)[names(df_melted_sales) == "d"] <- "date"

df_train <- merge(df_melted_sales, df_cal, by = 'date', all.x = TRUE)

df_prices$id <- paste(df_prices$item_id, df_prices$store_id, sep = "_")
df_prices <- df_prices[, !(names(df_prices) %in% c('item_id', 'store_id'))]

df_train$id <- paste(df_train$item_id, df_train$store_id, sep = "_")

df_train <- merge(df_train, df_prices, by = c('id', 'wm_yr_wk'), all.x = TRUE)

df_train$event_name_1[is.na(df_train$event_name_1)] <- 'None'
df_train$event_type_1[is.na(df_train$event_type_1)] <- 'None'
df_train$event_name_2[is.na(df_train$event_name_2)] <- 'None'
df_train$event_type_2[is.na(df_train$event_type_2)] <- 'None'






View(df_melted_sales)
View(df_cal)
View(df_train)
View(df_prices)
