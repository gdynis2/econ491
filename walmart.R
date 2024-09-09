library(tidyverse)
library(stats)
library(ggplot2)
library(readr)
library(dplyr)
library(reshape2)

df_cal <- read.csv('calendar.csv', stringsAsFactors = FALSE)
df_cal$date = as.character(df_cal$date)
df_sales_train <- read.csv('sales_train_validation.csv', stringsAsFactors = FALSE)
df_prices <- read.csv('sell_prices.csv', stringsAsFactors = FALSE)

start_date <- as.Date("2011-01-29")
num_days <- 1913
date_sequence <- seq.Date(from = start_date, by = "day", length.out = num_days)
d_columns <- grep("^d_", colnames(df_sales_train), value = TRUE)
colnames(df_sales_train)[colnames(df_sales_train) %in% d_columns] <- as.character(date_sequence)

list_id_vars <- c('item_id', 'dept_id', 'cat_id', 'store_id', 'state_id')
df_melted_sales <- melt(df_sales_train, id.vars = list_id_vars, variable.name = 'd', value.name = 'sales')
names(df_melted_sales)[names(df_melted_sales) == "d"] <- "date"

df_train <- merge(df_melted_sales, df_cal, by = 'date', all.x = TRUE)

colSums(is.na(df_train))

View(df_melted_sales)
View(df_cal)
View(df_train)

colnames(df_sales_train)
list_id_vars
