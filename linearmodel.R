library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)



df_summary <- read.csv("df_summary.csv")

df_TX = df_summary %>% filter(state_id == 'TX')

df_TX <- df_TX %>%
  mutate(Monday = ifelse(weekday == "Monday", 1, 0),
         Tuesday = ifelse(weekday == "Tuesday", 1, 0),
         Wednesday = ifelse(weekday == "Wednesday", 1, 0),
         Thursday = ifelse(weekday == "Thursday", 1, 0),
         Friday = ifelse(weekday == "Friday", 1, 0),
         Saturday = ifelse(weekday == "Saturday", 1, 0),
         Sunday = ifelse(weekday == "Sunday", 1, 0))

df_TX <- df_TX %>%
  mutate(event = ifelse(event_name_1 == "None", 0, 1))

train_df = head(df_TX, 1913 - 28)
test_df = tail(df_TX, 28)

print(dim(train_df))
print(dim(test_df))



lm_model_tx <- lm(total_sales ~ mean_sell_price +
                    Monday + Tuesday + Wednesday + Thursday
                  + Friday + Saturday + snap_TX, train_df)
summary(lm_model_tx)

features = c("date", "mean_sell_price", "Monday", "Tuesday", "Wednesday",
             "Thursday", "Friday", "Saturday", "Sunday", "snap_TX")

actual_y = test_df$total_sales
test_values <- test_df %>% select(all_of(features))

predictions = as.numeric(predict(lm_model_tx, test_values))

mse = mean((actual_y - predictions)^2, na.rm=TRUE)
rmse = sqrt(mse)
print(rmse)

squared_errors = (actual_y - predictions)^2
errors = (actual_y - predictions)

plot_df = data.frame(date = as.Date(test_df$date), errors = squared_errors)

ggplot(plot_df, aes(x = date, y = errors)) +
  geom_line(color = "blue") +
  labs(title = "Line Plot of Squared Errors", x = "Date", y = "Squared Error") +
  theme_minimal()


df_CA = df_summary |>
  filter(state_id == "CA")

df_CA <- df_CA %>%
  arrange(date) %>%  # Ensure data is ordered by date
  mutate(time_index = as.numeric(as.Date(date)) - 15002)  # Create time index

# Check the first few rows
View(head(df_CA))


df_CA <- df_CA %>%
  mutate(event = ifelse(event_name_1 == "None", 0, 1))

df_CA <- df_CA %>%
  mutate(Monday = ifelse(weekday == "Monday", 1, 0),
         Tuesday = ifelse(weekday == "Tuesday", 1, 0),
         Wednesday = ifelse(weekday == "Wednesday", 1, 0),
         Thursday = ifelse(weekday == "Thursday", 1, 0),
         Friday = ifelse(weekday == "Friday", 1, 0),
         Saturday = ifelse(weekday == "Saturday", 1, 0),
         Sunday = ifelse(weekday == "Sunday", 1, 0))

lm_model <- lm(total_sales ~ -1 + mean_sell_price + Monday + Tuesday + Wednesday +
                 Thursday + Friday + Saturday + Sunday  + event + snap_CA, df_CA)
summary(lm_model)







train_df = head(df_CA, 1913 - 28)
test_df = tail(df_CA, 28)

print(dim(train_df))
print(dim(test_df))



lm_model_ca <- lm(total_sales ~ mean_sell_price +
                    Monday + Tuesday + Wednesday + Thursday
                  + Friday + Saturday + snap_CA, train_df)
summary(lm_model_ca)

features = c("date", "mean_sell_price", "Monday", "Tuesday", "Wednesday",
             "Thursday", "Friday", "Saturday", "Sunday", "snap_CA")

actual_y = test_df$total_sales
test_values <- test_df %>% select(all_of(features))

predictions = as.numeric(predict(lm_model_ca, test_values))

mse = mean((actual_y - predictions)^2, na.rm=TRUE)
rmse = sqrt(mse)
print(rmse)

squared_errors = (actual_y - predictions)^2
errors = (actual_y - predictions)

plot_df = data.frame(date = as.Date(test_df$date), errors = squared_errors)

library(ggplot2)

ggplot(plot_df, aes(x = date, y = errors)) +
  geom_line(color = "blue") +
  labs(title = "Line Plot of Squared Errors", x = "Date", y = "Squared Error") +
  theme_minimal()

df_WI = df_summary %>% filter(state_id == 'WI')

df_WI <- df_WI %>%
  mutate(Monday = ifelse(weekday == "Monday", 1, 0),
         Tuesday = ifelse(weekday == "Tuesday", 1, 0),
         Wednesday = ifelse(weekday == "Wednesday", 1, 0),
         Thursday = ifelse(weekday == "Thursday", 1, 0),
         Friday = ifelse(weekday == "Friday", 1, 0),
         Saturday = ifelse(weekday == "Saturday", 1, 0),
         Sunday = ifelse(weekday == "Sunday", 1, 0))

df_WI <- df_WI %>%
  mutate(event = ifelse(event_name_1 == "None", 0, 1))

train_df = head(df_WI, 1913 - 28)
test_df = tail(df_WI, 28)

print(dim(train_df))
print(dim(test_df))

lm_model_wi <- lm(total_sales ~ mean_sell_price +
                    Monday + Tuesday + Wednesday + Thursday 
                  + Friday + Saturday + snap_WI, train_df)
summary(lm_model_wi)

features = c("date", "mean_sell_price", "Monday", "Tuesday", "Wednesday", 
             "Thursday", "Friday", "Saturday", "Sunday", "snap_WI")

actual_y = test_df$total_sales
test_values <- test_df[, features]

predictions = as.numeric(predict(lm_model_wi, test_values))

mse = mean((actual_y - predictions)^2, na.rm=TRUE)
rmse = sqrt(mse)
print(rmse)

squared_errors = (actual_y - predictions)^2
errors = (actual_y - predictions)

plot_df = data.frame(date = as.Date(test_df$date), errors = squared_errors)

ggplot(plot_df, aes(x = date, y = errors)) +
  geom_line(color = "blue") +
  labs(title = "Line Plot of Squared Errors", x = "Date", y = "Squared Error") +
  theme_minimal()

