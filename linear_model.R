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

