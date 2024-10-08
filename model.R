library(readr)
library(dplyr)

df_train <- df_train %>%
  mutate(Monday = ifelse(weekday == "Monday", 1, 0),
         Tuesday = ifelse(weekday == "Tuesday", 1, 0),
         Wednesday = ifelse(weekday == "Wednesday", 1, 0),
         Thursday = ifelse(weekday == "Thursday", 1, 0),
         Friday = ifelse(weekday == "Friday", 1, 0),
         Saturday = ifelse(weekday == "Saturday", 1, 0),
         Sunday = ifelse(weekday == "Sunday", 1, 0))

df_train <- df_train %>%
  mutate(event = ifelse(event_name_1 == "None", 0, 1))

df_train <- df_train %>%
  mutate(FOODS = ifelse(cat_id == "FOODS", 1, 0),
         HOBBIES = ifelse(cat_id == "HOBBIES", 1, 0),
         HOUSEHOLD = ifelse(cat_id == "HOUSEHOLD", 1, 0))

df_selected <- df_train %>%
  select(FOODS, HOBBIES, HOUSEHOLD,
         Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday,
         event,
         sales, sell_price, state_id)

df_CA <- df_selected %>%
  filter(state_id == "CA")
df_TX <- df_selected %>%
  filter(state_id == "TX")
df_WI <- df_selected %>%
  filter(state_id == "WI")

write.csv(df_CA, "df_CA.csv", row.names = FALSE)
write.csv(df_TX, "df_TX.csv", row.names = FALSE)
write.csv(df_WI, "df_WI.csv", row.names = FALSE)
