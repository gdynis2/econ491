library(tidyverse)
library(stats)
library(ggplot2)
library(readr)
library(dplyr)
library(reshape2)

df_train = read.csv('df_train.csv')
cat("Training Data Types:\n")
str(df_train)

df_train <- df_train %>%
  mutate(month_year_label = paste(month, year, sep = "-"))

breaks_6_months <- seq(1, max(df_summary$month_year_index), by = 6)

break_labels <- df_labels %>%
  filter(month_year_index %in% breaks_6_months)

ggplot(df_summary, aes(x = month_year_index, y = total_sales, color = state_id, group = state_id)) +
  geom_line(size = 1) +
  labs(title = "Sum of Sales by Month-Year for Each State",
       x = "Month-Year",
       y = "Total Sales") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(
    breaks = breaks_6_months,
    labels = break_labels$month_year_label
  )
)
