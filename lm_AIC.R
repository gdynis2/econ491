library(MASS)

df_CA = read.csv("df_CA.csv")
View(head(df_CA, 10))

lm_model <- lm(sales ~ sell_price + FOODS + HOBBIES + HOUSEHOLD +
                 Monday + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday +
                 event, df_CA)
summary(lm_model)

stepwise_model <- stepAIC(lm_model, direction = "both")
summary(stepwise_model)
