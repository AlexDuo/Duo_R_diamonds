library(tidyverse)
library(lubridate)
library(lmtest)
glucose <- read_csv("d:/glucose.csv")
mutate(glucose, blood_glucose = glucose$'Blood Glucose')
glucose <- mutate(glucose, blood_glucose = glucose$'Blood Glucose')
select(glucose, Timestamp, blood_glucose, `Prior Activity`)
glucose <- select(glucose, Timestamp, blood_glucose, `Prior Activity`)
mutate(glucose, timestamp = ymd_hm(glucose$Timestamp))
glucose <- mutate(glucose, timestamp = ymd_hm(glucose$Timestamp))
glucose <- select(glucose, timestamp, blood_glucose, `Prior Activity`)
mutate(glucose, imputed_prior_activity = ifelse(is.na(`Prior Activity`), "none", `Prior Activity`))
glucose <- mutate(glucose, imputed_prior_activity = ifelse(is.na(`Prior Activity`), "none", `Prior Activity`))
mutate(glucose, prior_activity = as.factor(glucose$imputed_prior_activity))
glucose <- mutate(glucose, prior_activity = as.factor(glucose$imputed_prior_activity))
glucose <- select(glucose, timestamp, blood_glucose, prior_activity)
ggplot(data = glucose) + geom_smooth(mapping = aes(x = timestamp, y = blood_glucose))
ggplot(data = glucose) + geom_path(mapping = aes(x = timestamp, y = blood_glucose))
ggplot(data = glucose, mapping = aes(x = prior_activity, y = blood_glucose)) + geom_boxplot()
model_1 <- lm(blood_glucose ~ prior_activity + timestamp, data=glucose)
mutate(glucose, day_of_week = wday(glucose$timestamp))
mutate(glucose, day_of_week = as.factor(wday(glucose$timestamp)))
glucose <- mutate(glucose, day_of_week = as.factor(wday(glucose$timestamp)))
model_2 <- lm(blood_glucose ~ prior_activity + timestamp + day_of_week, data=glucose)
mutate(glucose, hour_of_day = hour(glucose$timestamp))
glucose <- mutate(glucose, hour_of_day = as.factor(hour(glucose$timestamp)))
glucose <- mutate(glucose, period_of_day = cut_interval(hour(glucose$timestamp), length=4))
model_3 <- lm(blood_glucose ~ prior_activity + timestamp + period_of_day, data=glucose)

predicted_glucose <- mutate(glucose, predicted = predict(model_3, glucose))
  ggplot(data = predicted_glucose) + geom_path(mapping = aes(x = timestamp, y = blood_glucose))+
  geom_path(mapping = aes(x = timestamp, y = predicted, color="red"))
ggsave("timevsbloodglucose.jpg")

ggplot(data = predicted_glucose) + 
  geom_point(mapping = aes(x = blood_glucose, y = predicted)) + 
  geom_path(x = seq(from = 0, to = 400, length = nrow(predicted_glucose)), y = seq(from = 0, to = 400, length = nrow(predicted_glucose)))
ggsave("predictedvstrueglucose.jpg")


ggplot(data = predicted_glucose) + 
  geom_smooth(mapping = aes(x = blood_glucose, y = predicted))
ggsave("smoothpredictedvstrueglucose.jpg")
 

