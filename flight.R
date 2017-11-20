library(tidyverse)
library(modelr)
options(na.action = na.warn)
library(nycflights13)
library(lubridate)
install.packages("hexbin")
library(hexbin)
install.packages("lmtest")
library(lmtest)
library(splines)
# modify the format of date and group by date
daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())
daily


# day of week
daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))

# build a model
mod <- lm(n ~ wday, data = daily)

# add predicted value(mod) into the table
daily <- mutate(daily, predicted1= predict(mod, daily))
# compare the actrue and predicted
ggplot(data = daily)+
  geom_line(mapping = aes(x = wday, y = n))+ 
  geom_line(mapping = aes(x = wday, y = predicted1, color="red"))
ggsave("mod1vstrue01.jpg")

ggplot(data = daily)+
  geom_point(mapping = aes(x = n, y = predicted1))
# --------------------------------------------------------------------------
# add term
term <- function(date) {
  cut(date, 
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall") 
  )
}
# add term into table daily

daily <- daily %>% 
  mutate(term = term(date)) 

# new model
mod2 <- lm(n ~ wday * term, data = daily)
# add predicted value(mod2) into the table
daily <- mutate(daily, predicted2= predict(mod2, daily))
# compare the actrue and predicted
ggplot(data = daily)+
  geom_line(mapping = aes(x = term, y = n))+ 
  geom_line(mapping = aes(x = term, y = predicted2, color="red"))
ggsave("mod2vstrue01.jpg")


ggplot(data = daily)+
  geom_point(mapping = aes(x = n, y = predicted2))
# ---------------------------------------------------------------------------
# new model
mod3 <- MASS::rlm(n ~ wday * term, data = daily)
# add predicted value(mod3) into the table
daily <- mutate(daily, predicted3= predict(mod3, daily))
# compare the actrue and predicted
ggplot(data = daily)+
  geom_line(mapping = aes(x = term, y = n))+ 
  geom_line(mapping = aes(x = term, y = predicted3, color="red"))
ggsave("mod3vstrue01.jpg")

ggplot(data = daily)+
  geom_point(mapping = aes(x = n, y = predicted3))

# spline-based model
mod4 <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)
# add predicted value(mod4) into the table
daily <- mutate(daily, predicted4= predict(mod4, daily))
# compare the actrue and predicted
ggplot(data = daily)+
  geom_line(mapping = aes(x = wday, y = n))+ 
  geom_line(mapping = aes(x = wday, y = predicted4, color="red"))
ggsave("mod4vstrue01.jpg")

ggplot(data = daily)+
  geom_point(mapping = aes(x = n, y = predicted4))
