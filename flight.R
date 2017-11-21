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
  geom_line(mapping = aes(x = date, y = n))+ 
  geom_line(mapping = aes(x = date, y = predicted1, color="red"))
ggsave("mod1vstrue01.jpg")

ggplot(data = daily)+
  geom_smooth(mapping = aes(x = n, y = predicted1))
ggsave("mode1predictedvstrue.jpg")
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
  geom_line(mapping = aes(x = date, y = n))+ 
  geom_line(mapping = aes(x = date, y = predicted2, color="red"))
ggsave("mod2vstrue01.jpg")


ggplot(data = daily)+
  geom_smooth(mapping = aes(x = n, y = predicted2))
ggsave("model2predictedvstrue.jpg")
# ---------------------------------------------------------------------------
# new model
mod3 <- MASS::rlm(n ~ wday * term, data = daily)
# add predicted value(mod3) into the table
daily <- mutate(daily, predicted3= predict(mod3, daily))
# compare the actrue and predicted
ggplot(data = daily)+
  geom_line(mapping = aes(x = date, y = n))+ 
  geom_line(mapping = aes(x = date, y = predicted3, color="red"))
ggsave("mod3vstrue01.jpg")

ggplot(data = daily)+
  geom_smooth(mapping = aes(x = n, y = predicted3))
ggsave("model3predictedvstrue.jpg")
# spline-based model
mod4 <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)
# add predicted value(mod4) into the table
daily <- mutate(daily, predicted4= predict(mod4, daily))
# compare the actrue and predicted
ggplot(data = daily)+
  geom_line(mapping = aes(x = date, y = n))+ 
  geom_line(mapping = aes(x = date, y = predicted4, color="red"))
ggsave("mod4vstrue01.jpg")

ggplot(data = daily)+
  geom_smooth(mapping = aes(x = n, y = predicted4))
ggsave("model4predictedvstrue.jpg")

# exercises starts----------------------------------------------------------------------------
# create wdayterms
daily <- daily %>%
  mutate(wdayterms = 
           case_when(.$wday == "Sat" & .$term == "summer" ~ "Sat-summer",
                     .$wday == "Sat" & .$ term == "fall" ~ "Sat-fall",
                     .$wday == "Sat" & .$term == "spring" ~ "Sat-spring",
                     TRUE ~ as.character(.$wday)))
# fit a new modle
modwdayterms <- lm(n ~ wday2, data = daily)
