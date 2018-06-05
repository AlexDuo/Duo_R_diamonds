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
# ----------------------------------------------------------------------------
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



# exercises starts-------------------------------------------------------------
# create column wdaywithterms
daily <- daily %>%
  mutate(wdaywithterms = 
           case_when(.$wday == "Sat" & .$term == "summer" ~ "Sat-summer",
                     .$wday == "Sat" & .$ term == "fall" ~ "Sat-fall",
                     .$wday == "Sat" & .$term == "spring" ~ "Sat-spring",
                     TRUE ~ as.character(.$wday)))
# fit a new modle
<<<<<<< HEAD
modwdayterms <- lm(n ~ wdayterms, data = daily)
=======
modwdayterms <- lm(n ~ wdaywithterms, data = daily)

# every combination of wday is mod2 which is wday*term
daily %>% 
  gather_residuals(onlysatterms = modwdayterms, allcombination = mod2) %>% 
  ggplot(aes(date, resid, colour = model)) +
  geom_line(alpha = 0.75)

ggsave("modwdaytermvsmod2.jpg")


# create column holidayandtermsat
daily <- daily %>%
  mutate(holidayandtermsat = 
           case_when(
             .$date %in% lubridate::ymd(c(20130101,
                                          20130704, 
                                          20131031, 
                                          20131128,
                                          20131224,
                                          20131225)) ~
               "holiday",
             .$wday == "Sat" & .$term == "summer" ~ "Sat-summer",
             .$wday == "Sat" & .$ term == "fall" ~ "Sat-fall",
             .$wday == "Sat" & .$term == "spring" ~ "Sat-spring",
             TRUE ~ as.character(.$wday)))

# fit the new model

modholidaytermsat <- lm(n ~ holidayandtermsat, data = daily)

# plot the residual vs date

daily %>% 
  add_residuals(modholidaytermsat, "resid") %>% 
  ggplot(aes(date, resid)) + 
  geom_hline(yintercept = 0, size = 2, colour = "white") + 
  geom_line()

ggsave("modholidaytermsat.jpg")

# create a column for month
daily <- daily %>% 
  mutate(month = month(date))

# daily <- daily %>% 
#   mutate(month = monthcut(date))
# 
# 
# monthcut <- function(date) {
#   cut(date, 
#       breaks = ymd(20130101, 20130201, 20130301, 20130401, 20130501, 20130601, 20130701, 20130801, 20130901, 20131001, 20131101, 20131201),
#       labels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec") 
#   )
# }

# fit the new model

wdaymonth <- lm(n ~ wday*month, data = daily)

daily %>% 
  gather_residuals(wdaymonth = wdaymonth, wday = mod2) %>% 
  ggplot(aes(date, resid, colour = model)) +
  geom_line(alpha = 0.75)

ggsave("wdaymonth.jpg")

mod8 <- lm(n ~ wday * ns(date, 5), data = daily)

daily %>%
  data_grid(wday, date = seq_range(date, n = 13)) %>%
  add_predictions(mod8) %>%
  ggplot(aes(date, pred, colour = wday)) +
  geom_line() +
  geom_point()








# 7-------------------------------------------------------------------------
flights %>% 
  mutate(date = make_date(year, month, day),
         wday = wday(date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(mean_distance_of_wday =  mean(distance)) %>%
  
  ggplot(aes(y = mean_distance_of_wday, x = wday)) +
  geom_point()
ggsave("wdayvsmeandistance.jpg")

View(flights)

flights %>% 
  mutate(date = make_date(year, month, day),
         wday = wday(date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(mean_travel_time_wday =  mean(hour)) %>%
  ggplot(aes(y = mean_travel_time_wday, x = wday)) +
  geom_point()
  
ggsave("traveltimevswday.jpg")


flights %>% 
  mutate(date = make_date(year, month, day),
         wday = wday(date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(mean_distance_of_wday =  mean(distance)) %>%
  
  ggplot(aes(y = mean_distance_of_wday, x = wday)) +
  geom_point()
ggsave("wdayvsmeandistance.jpg")


>>>>>>> bd6dd759ea7c7ed960bad78de1373b04d38b729a
