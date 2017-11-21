# fit a simple linear regression model with price and carat
library(tidyverse)
library(modelr)
options(na.action = na.warn)
library(nycflights13)
library(lubridate)
install.packages("hexbin")
library(hexbin)
# make a filter that will only contain diamonds with carat<=2.5 and log the price and carat

diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>%
  mutate(lprice = log2(price), lcarat = log2(carat))


mod_diamond <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

# grid <- diamonds2 %>% 
#   data_grid(cut, .model = mod_diamond) %>% 
#   add_predictions(mod_diamond)


# create bins 

# grid <- diamonds2 %>% 
#   data_grid(carat = seq_range(carat, 20)) %>% 
#   mutate(lcarat = log2(carat)) %>% 
#   add_predictions(mod_diamond, "lprice") %>% 
#   mutate(price = 2 ^ lprice)




# predict(mod_diamond, diamonds2)



predicted_price <- mutate(diamonds2, predicted = predict(mod_diamond, diamonds2))

# predicted_price <- mutate(, blood_glucose = glucose$'Blood Glucose')
# 
#  grid <- predicted_price %>% 
#    data_grid(carat = seq_range(carat, 20)) %>% 
#    mutate(lcarat = log2(carat)) %>% 
#    add_predictions(mod_diamond, "lprice") %>% 
#    mutate(price = 2 ^ lprice)


ggplot(data = predicted_price)+
geom_line(mapping = aes(x = carat, y = price))+ 
geom_line(mapping = aes(x = carat, y = 2^predicted, color="red"))
ggsave("final.jpg")
ggplot(data = predicted_price)+
  geom_smooth(mapping = aes(x = carat, y = price))+ 
  geom_smooth(mapping = aes(x = carat, y = 2^predicted, color="red"))
ggsave("finalsmooth.jpg")
ggplot(data = predicted_price)+
  geom_smooth(mapping = aes(x = 2^predicted, y = price))
ggsave("finalpredictedvstrue.jpg")
