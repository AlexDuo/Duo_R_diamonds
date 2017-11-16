ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50)
