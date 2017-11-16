#1.Focus on diamonds smaller than 2.5 carats (99.7% of the data)
#2.Log-transform the carat and price variables.


  # The log-transformation is particularly useful here because it makes
  # the pattern linear, and linear patterns are the easiest to work with
  # Log-transform the carat and price variables


diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

ggplot(diamonds2, aes(lcarat, lprice)) + 
  geom_hex(bins = 50)