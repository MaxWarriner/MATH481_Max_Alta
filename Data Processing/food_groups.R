
sam <- read_csv('sample_data.csv')


sam <- sam |>
  mutate(vegetable_portions = sweet_potato + carrot + potatoes + chard + kale + cabbage + greenbeans, 
         legume_portions = lentils + chickpeas + beans, 
         grain_portions = teff + barley + sorghum + maize + wheat + rice + pasta + oats + barley_porride, 
         fruit_portions = pumpkin + tomatoes + banana + mango + guava + plums + oranges + avocado + papaya + pineapple, 
         meat_portions = beef + chicken + Fish + lamb + goat, 
         eggs_dairy_portions = eggs + milk + cheese + yogurt + milk_powder + butter, 
         processed_food_portions = soft_drinks + cakes + cakes + hamburger + french_fries + pizza + sambusa)








write_csv(sam, 'sample_data.csv')



