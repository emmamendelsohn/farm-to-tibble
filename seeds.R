library(tidyverse)

seeds <- read_csv("seeds.csv") %>%
  mutate(Variety = paste(class, variety, sep = " - "),
         Crop = "Flower") %>%
  select(Crop, 
         Variety, 
         `Product ID` = code,
         `Seed supplier` = company,
         `Seed Catalog Qty` = pkt_contents,
         `Seed Catalog Pricing` = pkt_price)

johnny <- seeds %>%
  filter(`Seed supplier` == "johnny's")

johnny_web <- read_csv("johnnys_flowers.csv") %>% 
  filter(product_id %in% johnny$`Product ID`) %>%
  select(`Product ID` = product_id, `Days to Germination` = days_to_germination, `Plant Spacing` = plant_spacing, sowing)

johnny2 <- left_join(johnny, johnny_web) %>%
  mutate(`Plant Spacing` = str_remove(`Plant Spacing`, "\".")) %>%
  mutate(`Plant Spacing` = str_replace(`Plant Spacing`, "12-15 For upright, plume type, thinner and more manageable stems can be achieved using a tighter spacing of 3-6\".",
                                       "3-6")) %>%
  mutate(`Plant Spacing` = str_replace(`Plant Spacing`, "12-18if being grown as a perennial. 12\" apart if being grown as an annual.",
                                       "12")) %>%
  mutate(`Plant Spacing` = str_replace(`Plant Spacing`, "6 Plants spaced more than 6\" will produce heads that are too large for cut flower use.",
                                       "6")) 

write_csv(johnny2, "obercreek_flowers_2020.csv")
  
# needed from cataloge: dtm, spacing, Stratify
# calculate: rows/bed, plants/foot, row feet, bed feet, tray size
# secondary - plant date, 