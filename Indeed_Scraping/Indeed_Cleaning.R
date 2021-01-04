library(dplyr)
library(stringr)

listings <- read.csv('Indeed_Scraping/listings_unitedstates.csv')

# Remove empty listings and all leftover html tags from job descriptions
listings <- listings %>%
  filter(Description != "") %>%
  mutate(Description = str_replace_all(Description, "<.*?>", ""))
  

write.csv(listings,'Indeed_Scraping/listings_unitedstates_cleaned.csv')
