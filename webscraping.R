# LIBRARIES----------------------------------------------------------------------------------------------------------

library(rvest)
library(httr)
library(dplyr)
library(stringr)

# Scraping ------------------------------------------------------------------------------------------------------------

url = "https://www.yelp.com/biz/dibellas-subs-warrensville-hts"

dibellas <- read_html(url)

# Collecting the data ----------------------------------------------------------------------------------------------

rating <- dibellas %>% html_nodes(".rating-large")

friends <- dibellas %>% html_nodes(".friend-count") %>% html_text(trim = TRUE)

review_count <- dibellas %>% html_nodes(".review-count") %>% html_text(trim = TRUE)

review <- dibellas %>% html_nodes(".review-content p") %>% html_text(trim = TRUE)

#Cleaning up the data -------------------------------------------------------------------------------------------------

rating <- rating %>% html_attr(name = "title")
rating <- rating %>% substr(1, 3)

friends <- as.numeric(gsub(" friends", "", friends))

review_count <- as.numeric(gsub(" reviews", "", review_count))

# Creating a data frame -------------------------------------------------------------------------------------------------

model <- cbind(rating, friends, review_count, review)
modelcolnames(model) = c("Rating", "Number of Friends", "Number of Reviews", "Review")

write.csv(model, file = "Yelp.csv")

fraud_data <- read.csv("Yelp.csv")

# Function ------------------------------------------------------------------------------------------------

scrape = function(url) {
  restaurant <- read_html(url)
  
  rating <- restaurant %>% html_nodes(".rating-large")
  friends <- restaurant %>% html_nodes(".friend-count") %>% html_text(trim = TRUE)
  review_count <- restaurant %>% html_nodes(".review-count") %>% html_text(trim = TRUE)
  review <- restaurant %>% html_nodes(".review-content p") %>% html_text(trim = TRUE)

  rating <- rating %>% html_attr(name = "title")
  rating <- rating %>% substr(1, 3)
  friends <- as.numeric(gsub(" friends", "", friends))
  review_count <- as.numeric(gsub(" reviews", "", review_count))
 
  model <- cbind(rating, friends, review_count, review)
  colnames(model) = c("Rating", "Number of Friends", "Number of Reviews", "Review")
  return(model)
}

yourstruly <- scrape("https://www.yelp.com/biz/yours-truly-restaurants-cleveland")

