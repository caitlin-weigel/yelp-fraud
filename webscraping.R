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
colnames(model) = c("Rating", "Number of Friends", "Number of Reviews", "Review")

write.csv(model, file = "Yelp.csv")

fraud_data <- read.csv("Yelp.csv")

# Function ------------------------------------------------------------------------------------------------

scrape = function(url) {
  restaurant <- read_html(url)
  
  rating <- restaurant %>% html_nodes(".rating-large")
  friends <- restaurant %>% html_nodes(".friend-count") %>% html_text(trim = TRUE)
  review_count <- restaurant %>% html_nodes(".review-count") %>% html_text(trim = TRUE)
  review <- restaurant %>% html_nodes(".review-content p") %>% html_text(trim = TRUE)
  overall <- restaurant %>% html_nodes(".rating-very-large")
  
  rating <- rating %>% html_attr(name = "title")
  rating <- rating %>% substr(1, 3)
  overall <- overall %>% html_attr(name = "title")
  overall <- overall %>% substr(1, 3)
  
  friends <- as.numeric(gsub(" friends", "", friends))
  review_count <- as.numeric(gsub(" reviews", "", review_count))
  
  model <- cbind(rating, friends, review_count, review, rep(overall, length(rating)))
  colnames(model) = c("rating", "total_friends", "number_reviews", "review", "overall_rating")
  return(model)
}

yourstruly <- scrape("https://www.yelp.com/biz/yours-truly-restaurants-cleveland")
buffalo <- scrape("https://www.yelp.com/biz/buffalo-wild-wings-warrensville-heights")
buffalo2 <- scrape("https://www.yelp.com/biz/buffalo-wild-wings-cleveland-5")
olive <- scrape("https://www.yelp.com/biz/olive-garden-italian-restaurant-beachwood-3")
panera <- scrape("https://www.yelp.com/biz/panera-bread-cleveland-4")
panera2 <- scrape("https://www.yelp.com/biz/panera-bread-south-euclid")
cicis <- scrape("https://www.yelp.com/biz/cicis-cleveland")
dibellas <- scrape("https://www.yelp.com/biz/dibellas-subs-warrensville-hts")
pandaex <- scrape("https://www.yelp.com/biz/panda-express-parma-2")
boiler <- scrape("https://www.yelp.com/biz/the-boiler-65-cleveland?start=100&sort_by=rating_asc")

restaurant_rating <- rbind(yourstruly, buffalo, buffalo2, olive, panera, panera2, cicis, dibellas, pandaex, boiler)
