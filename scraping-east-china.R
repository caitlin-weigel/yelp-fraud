### SCRAPING YELP ATTEMPT
## Caitlin Weigel

# Restaurant: East China
# Location: Cleveland, OH

# URL config
URL_restaurant = "https://www.yelp.com/biz/east-china-cleveland"

# Libraries
library(dplyr)
library(stringr)
library(rvest)

# Getting the words of the review into a tibble.

review_words <- read_html(URL_restaurant) %>% 
  html_nodes(".review-content p") %>% 
  html_text
review_words <- as_tibble(review_words)

# Getting the Number of Stars into a tibble
east_china <- read_html(URL_restaurant) %>% html_nodes(".review-content .rating-large")

stars <- html_attr(east_china, name = "title")
stars = strsplit(stars, " star rating")
stars <- as.numeric(as.character(stars))

review_stars <- as_tibble(stars)

# Binding the stars and the words together
reviews <- cbind(review_stars, review_words)
colnames(reviews) = c("Stars", "Words")

