library(rvest)

url <- "https://www.yelp.com/biz/the-boiler-65-cleveland?sort_by=rating_asc"
webpage <- read_html(url)

# get the reviews
review_html <- webpage %>% html_nodes(".review-content p")
review_data <- html_text(review_html)

# get the names
name_html <- webpage %>% html_nodes(".user-display-name")
name_data <- html_text(name_html)
name_data = name_data[3:22]

# get the number of friends
friend_html <- webpage %>% html_nodes(".friend-count b")
friend_data <- html_text(friend_html)

# get the number of reviews
nreview_html <- webpage %>% html_nodes(".review-count b")
nreview_data <- html_text(nreview_html)

# get the rating
rating_html <- webpage %>% html_nodes(".review-content .rating-large")
rating <- rating_html %>% html_attr(name = "title")
rating <- gsub(" star rating", "", rating)
rating <- as.numeric(rating)

item <- c(20, 40, 60, 80, 100)
new_url <- paste0(url, "&start=")
reviewAll <- c(review_data)
nameAll <- c(name_data)
friendAll <- c(friend_data)
nreviewAll <- c(nreview_data)
ratingAll <- c(rating)

for (i in item) {
  New_Url <- paste0(new_url, as.character(i))
  webpage_new <- read_html(New_Url)
  review_html_new <- webpage_new %>% html_nodes(".review-content p")
  review_data_new <- html_text(review_html_new)
  reviewAll <- c(reviewAll, review_data_new)
  
  name_html_new <- webpage_new %>% html_nodes(".user-display-name")
  name_data_new <- html_text(name_html_new)
  name_data_new = name_data_new[4:length(name_data_new)-1]
  nameAll <- c(nameAll, name_data_new)
  
  friend_html_new <- webpage_new %>% html_nodes(".friend-count b")
  friend_data_new <- html_text(friend_html_new)
  friendAll <- c(friendAll, friend_data_new)
  
  nreview_html_new <- webpage_new %>% html_nodes(".review-count b")
  nreview_data_new <- html_text(nreview_html_new)
  nreviewAll <- c(nreviewAll, nreview_data_new)
  
  rating_html_new <- webpage_new %>% html_nodes(".review-content .rating-large")
  rating_new <- rating_html_new %>% html_attr(name = "title")
  rating_new <- gsub(" star rating", "", rating_new)
  rating_new <- as.numeric(rating_new)
  ratingAll <- c(ratingAll, rating_new)
}

boiler <- data.frame(name = nameAll, ratings = ratingAll, reviews = reviewAll, friends_count = friendAll,
                     review_count = nreviewAll)
boiler$rest_rating = 3
boiler$differ = boiler$ratings - boiler$rest_rating
View(boiler)

bad_reviews = which(boiler$differ == min(boiler$differ))
good_reviews = which(boiler$differ == max(boiler$differ))


