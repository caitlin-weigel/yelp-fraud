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
  overall <- restaurant %>% html_nodes(".rating-very-large")

  rating <- rating %>% html_attr(name = "title")
  rating <- rating %>% substr(1, 3)
  rating <- as.numeric(as.character(rating))
  overall <- overall %>% html_attr(name = "title")
  overall <- overall %>% substr(1, 3)
  overall <- as.numeric(as.character(overall))
  
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
restaurant_rating <- transform(restaurant_rating, difference_rating = as.numeric(as.character(rating)) - as.numeric(as.character(overall_rating)))
restaurant_rating <- transform(restaurant_rating, isFraud = ifelse(difference_rating > 2, "1", "0"))
write.csv(restaurant_rating, file = "Yelp.csv")

# Text Analysis --------------------------------------------------------------------------------------------------------

words <- restaurant_rating$review
words <- iconv(words, "ASCII", "UTF-8", sub="byte")
(corpus <- Corpus(VectorSource(restaurant_rating$review)))

corpus <- tm_map(corpus, tolower, mc.cores=1)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(corpus)
dtm <- as.data.frame(as.matrix(dtm))

dtm = dtm[, sapply(dtm, function(column) sum(column > 0)) > 6]
dtm <- dtm[, -findCorrelation(cor(dtm), cutoff = 0.65, verbose = TRUE, exact = TRUE)]
dtm$fraud <- factor(restaurant_rating$isFraud == "1", levels = c(T, F))

index <- createDataPartition(y = dtm$fraud, p = 0.8)[[1]]

dtm_train <- dtm[index,]
dtm_test  <- dtm[-index,]

fit <- glm(fraud ~ ., data = dtm_train, family = binomial)

mean((predict(fit, dtm_test, type = "response") < 0.5) == dtm_test$fraud)

# Modeling -----------------------------------------------------------------------------------------------------------------

library(caret)
train.index <- createDataPartition(restaurant_rating$isFraud, p = 0.8, list = FALSE)
train <- restaurant_rating[train.index, ]
test <-restaurant_rating[-train.index, ]

train <- train %>% select(-review, -number_reviews)
test <- test %>% select(-review, -number_reviews)



fit <- glm(isFraud ~ rating + total_friends + difference_rating + overall_rating, data = train, family = "binomial")
prediction <- predict(fit, test, type = "prob")

mean(prediction==test$isFraud)


fit_rpart <- rpart(isFraud ~ as.factor(rating) + as.factor(total_friends)
                   + difference_rating + as.factor(overall_rating), data = train)
prediction <- predict(fit, test, type = "response")
mean(prediction == restaurant_rating$isFraud)

fit_rpart <- rpart(isFraud ~ as.factor(rating) + as.factor(total_friends) + as.factor(number_reviews) + difference_rating + as.factor(overall_rating), data = restaurant_rating)
test_predictions <- predict(fit, test, type = "response")
mean(test_predictions == restaurant_rating$isFraud)



