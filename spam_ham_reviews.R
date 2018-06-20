DATA1 = "data/sms-spam/SMSSpamCollection.csv"

# LIBRARIES -----------------------------------------------------------------------------------------------------------

library(dplyr)
library(tm)
library(e1071)
library(caret)

# ---------------------------------------------------------------------------------------------------------------------

data <- read.delim(DATA, quote = NULL, stringsAsFactors = FALSE) %>%
  setNames(c("label", "text")) %>%
  mutate(label = factor(label, levels = c("spam", "ham")))

# Create the corpus.
#
(corpus <- Corpus(VectorSource(data$text)))

# Clean up the corpus.
#
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
#
as.character(corpus[[1]])

# Stem the documents.
#
corpus <- tm_map(corpus, stemDocument)
#
as.character(corpus[[1]])

dtm <- DocumentTermMatrix(corpus)
#
dtm <- as.data.frame(as.matrix(dtm))

# Only retain terms that occur in more than five documents. This threshold is entirely arbitrary but necessary to
# reduce the size of the data.
#
dtm = dtm[, sapply(dtm, function(column) sum(column > 0)) > 5]

# Remove highly correlated terms.
#
dtm <- dtm[, -findCorrelation(cor(dtm), cutoff = 0.65, verbose = TRUE, exact = TRUE)]

# Add in label column.
#
dtm$spam <- factor(data$label == "spam", levels = c(T, F))
#
dim(dtm)
dim(data)

# TRAIN/TEST SPLIT ----------------------------------------------------------------------------------------------------

index <- createDataPartition(y = dtm$spam, p = 0.8)[[1]]

dtm_train <- dtm[index,]
dtm_test  <- dtm[-index,]

# CREATE MODEL: LOGISTIC REGRESSION -----------------------------------------------------------------------------------

fit <- glm(spam ~ ., data = dtm_train, family = binomial)

mean((predict(fit, dtm_test, type = "response") < 0.5) == dtm_test$spam)
#
# Is this result meaningful?
#
table(dtm_test$spam)
#
# Q. Check the model coefficients.

# Q. We haven't scaled the data (DTM is counts rather than proportions), does scaling improve the model?

# EXERCISES -----------------------------------------------------------------------------------------------------------

# 1. Use a TFIDF rather than DTM for the same analysis.
# 2. Try using a Naive Bayes model with the TFIDF.

# SOLUTIONS -----------------------------------------------------------------------------------------------------------