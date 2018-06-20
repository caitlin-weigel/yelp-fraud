library(tidytext)
library(dplyr)
library(tibble)

bad_reviews = which(boiler$differ == min(boiler$differ))
good_reviews = which(boiler$differ == max(boiler$differ))

# clean up the text
review_text <- as.character(boiler$reviews)
for (i in 1:103) {
  review_text[i] = as.character(review_text[i])
}
review_table <- as.data.frame(review_text)
review_table <- review_table %>% mutate(person = row_number())
reivew_table <- as.tibble(review_table)
review_table$review_text = as.character(review_table$review_text)
positive_review = review_table[c(good_reviews),]
negative_review = review_table[c(bad_reviews),]

positive_review <- positive_review %>% unnest_tokens(word, review_text)
negative_review <- negative_review %>% unnest_tokens(word, review_text)

# count the word frequencies
data(stop_words)
positive_review <- positive_review %>% anti_join(stop_words)
positive_review %>% count(word, sort = TRUE)

negative_review <- negative_review %>% anti_join(stop_words)
negative_review %>% count(word, sort = TRUE)

# plot the word frequencies
library(ggplot2)
positive_count <- positive_review %>% count(word, sort = TRUE) %>% filter(n >= 10) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()
negative_count <- negative_review %>% count(word, sort = TRUE) %>% filter(n >= 10) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

# the spread of sentiments
library(tidyr)
positive_sentiment <- positive_review %>% inner_join(get_sentiments("bing")) %>% count(person, sentiment) %>% spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative)
positive_spread <- ggplot(positive_sentiment, aes(person, sentiment)) +
  geom_col(show.legend = FALSE)

negative_sentiment <- negative_review %>% inner_join(get_sentiments("bing")) %>% count(person, sentiment) %>% spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative)
negative_spread <- ggplot(negative_sentiment, aes(person, sentiment)) +
  geom_col(show.legend = FALSE)

# common postive and negative words
bing_word_counts_positive <- positive_review %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts_positive_graph <- bing_word_counts_positive %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

bing_word_counts_negative <- negative_review %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts_negative_graph <- bing_word_counts_negative %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# generate word cloud
library(wordcloud)
library(reshape2)
positive_review %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)
negative_review %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)
