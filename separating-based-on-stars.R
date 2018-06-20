### Separating the restaurant_rating data (assembled in the webscraping script) into different csv/txt files based on the number of stars it has.

library(dplyr)

export1 <- restaurant_rating[which(restaurant_rating$rating == 1),4]
write.csv(export1, file = "1_star.csv")

export2 <- restaurant_rating[which(restaurant_rating$rating == 2),4]
write.csv(export2, file = "2_star.csv")

export3 <- restaurant_rating[which(restaurant_rating$rating == 3),4]
write.csv(export3, file = "3_star.csv")

export4 <- restaurant_rating[which(restaurant_rating$rating == 4),4]
write.csv(export4, file = "4_star.csv")

export5 <- restaurant_rating[which(restaurant_rating$rating == 5),4]
write.csv(export5, file = "5_star.csv")
