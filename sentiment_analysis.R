# Title     : TODO
# Objective : TODO
# Created by: Owner
# Created on: 1/24/2021
library(tidyverse)
library(RTextTools)
library(e1071)

tweet_all <- read.csv("amd_cleaned.csv")
positive <- read.csv("E:\\OneDrive\\SMT5\\Datsci\\proyekAkhir\\positive_negative_sentence\\positive_cleaned.csv")
negative <- read.csv("E:\\OneDrive\\SMT5\\Datsci\\proyekAkhir\\positive_negative_sentence\\negative_cleaned.csv")

#buaat factor ada berapa banyak positive dan negativenya
sentiment <- c(rep("positive", nrow(positive)), rep("negative", nrow(negative)))
sentiment <- as.factor(sentiment)

# menyatukan kalimat positif dan negatif jadi satu variabel
sentiment_all <- full_join(positive,negative)
sentiment_all <- as_tibble(sentiment_all)

mat <- create_matrix(tweet_all, language = "english", removeStopwords = TRUE,
                     stemWords = FALSE, tm::weightTfIdf)
mat <- as.matrix(mat)

mat_training <- create_matrix(sentiment_all, language = "english", removeStopwords = TRUE,
                     stemWords = FALSE, tm::weightTfIdf)
mat_training <- as.matrix(mat_training)

classifier <- naiveBayes(mat_training, sentiment)
predicted <- predict(classifier, mat)
predicted

tweet_all <- as_tibble(tweet_all)
tweet_all <- tweet_all %>% mutate(sentimen_nya = predicted)

plotnya <- ggplot(data = tweet_all, aes(x=sentimen_nya, fill = sentimen_nya)) +
            geom_bar()
plotnya
