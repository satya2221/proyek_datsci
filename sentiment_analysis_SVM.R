# Title     : TODO
# Objective : TODO
# Created by: Owner
# Created on: 1/24/2021
library(tidyverse)
library(RTextTools)
library(e1071)

set.seed(1234)
tweet_all <- read.csv("amd_cleaned.csv")
positive <- read.csv("E:\\OneDrive\\SMT5\\Datsci\\proyekAkhir\\positive_negative_sentence\\positive_cleaned.csv")
negative <- read.csv("E:\\OneDrive\\SMT5\\Datsci\\proyekAkhir\\positive_negative_sentence\\negative_cleaned.csv")

#buaat factor ada berapa banyak positive dan negativenya
sentiment <- c(rep("positive", nrow(positive)), rep("negative", nrow(negative)))
sentiment <- as.factor(sentiment)

# menyatukan kalimat positif dan negatif jadi satu variabel
sentiment_all <- full_join(positive,negative)
sentiment_all <- as_tibble(sentiment_all)

tweet_all <- as_tibble(tweet_all)

colnames(sentiment_all)[colnames(sentiment_all)=="value"] <- "text"

df <- full_join(sentiment_all,tweet_all)

df

#mat_x <- create_matrix(tweet_all, language = "english", removeStopwords = TRUE,
#                     stemWords = FALSE, tm::weightTfIdf)
#mat_x <- as.matrix(mat_x)

mat <- create_matrix(df, language = "english", removeStopwords = TRUE,
                     stemWords = TRUE, tm::weightTfIdf)
class(mat)
mat <- as.matrix(mat)

#mat_training <- create_matrix(sentiment_all, language = "english", removeStopwords = TRUE,
#                     stemWords = FALSE, tm::weightTfIdf)
#mat_training <- as.matrix(mat_training)
#
#gabungan <- merge(mat, mat_training,  by = "row.names", all = TRUE)
#
container <- create_container(mat, as.numeric(sentiment), trainSize = 1:2000,
   testSize = 2001:2511, virgin = TRUE)

models <- train_models(container, algorithms = "SVM")
#models <- train_models(container, algorithms = "SVM")
results <- classify_models(container, models)
# Cross Validation

analytics <- create_analytics(container, results)
summary(analytics)
##classifier <- naiveBayes(mat_training, sentiment)
##predicted <- predict(classifier, mat)
##predicted
#
#tweet_all <- as_tibble(tweet_all)
#tweet_all <- tweet_all %>% mutate(sentimen_nya = predicted)
#
results <- results %>% mutate(sentiment_nya = if_else(SVM_LABEL==1,"Negative","Positive"))
plotnya <- ggplot(data = results, aes(x=sentiment_nya, fill = sentiment_nya)) +
            geom_bar()
plotnya
