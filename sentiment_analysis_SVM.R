# Title     : TODO
# Objective : TODO
# Created by: Owner
# Created on: 1/24/2021
library(tidyverse)
library(RTextTools)
library(e1071)

svm <- function (tweet_all, positive, negative, length_data) {
  #buaat factor ada berapa banyak positive dan negativenya
  sentiment <- c(rep("positive", nrow(positive)), rep("negative", nrow(negative)))
  sentiment <- as.factor(sentiment)

  # menyatukan kalimat positif dan negatif jadi satu variabel
  dataDetail <- full_join(positive,negative)
  dataDetail <- as_tibble(dataDetail)

  # buat twitter jadi tibble biar bisa disatukan dengan dataDetail
  tweet_all <- as_tibble(tweet_all)

  # buat nama kolom value di dataDetail jadi text
  #colnames(dataDetail)[colnames(dataDetail)=="value"] <- "text"

  # gabungkan detailData dengan tweet menjadi dataframe
  df <- full_join(dataDetail,tweet_all)

  # buat matrix dari dataframe
  mat <- create_matrix(df, language = "english", removeStopwords = TRUE,
                     stemWords = TRUE, tm::weightTfIdf)
  mat <- as.matrix(mat)

  length_parameter <- 2000 + length_data
  # buat container dari matrix. disini data sentimen dengan tweet akan dipisahkan lagi
  container <- create_container(mat, as.numeric(sentiment), trainSize = 1:2000,
   testSize = 2001:length_parameter, virgin = TRUE)

  # latih modelnya dengan algoritma SVM
  models <- train_models(container, algorithms = "SVM")

  # prediksi tweet nya dan hasilkan
  results <- classify_models(container, models)

  results <- results %>% mutate(sentiment_nya = if_else(SVM_PROB >= 0.50 & SVM_PROB <= 0.51,"Neutral", if_else(SVM_LABEL==1,"Negative","Positive")))

  results <- results %>% mutate(tweet = tweet_all$value)

  results
}

set.seed(1234)
# panggil dataset kalimat positve negative yang sudah dibersihkan
positive <- read.csv("E:\\OneDrive\\SMT5\\Datsci\\proyekAkhir\\positive_negative_sentence\\positive_cleaned.csv")
negative <- read.csv("E:\\OneDrive\\SMT5\\Datsci\\proyekAkhir\\positive_negative_sentence\\negative_cleaned.csv")

# tentukan sentimen AMD
tweet_amd <- read.csv("amd_cleaned_baru.csv")
amd_result <- svm(tweet_amd,positive, negative, nrow(tweet_amd))
amd <- ggplot(data = amd_result, aes(x=sentiment_nya, fill = sentiment_nya)) +
            geom_bar() + labs(title = "AMD")

# tentukan sentimen NVDA
tweet_nvda <- read.csv("nvda_cleaned_baru.csv")
nvda_result <- svm(tweet_nvda,positive, negative,nrow(tweet_nvda))
nvda<- ggplot(data = nvda_result, aes(x=sentiment_nya, fill = sentiment_nya)) +
            geom_bar() + labs(title = "NVDA")

# tentukan sentimen GOOGL
tweet_googl <- read.csv("googles_cleaned.csv")
googl_result <- svm(tweet_googl,positive, negative,nrow(tweet_googl))
google <- ggplot(data = googl_result, aes(x=sentiment_nya, fill = sentiment_nya)) +
            geom_bar()+ labs(title = "GOOGL")

ggsave("google.png", plot = google)
ggsave("nvda.png", plot = nvda)
ggsave("amd.png", plot = amd)