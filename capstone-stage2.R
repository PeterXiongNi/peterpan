# Setup working directory
# setwd("~/Documents/DataScience/Coursera_data_science_specialization/capstone_project")
library(quanteda)
library(data.table)
library(R.utils)
library(stringr)
library(readr)

# Read in, split data into train and test set and save into rds
files <- findFiles("en_US", "final/", recursive = TRUE, firstOnly = FALSE)
if (!dir.exists("rawData")) dir.create("rawData")
set.seed(142857)
training <- 0.7
testing <- 0.3
train <- c()
test <- c()
for (file in files) {
  dest <- str_extract(str_extract(file, "\\.[a-z]+\\."), "[a-z]+")
  data <- read_lines(file, progress = TRUE)
  sample_size <- floor(length(data) * training)
  train_index <- sample(1:length(data), sample_size)
  train <- c(train, data[train_index])
  test <- c(test, data[-train_index])
  saveRDS(data, sprintf("rawData/%s.rds", dest))
  print(paste("Saved", sprintf("rawData/%s.rds", dest)))
}
saveRDS(train, "rawData/train.rds")
saveRDS(test, "rawData/test.rds")

# Preprocessing data
data <- list()
data$text <- read_rds("rawData/train.rds")
data$chunkSize <- 50000
data$length <- length(data$text)
data$index <- index(data)
data$tokens <- pre_process(data)
if (!dir.exists("cleanData")) dir.create("cleanData")
saveRDS(data, "cleanData/tokenized_train.rds")

index <- function(data) {
  chunkSize <- data$chunkSize
  length <- data$length
  rows <- floor(length / chunkSize)
  idx_minus_last_row <- matrix(1:(chunkSize * rows), nrow = rows, ncol = chunkSize, byrow = TRUE)
  idx_last_row <- rep(NA, chunkSize)
  remaining_index <- length - chunkSize * rows
  idx_last_row[1:remaining_index] <- (rows * chunkSize  + 1):length
  if (all(is.na(idx_last_row))) 
    return(idx_minus_last_row) 
  return(rbind(idx_minus_last_row, idx_last_row))
}

pre_process <- function(data) {
  text <- data$text
  idx <- data$index
  chunkSize <- data$chunkSize
  rows <- floor(data$length / chunkSize)
  nGramTokens <- c()
  for (i in 1:rows) {
    t <- Sys.time()
    corpus <- corpus(text[idx[i,]])
    nGramTokens <- c(nGramTokens, 
                     tokens(corpus, "word",
                            remove_numbers = TRUE,
                            remove_punct = TRUE,
                            remove_symbols = TRUE,
                            remove_hyphens = TRUE,
                            remove_url = TRUE,
                            ngrams = 1:4) %>%
                       tokens_tolower() %>%
                       tokens_select(stopwords("en"), selection = "remove"))
    printf("chunk %d tokenized in %.3f s\n", i, Sys.time() - t)
  }
  t <- Sys.time()
  last_row_idx <- idx[rows + 1, ]
  last_row_idx <- last_row_idx[!is.na(last_row_idx)]
  corpus <- corpus(text[last_row_idx])
  nGramTokens <- c(nGramTokens, 
                   tokens(corpus, "word",
                          remove_numbers = TRUE,
                          remove_punct = TRUE,
                          remove_symbols = TRUE,
                          remove_hyphens = TRUE,
                          remove_url = TRUE,
                          ngrams = 1:4) %>%
                     tokens_tolower() %>%
                     tokens_select(stopwords("en"), selection = "remove"))
  printf("chunk %d tokenized in %.3f s\n", rows + 1, Sys.time() - t)
  return(nGramTokens)
}

# library(quanteda)
# library(data.table)
# library(R.utils)
# corpus <- corpus(text)
# nGramTokens <- tokens(corpus, "word",
#                       remove_numbers = TRUE, 
#                       remove_punct = TRUE, 
#                       remove_symbols = TRUE, 
#                       remove_hyphens = TRUE, 
#                       remove_url = TRUE,
#                       ngrams = 1:4) %>% 
#   tokens_tolower() %>% 
#   tokens_select(stopwords("en"), selection = "remove")
# dfm <- dfm(nGramTokens)
# featureFre <- colSums(dfm)
# 
# input <- readline(prompt = "Please type input string: ")
# preProcessInput <- function(input) {
#   token <- input %>% 
#     corpus() %>% 
#     tokens("word",
#            remove_numbers = TRUE, 
#            remove_punct = TRUE, 
#            remove_symbols = TRUE, 
#            remove_hyphens = TRUE, 
#            remove_url = TRUE) %>%
#     tokens_tolower()
#   length <- length(token$text1)
#   gram <- ifelse(length > 3, 3, length)
#   output <- as.numeric()
#   for (i in gram:1) {
#     nGramToken <- tokens_ngrams(token, i)
#     output <- c(output, nGramToken$text1[length(nGramToken$text1)])
#   }
#   return(output)
# }
# processedInput <- preProcessInput(input)
# prediction <- as.numeric()
# for (i in 1:length(processedInput)) {
#   pattern <- paste0("(^", processedInput[i], ")(_[a-z]+)$")
#   index <- grep(pattern, names(featureFre))
#   match <- sort(featureFre[index] / featureFre[processedInput[i]], decreasing = TRUE)[1:3]
#   names(match) <- sub(paste0("^", processedInput[i], "_"), "", names(match))
#   prediction <- c(prediction, match)
# }
# prediction


# while(notFinished) {
#   chunk <- readInChunk(files)
#   buildNGramModelonChunk(chunk)
#   conbineResult()
# }



