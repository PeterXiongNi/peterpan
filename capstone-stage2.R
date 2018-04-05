# Setup working directory
# setwd("~/Development/capstone/")
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
model <- list()
model$bi_gram <- build_ngram_model(data, 2)
model$tri_gram <- build_ngram_model(data, 3)
model$four_gram <- build_ngram_model(data, 4)
saveRDS(model, "model.rds")

input <- str_split("what is your", " ")
w1 <- sapply(input, "[", 1)
w2 <- sapply(input, "[", 2)
w3 <- sapply(input, "[", 3)


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

build_ngram_model <- function(data, n) {
  text <- data$text
  idx <- data$index
  chunkSize <- data$chunkSize
  rows <- floor(data$length / chunkSize)
  model <- initial_model(n)
  for (i in 1:rows) {
    t <- Sys.time()
    corpus <- corpus(text[idx[i,]])
    ngram_tokens <- tokens(corpus, "word",
                           remove_numbers = TRUE,
                           remove_punct = TRUE,
                           remove_symbols = TRUE,
                           remove_hyphens = TRUE,
                           remove_url = TRUE,
                           ngrams = n) %>%
                      tokens_tolower() %>%
                      tokens_select(stopwords("en"), selection = "remove")
    printf("chunk %d tokenized in %.3f s\n", i, Sys.time() - t)
    t <- system.time(chunk_model <- chunk_build_model(ngram_tokens, n))
    printf("chunk_model built with chunk %d in %.3f s\n", i, t[3])
    t <- system.time(model <- merge_chunks(model, chunk_model))
    printf("No.%d chunk_model merged with model in %.3f s\n\n\n", i, t[3])
  }
  t <- Sys.time()
  last_row_idx <- idx[rows + 1, ]
  last_row_idx <- last_row_idx[!is.na(last_row_idx)]
  corpus <- corpus(text[last_row_idx])
  ngram_tokens <- tokens(corpus, "word",
                        remove_numbers = TRUE,
                        remove_punct = TRUE,
                        remove_symbols = TRUE,
                        remove_hyphens = TRUE,
                        remove_url = TRUE,
                        remove_twitter = TRUE,
                        ngrams = n) %>%
                   tokens_tolower() %>%
                   tokens_select(stopwords("en"), selection = "remove")
  printf("chunk %d tokenized in %.3f s\n", rows + 1, Sys.time() - t)
  t <- system.time(chunk_model <- chunk_build_model(ngram_tokens, n))
  printf("chunk_model built with chunk %d in %.3f s\n", rows + 1, t[3])
  t <- system.time(model <- merge_chunks(model, chunk_model))
  printf("%d chunk_model merged with model in %.3f s\n\n\n", rows + 1, t[3])
  return(summarize_model(model, n))
}

initial_model <- function(n) {
  dt <- data.table(ngram = character(), n = integer(), w1 = character())
  if (n == 2) {
    dt <- dt[, w2 := character()]
  } else if (n == 3) {
    dt <- dt[, w2 := character()]
    dt <- dt[, w3 := character()]
  } else if (n == 4) {
    dt <- dt[, w2 := character()]
    dt <- dt[, w3 := character()]
    dt <- dt[, w4 := character()]
  }
}

chunk_build_model <- function(ngram_tokens, n) {
  dfm <- dfm(ngram_tokens)
  feature_fre <- colSums(dfm)
  dt <- data.table(ngram = names(feature_fre), n = feature_fre)
  tmp <- str_split(dt$ngram, pattern = "_")
  dt[, w1:= sapply(tmp, "[", 1)]
  if (n == 2) {
    dt[, w2:= sapply(tmp, "[", 2)]
  } else if (n == 3) {
    dt[, w2:= sapply(tmp, "[", 2)]
    dt[, w3:= sapply(tmp, "[", 3)]
  } else if (n == 4) {
    dt[, w2:= sapply(tmp, "[", 2)]
    dt[, w3:= sapply(tmp, "[", 3)]
    dt[, w4:= sapply(tmp, "[", 4)]
  }
  return(dt)
} 

summarize_model <- function(dt, n) {
  if (n == 4) {
    dt <- dt[, .(w4, n, p = round(n / sum(n), 6)), by = .(w1, w2, w3)]
    dt <- dt[order(-n)]
    dt[, n:=NULL]
    setkey(dt, w1, w2, w3)
  } else if (n == 3) {
    dt <- dt[, .(w3, n, p = round(n / sum(n), 6)), by = .(w1, w2)]
    dt <- dt[order(-n)]
    dt[, n:=NULL]
    setkey(dt, w1, w2)
  } else if (n == 2) {
    dt <- dt[, .(w2, n, p = round(n / sum(n), 6)), by = .(w1)]
    dt <- dt[order(-n)]
    dt[, n:=NULL]
    setkey(dt, w1)
  } else {
    dt <- dt[, .(w1, n, p = round(n / sum(n), 6))]
  }
  return(dt)
}

merge_chunks <- function(a, b) {
  setkey(a, ngram)
  setkey(b, ngram)
  a$n[a$ngram %in% b$ngram] <-  a$n[a$ngram %in% b$ngram] + b$n[b$ngram %in% a$ngram]
  a <- rbindlist(list(a, b[!ngram %in% a$ngram]))
  rm(b)
  setkey(a, ngram)
  a
}

predict <- function(model, w1, w2, w3) {
  #search 4gram, full match
  res <- model$four_gram[list(w1, w2, w3), .(w4, p)]
  if(nrow(res) > 0 & res[1, p] > 0) {
    return(res)
  }
  #backoff to 3 gramm model
  res <- model$tri_gram[list(w2, w3), .(w4, p)]
  if(nrow(res) > 0 & res[1, p] > 0) {
    return(res)
  }
  #backoff to bigrams
  res <- model$bi_gram[t3, .(w4, p)]
  if(nrow(res) > 0 & res[1, p] > 0) {
    return(res)
  }
  return (data.table(w4 = "the", p = 0))
}

test <- list()
test$text <- read_rds("rawData/test.rds")

