library(tidyverse)
library(corrplot)
library(glmnet)
library(Matrix)
library(tidytext)
library(xgboost)
library(data.table)
library(Matrix)
library(patchwork)
library(caret)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(glasso)
library(igraph)
library(tm)
library(textstem)
library(caret)


# ----------------------- DATASET PREPARATION  ----------------------- 

data<-read.csv(file.choose())

data$X<-NULL
data$Unnamed..0<-NULL
data$Unnamed..0_x<-NULL

data$Unnamed..0_y<-NULL

data<-data[!data$genre_label=='',]
data<-data[!duplicated(data$track_uri),]

colnames(data)[13]<-"tempo_numeric"

data <- data[data$lyrics_IO==1,]
data$lyrics_IO <- NULL

data$genre_label<-as.factor(data$genre_label)
data$mode<-as.factor(data$mode)
data$key<-as.factor(data$key)


set.seed(1234)
train_dim<-round(NROW(data)*0.8)
random<-sample(1:NROW(data), train_dim)

data_train<-data[random,]
data_test<-data[-random,]

y<-as.factor(data$genre_label)
y.train <- y[random]
y.test <- y[-random]


# in order to clean up the text all these artifacts 
create_corpus <- function(df){
  word_dataset_1<-tibble(text=df$lyric)
  
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\w*Lyrics\\w*", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\w*Contributors\\w*\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\w*contributors\\w*\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\Contributors\\w*\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\contributors\\w*\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="Lyrics", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="lyrics", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\w*Türkçe\\w*\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\w*bokmål\\w*\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\w*Embed\\b", replacement="", ignore.case=T))
  
  
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="[[:punct:]]", replacement="", ignore.case=T))
  
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="Chorus", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="chorus", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="Verse", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="verse", replacement="", ignore.case=T))
  
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\[.*?\\]", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\(.*?\\)", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\w*\n\\w*", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="[\r\n]", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\w*Contributor\\w*\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\w*contributor\\w*\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\Contributor\\w*\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\contributor\\w*\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="[[:digit:]]+", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\s+", replacement=" ", ignore.case=T))
  
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="[[:digit:]]", replacement="", ignore.case=T))
  
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\bah\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\boh\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\buh\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\bhuh\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\bna\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\byeah\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\bem\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\bmm\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\bwhoa\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="also", replacement="", ignore.case=T))
  word_dataset_1 <- word_dataset_1 %>% mutate(text = str_replace_all(text, pattern = "[^A-Za-z\\s]", replacement = ""))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="Travi", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="Scott", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\boh\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\booh\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="\\boohooh\\b", replacement="", ignore.case=T))
  word_dataset_1<-word_dataset_1 %>% mutate(text=sapply(text, FUN=gsub, pattern="lllove", replacement="love", ignore.case=T))
  
  # Remove Japanese, Korean, and special characters
  
  x<-VectorSource(word_dataset_1$text)
  corpus<-tm::VCorpus(x, readerControl = list(reader = reader(x), language = c('en')))
  # Remove Japanese, Korean, and special characters
  
  removeSpecialCharacters <- function(x) {
    x <- gsub("[\u3040-\u309F]|[\u30A0-\u30FF]|[\uFF00-\uFF9F]|[\u4E00-\u9FAF]", "", x) # Remove Japanese characters
    x <- gsub("[\uAC00-\uD7A3]", "", x) # Remove Korean characters
    gsub("[^[:alnum:][:space:]]*", "", x) # Remove special characters
  }
  corpus <- tm_map(corpus, content_transformer(removeSpecialCharacters))
  
  corpus <- tm_map(corpus, content_transformer(removeSpecialCharacters))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords::stopwords("en", source = "stopwords-iso"))
  
  
  corpus <- tm_map(corpus, stemDocument, language = c('english'))
  
  corpus <- tm_map(corpus, content_transformer(lemmatize_words), language='en')
  
  return(corpus)
}

# split all corpus in train and test 

corpus_train <- create_corpus(data_train)
corpus_test <- create_corpus(data_test)


dtm_train<-DocumentTermMatrix(corpus_train)


tfidf_train <- weightTfIdf(dtm_train)

tfidf_test <- DocumentTermMatrix(corpus_test, control = list(dictionary = Terms(tfidf_train)))
# lyrics_test <- as.matrix(tfidf_test)
# lyrics_train <- as.matrix(tfidf_train)



gc()

# ----------------------- DATASET PREPARATION  ----------------------- 

data$genre_label<-as.factor(data$genre_label)
data$mode<-as.factor(data$mode)
data$key<-as.factor(data$key)

lyrics_train<-Matrix(as.matrix(tfidf_train), sparse = TRUE)
lyrics_test<-Matrix(as.matrix(tfidf_test), sparse = TRUE)

y<-as.factor(data$genre_label)

# --------------------------------- graph -----------------------------


word_freq <- col_sums(as.matrix(dtm))
word_freq_df <- tibble(word = (dtm_train$dimnames$Terms), freq = apply(as.matrix(dtm_train), 2, sum))


word_freq_df_rock<-tibble(word = (dtm_train[y[random]=='rock',]$dimnames$Terms),
                          freq = apply(as.matrix(dtm_train)[y[random]=='rock',], 2, sum))

word_freq_df_rock %>% filter(freq>150) %>% ggplot(aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_minimal() +
  labs(title = "Word Frequency Barplot rock songs",
       x = "Words",
       y = "Frequency")+ coord_flip()

word_freq_df_pop<-tibble(word = (dtm_train[y[random]=='pop',]$dimnames$Terms),
                          freq = apply(as.matrix(dtm_train)[y[random]=='pop',], 2, sum))

word_freq_df_pop %>% filter(freq>150) %>% ggplot(aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "#82B91D") +
  theme_minimal() +
  labs(title = "Word Frequency Barplot pop songs",
       x = "Words",
       y = "Frequency")+ coord_flip()

word_freq_df_rap<-tibble(word = (dtm_train[y[random]=='rap',]$dimnames$Terms),
                         freq = apply(as.matrix(dtm_train)[y[random]=='rap',], 2, sum))

word_freq_df_rap %>% filter(freq>250) %>% ggplot(aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "#1D82B9") +
  theme_minimal() +
  labs(title = "Word Frequency Barplot rap songs",
       x = "Words",
       y = "Frequency")+ coord_flip()


word_freq_df %>% filter(freq>500) %>% ggplot(aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "#1DB954") +
  theme_minimal() +
  labs(title = "Word Frequency Barplot",
       x = "Words",
       y = "Frequency")+ coord_flip()

# tf idf

word_tfidf_df<-tibble(word = (dtm_train$dimnames$Terms), tfidf = apply(as.matrix(lyrics_train), 2, mean))

word_tfidf_df %>% filter(tfidf>0.015) %>% ggplot(aes(x = reorder(word, tfidf), y = tfidf)) +
  geom_bar(stat = "identity", fill = "#1DB954") +
  theme_minimal() +
  labs(title = "Word tf-idf Barplot",
       x = "Words",
       y = "tf-idf")+ coord_flip()

word_tfidf_df_rock<-tibble(word = (dtm_train[y[random]=='rock',]$dimnames$Terms), tfidf = apply(as.matrix(lyrics_train[y[random]=='rock',]), 2, mean))

word_tfidf_df_rock %>% filter(tfidf>0.015) %>% ggplot(aes(x = reorder(word, tfidf), y = tfidf)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_minimal() +
  labs(title = "Word tf-idf Barplot for rock",
       x = "Words",
       y = "tf-idf")+ coord_flip()

word_tfidf_df_rap<-tibble(word = (dtm_train[y[random]=='rap',]$dimnames$Terms), tfidf = apply(as.matrix(lyrics_train[y[random]=='rap',]), 2, mean))

word_tfidf_df_rap %>% filter(tfidf>0.015) %>% ggplot(aes(x = reorder(word, tfidf), y = tfidf)) +
  geom_bar(stat = "identity", fill = "#1D82B9") +
  theme_minimal() +
  labs(title = "Word tf-idf Barplot for rap",
       x = "Words",
       y = "tf-idf")+ coord_flip()

word_tfidf_df_pop<-tibble(word = (dtm_train[y[random]=='pop',]$dimnames$Terms), tfidf = apply(as.matrix(lyrics_train[y[random]=='pop',]), 2, mean))

word_tfidf_df_pop %>% filter(tfidf>0.015) %>% ggplot(aes(x = reorder(word, tfidf), y = tfidf)) +
  geom_bar(stat = "identity", fill = "#82B91D") +
  theme_minimal() +
  labs(title = "Word tf-idf Barplot for pop",
       x = "Words",
       y = "tf-idf")+ coord_flip()

# Find the frequency of words
word_freqs <- sort(colSums(as.matrix(dtm_train)), decreasing=TRUE) 

# Create a data frame of words and their frequencies
df <- data.frame(word=names(word_freqs), freq=word_freqs)

# Generate the word cloud of most 100 frequent words
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Find positions where y is "rock"
rock_positions <- which(y[random] == "rock")
print(paste("Rock positions: ", toString(rock_positions)))

# Find positions where y is "pop"
pop_positions <- which(y[random] == "pop")
print(paste("Pop positions: ", toString(pop_positions)))

# Find positions where y is "rap"
rap_positions <- which(y[random] == "rap")
print(paste("Rap positions: ", toString(rap_positions)))

# Slice DTM for "rock"
dtm_rock <- dtm_train[rock_positions, ]

# Slice DTM for "pop"
dtm_pop <- dtm_train[pop_positions, ]

# Slice DTM for "rap"
dtm_rap <- dtm_train[rap_positions, ]


# Find the frequency of words
word_freqs_rock <- sort(colSums(as.matrix(dtm_rock)), decreasing=TRUE) 

# Create a data frame of words and their frequencies
df_rock <- data.frame(word=names(word_freqs_rock), freq=word_freqs_rock)

# Generate the word cloud of most 100 frequent words
wordcloud(words = df_rock$word, freq = df_rock$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
title("Worldcloud for rock genre")

# Find the frequency of words
word_freqs_pop <- sort(colSums(as.matrix(dtm_pop)), decreasing=TRUE) 

# Create a data frame of words and their frequencies
df_pop <- data.frame(word=names(word_freqs_pop), freq=word_freqs_pop)

# Generate the word cloud of most 100 frequent words
wordcloud(words = df_pop$word, freq = df_pop$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
title("Worldcloud for pop genre")

# Find the frequency of words
word_freqs_rap <- sort(colSums(as.matrix(dtm_rap)), decreasing=TRUE) 

# Create a data frame of words and their frequencies
df_rap <- data.frame(word=names(word_freqs_rap), freq=word_freqs_rap)

# Generate the word cloud of most 100 frequent words
wordcloud(words = df_rap$word, freq = df_rap$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"),  size=1.6)
title("Worldcloud for rap genre")


# NUMERICAL VARIABLES FROM SPOTIFY
# Load the necessary library
library(ggplot2)

# Numerical variables from Spotify API
variables <- c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo_numeric")

# Loop over the variables
for (var in variables) {
  # Histogram
  p1 <- ggplot(data_train, aes(!!sym(var))) + 
    geom_histogram(binwidth=0.5, fill="blue", color="black") +
    labs(title=paste("Histogram of", var), x=var, y="Frequency")
  
  # Boxplot
  p2 <- ggplot(data_train, aes(!!sym(var))) + 
    geom_boxplot(fill="lightgreen", color="black") +
    labs(title=paste("Boxplot of", var), x=var, y="Value")+theme_minimal()
  
  # Plots
  print(p1)
  print(p2)
}

for (var in variables) {
  # Boxplot for each genre_label category
  p <- ggplot(data_train, aes_string(x = "genre_label", y = var, fill="genre_label")) + 
    geom_boxplot() +
    labs(title=paste("Boxplot of", var, "by genre"), x="Genre Label", y=var)+
    scale_fill_manual(values = c("rap" = "#82B91D", "pop" = "#1D82B9", "rock" = "lightblue"))+theme_minimal()
  
  # Plot
  print(p)
}

prop.table(table(data_train$mode, data_train$genre_label),1)

prop.table(table(data_train$key, data_train$genre_label),1)

# --------------------------- training testing set ----------------------

data_train_model_matrix<-model.matrix(~.-1, data=data_train[,-c(1,2,3,16,17,18,19)])

data_test_model_matrix<-model.matrix(~.-1, data=data_test[,-c(1,2,3,16,17,18,19)])

train_sparse_matrix<-cbind(data_train_model_matrix, lyrics_train)

test_sparse_matrix<-cbind(data_test_model_matrix, lyrics_test)

# #Extract corpus for NN
# Extraction for NN 
#Extraction of training and test set 

#train_df <- cbind(as.data.frame((as.matrix(train_sparse_matrix))),y.train)
#write_csv(train_df,file="train_df.csv")
#test_df <- cbind(as.data.frame((as.matrix(test_sparse_matrix))),y.test)
#write_csv(test_df,file="test_df.csv")


#Extraction of scaled numerical features for NN

#train_matrix <- as.matrix(train_sparse_matrix)
#scaled_v <- c(1,2,15,16,17,18,19,20,21,23)
#mean_train <- apply(train_matrix[,scaled_v],2,mean)
#std_train <- apply(train_matrix[,scaled_v],2,sd)

# Create dictionaries
#mean_dict <- setNames(as.list(mean_train), scaled_v)
#std_dict <- setNames(as.list(std_train), scaled_v)

#test_matrix <- as.matrix(test_sparse_matrix)
#for (i in names(mean_dict)){
#  col_index <- as.integer(i)
#  mean_value <- unlist(mean_dict[i])
#  std_value <- unlist(std_dict[i])

#  train_matrix[, col_index] <- (train_matrix[, col_index] - mean_value) / std_value
#  test_matrix[, col_index] <- (test_matrix[, col_index] - mean_value) / std_value
#}
#num_features_tr <- train_matrix[,seq(1,23)]
#num_features_tr <- data.frame(num_features_tr, stringsAsFactors = F)
#write_csv(num_features_tr, file="num_features_tr.csv")

#num_features_test <- test_matrix[,seq(1,23)]
#num_features_test <- data.frame(num_features_test, stringsAsFactors = F)
#write_csv(num_features_test, file="num_features_test.csv")

# texts_tr <- sapply(corpus_train, as.character)
# corpus_tr <- data.frame(texts_tr, stringsAsFactors = F)
# write_csv(corpus_tr, file="corpus_tr.csv")
# 
# texts_test <- sapply(corpus_test, as.character)
# corpus_ts <- data.frame(texts_test, stringsAsFactors = F)
# write_csv(corpus_ts, file="corpus_test.csv")


# ------------------------ CROSS VALIDATION ON DIFFERENT MODELS ------------------------

foldid <- sample(1:10, size = length(y[random]), replace = TRUE)
cv1  <- cv.glmnet(train_sparse_matrix, y[random], foldid = foldid, alpha = 1 ,family='multinomial')
cv.8  <- cv.glmnet(train_sparse_matrix, y[random], foldid = foldid, alpha = 0.75 ,family='multinomial')
cv.5 <- cv.glmnet(train_sparse_matrix, y[random], foldid = foldid, alpha = 0.5 ,family='multinomial')
cv.4 <- cv.glmnet(train_sparse_matrix, y[random], foldid = foldid ,alpha = 0.25 ,family='multinomial')
cv0  <- cv.glmnet(train_sparse_matrix, y[random], foldid = foldid,lambda.min.ratio=0.00000001 ,alpha = 0 ,family='multinomial')

par(mfrow = c(3,2))
plot(cv1); plot(cv.8) ;plot(cv.5); plot(cv.4) ; plot(cv0);
par(mfrow=c(1,1))
plot(log(cv1$lambda)   , cv1$cvm , pch = 19, col = "red",
     xlab = "log(Lambda)", ylab = cv1$name, xlim=c(-6,2), ylim=c(1.2, 3),main=c("elastic-net family comparison - multinomial deviance"))
points(log(cv.8$lambda), cv.8$cvm, pch = 19, col = "grey")
points(log(cv.5$lambda), cv.5$cvm, pch = 19, col = "green")
points(log(cv.4$lambda) , cv.4$cvm , pch = 19, col = "blue")
points(log(cv0$lambda), cv0$cvm, pch = 19, col = "purple")
#legend('topleft', c("alpha=1", "alpha=0.75", "alpha=0.5", "alpha=0.25", "alpha=0"))
legend("topright",legend = c("Lasso regression", 
                             expression(paste( alpha, " = 0.75")),
                             expression(paste( alpha, " = 0.5")),
                             expression(paste(alpha, " = 0.25")),
                             "Ridge regression"),
       col = c("red", "grey", "green", "blue", "purple"),
       pch = 19)
# cross validation for a grid of 5 values of alpha: we habe extracted the minimum and one std 
# error MSE values and their respective lambda value

pd <- position_dodge(0.1) # move them .05 to the left and right
tibble(se=c(cv0$cvsd[which.min(cv0$cvm)], cv.4$cvsd[which.min(cv.4$cvm)], cv.5$cvsd[which.min(cv.5$cvm)], cv.8$cvsd[which.min(cv.8$cvm)], cv1$cvsd[which.min(cv1$cvm)]),
       m=c(min(cv0$cvm), min(cv.4$cvm), min(cv.5$cvm), min(cv.8$cvm), min(cv1$cvm))) %>% 
  ggplot(aes(x=c(0,0.25,0.5,0.75,1), y=m)) + labs(y= "multinomial deviance", x = "alpha")+
  ggtitle("multinomial deviance vs alpha")+
  geom_errorbar(aes(ymin=m-se, ymax=m+se), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)


# it seems that the best model is between the lasso and he elasticnet with alpha=.85
# this make sense since we expect some kind of correlation between the covariates

foldid <- sample(1:10, size = length(y[random]), replace = TRUE)
cv1_class <- cv.glmnet(train_sparse_matrix, y[random], foldid = foldid, alpha = 1,family='multinomial', type.measure = 'class')
cv.8_class<- cv.glmnet(train_sparse_matrix, y[random], foldid = foldid, alpha = 0.75 ,family='multinomial', type.measure = 'class')
cv.5_class <- cv.glmnet(train_sparse_matrix, y[random], foldid = foldid, alpha = 0.5 ,family='multinomial', type.measure = 'class')
cv.4_class <- cv.glmnet(train_sparse_matrix, y[random], foldid = foldid, alpha = 0.25,family='multinomial', type.measure = 'class')
cv0_class  <- cv.glmnet(train_sparse_matrix, y[random], foldid = foldid, alpha = 0, lambda.min.ratio=0.00001 ,family='multinomial', type.measure = 'class')


par(mfrow = c(3,2))
plot(cv1_class); plot(cv.8_class) ;plot(cv.5_class); plot(cv.4_class) ; plot(cv0_class);
par(mfrow=c(1,1))
plot(log(cv1_class$lambda)   , cv1_class$cvm , pch = 19, col = "red",
     xlab = "log(Lambda)", ylab = cv1_class$name, xlim=c(-6,4.5),
     main=c("elastic-net family comparison - misclassification error"))
points(log(cv.8_class$lambda), cv.8_class$cvm, pch = 19, col = "grey")
points(log(cv.5_class$lambda), cv.5_class$cvm, pch = 19, col = "green")
points(log(cv.4_class$lambda) , cv.4_class$cvm , pch = 19, col = "blue")
points(log(cv0_class$lambda), cv0_class$cvm, pch = 19, col = "purple")
legend("topright",legend = c("Lasso regression", 
                             expression(paste( alpha, " = 0.75")),
                             expression(paste( alpha, " = 0.5")),
                             expression(paste(alpha, " = 0.25")),
                             "Ridge regression"),
       col = c("red", "grey", "green", "blue", "purple"),
       pch = 19)

pd <- position_dodge(0.1) # move them .05 to the left and right
tibble(se=c(cv0_class$cvsd[which.min(cv0_class$cvm)], cv.4_class$cvsd[which.min(cv.4_class$cvm)], cv.5_class$cvsd[which.min(cv.5_class$cvm)], cv.8_class$cvsd[which.min(cv.8_class$cvm)], cv1_class$cvsd[which.min(cv1_class$cvm)]),
       m=c(min(cv0_class$cvm), min(cv.4_class$cvm), min(cv.5_class$cvm), min(cv.8_class$cvm), min(cv1_class$cvm))) %>% 
  ggplot(aes(x=c(0,0.25,0.5,0.75,1), y=m)) + labs(y= "misclassification error", x = "alpha")+
  ggtitle("misclassification error vs alpha")+
  geom_errorbar(aes(ymin=m-se, ymax=m+se), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)

c(cv1$lambda.min, cv1_class$lambda.min)
c(cv1$lambda.1se, cv1_class$lambda.1se)

confusion.glmnet(cv1, newx = test_sparse_matrix, newy = y[-random])


# ----------------------- LASSO MULTINOMIAL LOGISTIC CLASSIFIER  ----------------------- 

fit_log_1<-glmnet(x=train_sparse_matrix, y=y[random], family = 'multinomial')

par(mfrow=c(1,3))
plot(fit_log_1, label=TRUE)

cv.fit<-cv.glmnet(train_sparse_matrix, y[random], nfold=10, family='multinomial')
par(mfrow=c(1,1))
plot(cv.fit)

par(mfrow=c(1,3))
#abline(v=log(cv.fit$lambda.min), col='red')
plot(fit_log_1, xvar='lambda')

#best_mod<-glmnet(x=train_sparse_matrix, y=as.factor(y[random]), family = 'multinomial', lambda = cv.fit$lambda.min)

optimal_lambda <- cv.fit$lambda.1se

coef.est1 <- as.matrix(glmnet::coef.glmnet(cv.fit, s=cv.fit$lambda.1se))

class_names <- attr(coef.est1, "dimnames")[[1]]

par(mfrow=c(1,3))
plot(fit_log_1, xvar='lambda')

fit_log_1_ridge<-glmnet(x=train_sparse_matrix, y=y[random], family = 'multinomial', alpha = 0)

par(mfrow=c(1,3))
plot(fit_log_1_ridge)


fit_log_1_el<-glmnet(x=train_sparse_matrix, y=y[random], family = 'multinomial', alpha = 0.75)

par(mfrow=c(1,3))
plot(fit_log_1_el)


# Itera su ogni classe (ogni elemento della lista)
for(i in 1:length(coef.est1)) {
  cat("Coefficients for class", class_names[i], ":\n")
  
  # Coefficienti per la classe corrente
  coefs_current_class <- coef.est1[[i]]
  
  # Identifica gli indici dei coefficienti non nulli
  non_zero_indices <- which(coefs_current_class@x != 0)
  
  # Estrai i nomi delle variabili e i coefficienti corrispondenti
  non_zero_coefs <- data.frame(
    Variable = rownames(coefs_current_class)[coefs_current_class@i[non_zero_indices] + 1],
    Coefficient = coefs_current_class@x[non_zero_indices])
  
  if(i==1){a<-tibble(var=rownames(coefs_current_class)[coefs_current_class@i[non_zero_indices] + 1], 
                     genre=  class_names[i])}
  else{a<-a %>% add_row(var=rownames(coefs_current_class)[coefs_current_class@i[non_zero_indices] + 1], 
                        genre=  class_names[i])}
  
  # Stampa i risultati
  print(non_zero_coefs)
  cat("\n")
}

# here there is some plot for comparison between different coefficients
coef_lasso_min<-coef(cv1, s='lambda.min')

coef_lasso<-coef(cv1, s='lambda.1se')

# all the parameters


colnames(train_sparse_matrix)[coef_lasso$pop@i]
selected_word_dataframe_lasso_pop<-tibble(words=colnames(train_sparse_matrix)[coef_lasso$pop@i][-c(1,2,3,4)])
selected_word_dataframe_lasso_pop<-selected_word_dataframe_lasso_pop %>% add_column(value=coef_lasso$pop@x[-c(1,2,3,4,5)])

ggplot(selected_word_dataframe_lasso_pop, aes(x = reorder(words, value), y = value)) +
  geom_bar(stat = "identity", fill = "#82B91D") +coord_flip()+
  labs(title = "Coefficients of words pop", y = "value", x = "word")

colnames(train_sparse_matrix)[coef_lasso$rap@i]
selected_word_dataframe_lasso_rap<-tibble(words=colnames(train_sparse_matrix)[coef_lasso$rap@i][-c(1,2,3)])
selected_word_dataframe_lasso_rap<-selected_word_dataframe_lasso_rap %>% add_column(value=coef_lasso$rap@x[-c(1,2,3,4)])

ggplot(selected_word_dataframe_lasso_rap, aes(x = reorder(words, value), y =value)) +
  geom_bar(stat = "identity", fill = "#1D82B9") +coord_flip()+
  labs(title = "Coefficients of words rap", y = "value", x = "word")

colnames(train_sparse_matrix)[coef_lasso$rock@i]
selected_word_dataframe_lasso_rock<-tibble(words=colnames(train_sparse_matrix)[coef_lasso$rock@i][-c(1,2,3,4,5,6,7,8,9)])
selected_word_dataframe_lasso_rock<-selected_word_dataframe_lasso_rock %>% add_column(value=coef_lasso$rock@x[-c(1,2,3,4,5,6,7,8,9,10)])

ggplot(selected_word_dataframe_lasso_rock, aes(x = reorder(words, value), y = value)) +
  geom_bar(stat = "identity", fill = "lightblue") +coord_flip()+
  labs(title = "Coefficients of words rock", y = "value", x = "word")


numeric_coeff_lasso_pop<-tibble(coef=colnames(train_sparse_matrix)[coef_lasso$pop@i][c(1,2,3,4)],
                                value=coef_lasso$pop@x[c(2,3,4,5)])

numeric_coeff_lasso_rap<-tibble(coef=colnames(train_sparse_matrix)[coef_lasso$rap@i][c(1,2,3)],
                                value=coef_lasso$rap@x[c(2,3,4)])

numeric_coeff_lasso_rock<-tibble(coef=colnames(train_sparse_matrix)[coef_lasso$rock@i][c(1,2,3,4,5,6,7,8,9)],
                                 value=coef_lasso$rock@x[c(2,3,4,5,6,7,8,9,10)])


coef_lasso_min<-coef(cv1, s='lambda.min')


colnames(train_sparse_matrix)[coef_lasso_min$pop@i]
selected_word_dataframe_lasso_min_pop<-tibble(words=colnames(train_sparse_matrix)[coef_lasso_min$pop@i][-c(1,2,3,4)])
selected_word_dataframe_lasso_min_pop<-selected_word_dataframe_lasso_min_pop %>% add_column(value=coef_lasso_min$pop@x[-c(1,2,3,4,5)])

ggplot(selected_word_dataframe_lasso_min_pop, aes(x = reorder(words, value), y = value)) +
  geom_bar(stat = "identity", fill = "#1DB954") +coord_flip()+
  labs(title = "Coefficients of words pop", y = "value", x = "word")

colnames(train_sparse_matrix)[coef_lasso_min$rap@i]
selected_word_dataframe_lasso_min_rap<-tibble(words=colnames(train_sparse_matrix)[coef_lasso_min$rap@i][-c(1,2,3)])
selected_word_dataframe_lasso_min_rap<-selected_word_dataframe_lasso_min_rap %>% add_column(value=coef_lasso_min$rap@x[-c(1,2,3,4)])

ggplot(selected_word_dataframe_lasso_min_rap, aes(x = reorder(words, value), y = value)) +
  geom_bar(stat = "identity", fill = "#1DB954") +coord_flip()+
  labs(title = "Coefficients of words rap", y = "value", x = "word")

colnames(train_sparse_matrix)[coef_lasso_min$rock@i]
selected_word_dataframe_lasso_min_rock<-tibble(words=colnames(train_sparse_matrix)[coef_lasso_min$rock@i][-c(1,2,3,4,5,6,7,8,9)])
selected_word_dataframe_lasso_min_rock<-selected_word_dataframe_lasso_min_rock %>% add_column(value=coef_lasso_min$rock@x[-c(1,2,3,4,5,6,7,8,9,10)])

ggplot(selected_word_dataframe_lasso_min_rock, aes(x = reorder(words, value), y = value)) +
  geom_bar(stat = "identity", fill = "#1DB954") +coord_flip()+
  labs(title = "Coefficients of words rock", y = "value", x = "word")

# ---------------------------- GRAPHICAL MODELS ----------------------------

coef.est1 <- as.matrix(glmnet::coef.glmnet(cv1, s='lambda.1se'))

class_names <- attr(coef.est1, "dimnames")[[1]]


# Itera su ogni classe (ogni elemento della lista)
for(i in 1:length(coef.est1)) {
  cat("Coefficients for class", class_names[i], ":\n")
  
  # Coefficienti per la classe corrente
  coefs_current_class <- coef.est1[[i]]
  
  # Identifica gli indici dei coefficienti non nulli
  non_zero_indices <- which(coefs_current_class@x != 0)
  
  # Estrai i nomi delle variabili e i coefficienti corrispondenti
  non_zero_coefs <- data.frame(
    Variable = rownames(coefs_current_class)[coefs_current_class@i[non_zero_indices] + 1],
    Coefficient = coefs_current_class@x[non_zero_indices])
  
  if(i==1){a<-tibble(var=rownames(coefs_current_class)[coefs_current_class@i[non_zero_indices] + 1], 
                     genre=  class_names[i])}
  else{a<-a %>% add_row(var=rownames(coefs_current_class)[coefs_current_class@i[non_zero_indices] + 1], 
                        genre=  class_names[i])}
  
  # Stampa i risultati
  print(non_zero_coefs)
  cat("\n")
}


a<-a[!a$var=='(Intercept)',]
a<-a[!a$var=='loudness',]
a<-a[!a$var=='acousticness',]
a<-a[!a$var=='duration',]
a<-a[!a$var=='energy',]
a<-a[!a$var=='speechiness',]
a<-a[!a$var=='valence',]
a<-a[!a$var=='danceability',]
a<-a[!a$var=='instrumentalness',]
a<-a[!a$var=='mode1',]
a<-a[!a$var=='key4',]
a<-a[!a$var=='key2',]

name_pop<-rep(0,dim(a[a$genre=='pop',])[1])

for(i in 1:dim(a[a$genre=='pop',])[1]){
  if(i==1){
    name_pop[i]<-a[a$genre=='pop',]$var[i]
    sub_lyric_pop<-lyrics_train[,lyrics_train@Dimnames$Terms==a[a$genre=='pop',]$var[i]]
  }
  else{
    name_pop[i]<-a[a$genre=='pop',]$var[i]
    sub_lyric_pop<-cbind(sub_lyric_pop,lyrics_train[,lyrics_train@Dimnames$Terms==a[a$genre=='pop',]$var[i]])
  }
}

name_rap<-rep(0,dim(a[a$genre=='rap',])[1])

for(i in 1:dim(a[a$genre=='rap',])[1]){
  if(i==1){
    name_rap[i]<-a[a$genre=='rap',]$var[i]
    sub_lyric_rap<-lyrics_train[,lyrics_train@Dimnames$Terms==a[a$genre=='rap',]$var[i]]
  }
  else{
    name_rap[i]<-a[a$genre=='rap',]$var[i]
    sub_lyric_rap<-cbind(sub_lyric_rap,lyrics_train[,lyrics_train@Dimnames$Terms==a[a$genre=='rap',]$var[i]])
  }
}

name_rock<-rep(0,dim(a[a$genre=='rock',])[1])

for(i in 1:dim(a[a$genre=='rock',])[1]){
  if(i==1){
    name_rock[i]<-a[a$genre=='rock',]$var[i]
    sub_lyric_rock<-lyrics_train[,lyrics_train@Dimnames$Terms==a[a$genre=='rock',]$var[i]]
  }
  else{
    name_rock[i]<-a[a$genre=='rock',]$var[i]
    sub_lyric_rock<-cbind(sub_lyric_rock,lyrics_train[,lyrics_train@Dimnames$Terms==a[a$genre=='rock',]$var[i]])
  }
}

c<-glassopath(var(sub_lyric_rap))

# 0.0002256231

gl<-glasso(var(sub_lyric_rap), rho=0.0002)

g <- graph_from_adjacency_matrix(gl$w, mode = "undirected", weighted = TRUE)
LO = layout_with_fr(g)
Isolated = which(degree(g)==2)
G2 = delete.vertices(g, Isolated)
LO2 = LO[-Isolated,]
name_rap<-name_rap[-Isolated]

plot(simplify(G2), layout=LO2, vertex.label=name_rap)
title("Network of word coefficients lambda_1se rap")


gl<-glasso(var(as.matrix(sub_lyric_pop)), rho=0.0002)

g <- graph_from_adjacency_matrix(gl$w, mode = "undirected", weighted = TRUE)
LO = layout_with_fr(g)
Isolated = which(degree(g)==2)
G2 = delete.vertices(g, Isolated)
LO2 = LO[-Isolated,]
name_pop<-name_pop[-Isolated]
plot(simplify(G2), layout=LO2, vertex.label=name_pop)
title("Network of word coefficients lambda_1se pop")



coef.est1 <- as.matrix(glmnet::coef.glmnet(cv1, s='lambda.min'))

class_names <- attr(coef.est1, "dimnames")[[1]]

for(i in 1:length(coef.est1)) {
  cat("Coefficients for class", class_names[i], ":\n")
  
  coefs_current_class <- coef.est1[[i]]
  
  non_zero_indices <- which(coefs_current_class@x != 0)
  
  non_zero_coefs <- data.frame(
    Variable = rownames(coefs_current_class)[coefs_current_class@i[non_zero_indices] + 1],
    Coefficient = coefs_current_class@x[non_zero_indices])
  
  if(i==1){a<-tibble(var=rownames(coefs_current_class)[coefs_current_class@i[non_zero_indices] + 1], 
                     genre=  class_names[i])}
  else{a<-a %>% add_row(var=rownames(coefs_current_class)[coefs_current_class@i[non_zero_indices] + 1], 
                        genre=  class_names[i])}
  
  print(non_zero_coefs)
  cat("\n")
}



a<-a[!a$var=='(Intercept)',]
a<-a[!a$var=='loudness',]
a<-a[!a$var=='acousticness',]
a<-a[!a$var=='duration',]
a<-a[!a$var=='energy',]
a<-a[!a$var=='speechiness',]
a<-a[!a$var=='valence',]
a<-a[!a$var=='danceability',]
a<-a[!a$var=='instrumentalness',]
a<-a[!a$var=='mode1',]
a<-a[!a$var=='key4',]
a<-a[!a$var=='key2',]

name_pop<-rep(0,dim(a[a$genre=='pop',])[1])

for(i in 1:dim(a[a$genre=='pop',])[1]){
  if(i==1){
    name_pop[i]<-a[a$genre=='pop',]$var[i]
    sub_lyric_pop<-lyrics_train[,lyrics_train@Dimnames$Terms==a[a$genre=='pop',]$var[i]]
  }
  else{
    name_pop[i]<-a[a$genre=='pop',]$var[i]
    sub_lyric_pop<-cbind(sub_lyric_pop,lyrics_train[,lyrics_train@Dimnames$Terms==a[a$genre=='pop',]$var[i]])
  }
}

name_rap<-rep(0,dim(a[a$genre=='rap',])[1])

for(i in 1:dim(a[a$genre=='rap',])[1]){
  if(i==1){
    name_rap[i]<-a[a$genre=='rap',]$var[i]
    sub_lyric_rap<-lyrics_train[,lyrics_train@Dimnames$Terms==a[a$genre=='rap',]$var[i]]
  }
  else{
    name_rap[i]<-a[a$genre=='rap',]$var[i]
    sub_lyric_rap<-cbind(sub_lyric_rap,lyrics_train[,lyrics_train@Dimnames$Terms==a[a$genre=='rap',]$var[i]])
  }
}

name_rock<-rep(0,dim(a[a$genre=='rock',])[1])

for(i in 1:dim(a[a$genre=='rock',])[1]){
  if(i==1){
    name_rock[i]<-a[a$genre=='rock',]$var[i]
    sub_lyric_rock<-lyrics_train[,lyrics_train@Dimnames$Terms==a[a$genre=='rock',]$var[i]]
  }
  else{
    name_rock[i]<-a[a$genre=='rock',]$var[i]
    sub_lyric_rock<-cbind(sub_lyric_rock,lyrics_train[,lyrics_train@Dimnames$Terms==a[a$genre=='rock',]$var[i]])
  }
}

c<-glassopath(var(sub_lyric_rap))

gl<-glasso(var(sub_lyric_rap), rho=0.0002)

g <- graph_from_adjacency_matrix(gl$w, mode = "undirected", weighted = TRUE)
LO = layout_with_fr(g)
Isolated = which(degree(g)==2)
G2 = delete.vertices(g, Isolated)
LO2 = LO[-Isolated,]
name_rap<-name_rap[-Isolated]

plot(simplify(G2), layout=LO2, vertex.label=name_rap)
title("Network of word coefficients lambda_min rap")


gl<-glasso(var(as.matrix(sub_lyric_pop)), rho=0.0002)

g <- graph_from_adjacency_matrix(gl$w, mode = "undirected", weighted = TRUE)
LO = layout_with_fr(g)
Isolated = which(degree(g)==2)
G2 = delete.vertices(g, Isolated)
LO2 = LO[-Isolated,]
name_pop<-name_pop[-Isolated]
plot(simplify(G2), layout=LO2, vertex.label=name_pop)
title("Network of word coefficients lambda_min pop")

# ------------------------- XGBOOSTING ---------------------------


eta_p=seq(0.1,0.5,0.05)
max_depth_p=seq(8,15,1)
results_m=matrix(0, nrow = length(eta_p), ncol = length(max_depth_p))
results_std=matrix(0, nrow = length(eta_p), ncol = length(max_depth_p))
for(i in 1:length(eta_p)){
  for(j in 1:length(max_depth_p)){
    print(max_depth_p[i])
    folds <- cut(seq(1,nrow(train_sparse_matrix)),breaks=10,labels=FALSE)
    results<-rep(0, 10)
    for(k in 1:10){
      testIndexes <- which(folds==i,arr.ind=TRUE)
      testData <- train_sparse_matrix[testIndexes, ]
      trainData <- train_sparse_matrix[-testIndexes, ]
      
      numberOfClasses <- length(unique(y[random]))
      xgb_params <- list("objective" = "multi:softprob",
                         "eval_metric" = "mlogloss",
                         "num_class" = numberOfClasses,
                         eta=eta_p[i])
      nround    <- 20 
      
      a=as.integer(y[random])-1
      cv_model <- xgboost(params = xgb_params,
                          data = trainData, 
                          max_depth=max_depth_p[j],
                          label=a[-testIndexes],
                          nrounds = nround,
                          verbose = TRUE,
                          prediction = TRUE)
      
      test_pred <- predict(cv_model, newdata = testData)
      test_prediction <- matrix(test_pred, nrow = 3,
                                ncol=length(test_pred)/3) %>%
        t() %>%
        data.frame() %>%
        mutate(label = as.integer(y[random])[testIndexes] ,
               max_prob = max.col(., "last"))
      results[k]<-mean(test_prediction$label==test_prediction$max_prob)
    }
    results_m[i,j]=mean(results)
    results_std[i,j]<-var(results)
  }
}


image(1:ncol(results_m), 1:nrow(results_m), t(results_m), col = terrain.colors(60), axes = FALSE)
axis(1, 1:ncol(results_m), colnames(results_m))
axis(2, 1:nrow(results_m), rownames(results_m))

for (x in 1:ncol(results_m))
  for (z in 1:nrow(results_m))
    text(x, z, round(results_m[z,x], 3))


numberOfClasses <- length(unique(y[random]))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses, eta=0.2)
nround    <- 50 

a=as.integer(y[random])-1
cv_model <- xgboost(params = xgb_params,
                   data = train_sparse_matrix, 
                   max_depth=8,
                   label=a,
                   nrounds = nround,
                   verbose = TRUE,
                   prediction = TRUE)

test_pred <- predict(cv_model, newdata = test_sparse_matrix)
test_prediction <- matrix(test_pred, nrow = 3,
                          ncol=length(test_pred)/3) %>%
  t() %>%
  data.frame() %>%
  mutate(label = as.integer(y[-random]) ,
         max_prob = max.col(., "last"))

confusionMatrix(as.factor(test_prediction$max_prob),
                as.factor(test_prediction$label),
                mode = "everything")


confusionMatrix(as.factor(predict(cv1, newx = test_sparse_matrix, s='lambda.min', type='class')),
                as.factor(y[-random]),
                mode = "everything")

importance<-xgb.importance(feature_names = colnames(train_sparse_matrix), cv_model)

tibble(name=importance$Feature, gain=importance$Gain) %>% filter(gain>0.01) %>% 
  ggplot(aes(x=reorder(name, gain), y=gain))+ggtitle("Feature importance") +labs(y= "gain", x = "words")+geom_bar(stat = "identity", fill = "#1DB954")+coord_flip()

pop_importance=xgb.importance(model = cv_model, trees = seq(from=0, by=3, nround))
rap_imporatance=xgb.importance(model = cv_model, trees = seq(from=1, by=3, nround))
rock_importance=xgb.importance(model = cv_model, trees = seq(from=2, by=3, nround))

tibble(name=pop_importance$Feature, gain=pop_importance$Gain) %>% filter(gain>0.01) %>% 
  ggplot(aes(x=reorder(name, gain), y=gain))+ ggtitle("Feature importance for pop") +labs(y= "gain", x = "words")+
  geom_bar(stat = "identity", fill = "#82B91D")+coord_flip()

tibble(name=rap_imporatance$Feature, gain=rap_imporatance$Gain) %>% filter(gain>0.01) %>% 
  ggplot(aes(x=reorder(name, gain), y=gain))+ggtitle("Feature importance for rap")+labs(y= "gain", x = "words")+geom_bar(stat = "identity", fill = "#1D82B9")+coord_flip()

tibble(name=rock_importance$Feature, gain=rock_importance$Gain) %>% filter(gain>0.01) %>% 
  ggplot(aes(x=reorder(name, gain), y=gain))+ggtitle("Feature importance for rock")+labs(y= "gain", x = "words")+geom_bar(stat = "identity", fill = "lightblue")+coord_flip()

xgb.dump(cv_model, with_stats = TRUE)
xgb.plot.tree(model = cv_model)


# -------------------------------------- CONCLUSION ------------------------------------------

test_pred <- predict(cv_model, newdata = test_sparse_matrix)
test_prediction <- matrix(test_pred, nrow = 3,
                          ncol=length(test_pred)/3) %>%
  t() %>%
  data.frame() %>%
  mutate(label = as.integer(y[-random]) ,
         max_prob = max.col(., "last"))
# confusion matrix of test set
confusionMatrix(as.factor(test_prediction$max_prob),
                as.factor(test_prediction$label),
                mode = "everything")


confusionMatrix(as.factor(predict(cv1, newx = test_sparse_matrix, s='lambda.1se', type='class')),
                as.factor(y[-random]),
                mode = "everything")

confusionMatrix(as.factor(predict(cv.5, newx = test_sparse_matrix, s='lambda.min', type='class')),
                as.factor(y[-random]),
                mode = "everything")


glmnet(x=train_sparse_matrix, y=y[random], family = 'multinomial', relax = TRUE)
