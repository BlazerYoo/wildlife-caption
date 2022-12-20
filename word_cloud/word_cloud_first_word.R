# Create word clouds w/ taking the first word of all responses

# Libraries
library(tidyverse)
library(wordcloud)
library(stringr)
library(tm)

# Load data
data <- read.csv(file = 'https://raw.githubusercontent.com/BlazerYoo/wildlife-caption/main/data/data_finished_surveys.csv?token=GHSAT0AAAAAAB3WS2WS7LYMQXB6G3HN3WYMY4FCL7A')

# Take the word + image column
words_col_name = "To.see.the.post.again..use.your.mouse.to.click.on.the..Back..button.....What.one.word.comes.to.mind.when.you.see.this.post."
img_col_name = "image"
words_img <- data[c(words_col_name, img_col_name)]

# Create word dataframe for each image
img_col <- words_img$image
words_img1 <- words_img[img_col == "Image1",]
words_img2 <- words_img[img_col == "Image2",]
words_img3 <- words_img[img_col == "Image3",]
words_img4 <- words_img[img_col == "Image4",]

# Extract just the first word
first_word_img1 <- word(words_img1$To.see.the.post.again..use.your.mouse.to.click.on.the..Back..button.....What.one.word.comes.to.mind.when.you.see.this.post., 1)
first_word_img2 <- word(words_img2$To.see.the.post.again..use.your.mouse.to.click.on.the..Back..button.....What.one.word.comes.to.mind.when.you.see.this.post., 1)
first_word_img3 <- word(words_img3$To.see.the.post.again..use.your.mouse.to.click.on.the..Back..button.....What.one.word.comes.to.mind.when.you.see.this.post., 1)
first_word_img4 <- word(words_img4$To.see.the.post.again..use.your.mouse.to.click.on.the..Back..button.....What.one.word.comes.to.mind.when.you.see.this.post., 1)

# Create corpus from the first words
docs_img1 <- Corpus(VectorSource(first_word_img1))
docs_img2 <- Corpus(VectorSource(first_word_img2))
docs_img3 <- Corpus(VectorSource(first_word_img3))
docs_img4 <- Corpus(VectorSource(first_word_img4))

# Clean
docs_img1 <- docs_img1 %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs_img1 <- tm_map(docs_img1, content_transformer(tolower))
docs_img1 <- tm_map(docs_img1, removeWords, stopwords("english"))

docs_img2 <- docs_img2 %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs_img2 <- tm_map(docs_img2, content_transformer(tolower))
docs_img2 <- tm_map(docs_img2, removeWords, stopwords("english"))

docs_img3 <- docs_img3 %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs_img3 <- tm_map(docs_img3, content_transformer(tolower))
docs_img3 <- tm_map(docs_img3, removeWords, stopwords("english"))

docs_img4 <- docs_img4 %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs_img4 <- tm_map(docs_img4, content_transformer(tolower))
docs_img4 <- tm_map(docs_img4, removeWords, stopwords("english"))

# Create document term matrix
dtm_img1 <- TermDocumentMatrix(docs_img1)
matrix_img1 <- as.matrix(dtm_img1)
words_img1 <- sort(rowSums(matrix_img1),decreasing=TRUE)
df1 <- data.frame(word = names(words_img1),freq=words_img1)

dtm_img2 <- TermDocumentMatrix(docs_img2)
matrix_img2 <- as.matrix(dtm_img2)
words_img2 <- sort(rowSums(matrix_img2),decreasing=TRUE)
df2 <- data.frame(word = names(words_img2),freq=words_img2)

dtm_img3 <- TermDocumentMatrix(docs_img3)
matrix_img3 <- as.matrix(dtm_img3)
words_img3 <- sort(rowSums(matrix_img3),decreasing=TRUE)
df3 <- data.frame(word = names(words_img3),freq=words_img3)

dtm_img4 <- TermDocumentMatrix(docs_img4)
matrix_img4 <- as.matrix(dtm_img4)
words_img4 <- sort(rowSums(matrix_img4),decreasing=TRUE)
df4 <- data.frame(word = names(words_img4),freq=words_img4)

# Remove words that only appear once
df1 <- df1[df1$freq > 1,]
df2 <- df2[df2$freq > 1,]
df3 <- df3[df3$freq > 1,]
df4 <- df4[df4$freq > 1,]

# Create word cloud
set.seed(1234) # for reproducibility
wordcloud(words = df1$word, freq = df1$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = df2$word, freq = df2$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = df3$word, freq = df3$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = df4$word, freq = df4$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

sum(df1$freq)
sum(df2$freq)
sum(df3$freq)
sum(df4$freq)