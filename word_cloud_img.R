# Libraries
library(tidyverse)
library(wordcloud)
library(stringr)
library(tm)

# Load data
data <- read.csv(file = 'https://raw.githubusercontent.com/BlazerYoo/wildlife-caption/main/data/data_finished_surveys.csv?token=GHSAT0AAAAAAB3WS2WTYBFO336VGWJEAWO4Y4E5H6A')

# Take the word + image column
word_col = "To.see.the.post.again..use.your.mouse.to.click.on.the..Back..button.....What.one.word.comes.to.mind.when.you.see.this.post."
img_col = "image"
words_img <- data[c(word_col, img_col)]

# Create word dataframe for each image
word_img1 <- words_img[words_img$image == "Image1",]
word_img2 <- words_img[words_img$image == "Image2",]
word_img3 <- words_img[words_img$image == "Image3",]
word_img4 <- words_img[words_img$image == "Image4",]

# Extract just the first word
first_word_img1 <- word(word_img1$To.see.the.post.again..use.your.mouse.to.click.on.the..Back..button.....What.one.word.comes.to.mind.when.you.see.this.post., 1)
first_word_img2 <- word(word_img2$To.see.the.post.again..use.your.mouse.to.click.on.the..Back..button.....What.one.word.comes.to.mind.when.you.see.this.post., 1)
first_word_img3 <- word(word_img3$To.see.the.post.again..use.your.mouse.to.click.on.the..Back..button.....What.one.word.comes.to.mind.when.you.see.this.post., 1)
first_word_img4 <- word(word_img4$To.see.the.post.again..use.your.mouse.to.click.on.the..Back..button.....What.one.word.comes.to.mind.when.you.see.this.post., 1)

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

# Create word cloud w/ wordcloud2
wordcloud2(data=df, size=1.6, color='random-dark')
wordcloud2(data=df, size = 0.7, shape = 'pentagon')

# Case-sensitive (take first word)


# Case-insensitive (take first word)


# Case-sensitive (remove multi-words)


# Case-insensitive (remove multi-words)
