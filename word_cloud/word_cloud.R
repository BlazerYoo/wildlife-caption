# Libraries
library(tidyverse)
library(wordcloud)
library(stringr)
library(tm)

# Load data
data <- read.csv(file = 'https://raw.githubusercontent.com/BlazerYoo/wildlife-caption/main/data/data_finished_surveys.csv?token=GHSAT0AAAAAAB3WS2WT56JT5UION6BMNNAUY4EI53Q')

# Take the word column
column_name = "To.see.the.post.again..use.your.mouse.to.click.on.the..Back..button.....What.one.word.comes.to.mind.when.you.see.this.post."
words <- data[c(column_name)]

# Extract just the first word
first_word <- word(words$To.see.the.post.again..use.your.mouse.to.click.on.the..Back..button.....What.one.word.comes.to.mind.when.you.see.this.post., 1)

# Create corpus from the first words
docs <- Corpus(VectorSource(first_word))

# Clean
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

# Create document term matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Create word cloud
set.seed(1234) # for reproducibility
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

# Create word cloud w/ wordcloud2
wordcloud2(data=df, size=1.6, color='random-dark')
wordcloud2(data=df, size = 0.7, shape = 'pentagon')

# Case-sensitive (take first word)


# Case-insensitive (take first word)


# Case-sensitive (remove multi-words)


# Case-insensitive (remove multi-words)
