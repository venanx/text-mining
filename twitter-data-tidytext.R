# See Article : Earth Lab - Text Mining Twitter Data With TidyText in R
# Original Source http://bit.ly/txt-mining

# This Version : Andre Venancio - andre@venanc.me - PUC

#### Load Libraries ####

# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)

# ggplot2 is a system for declaratively creating graphics - tidyverse!
library(ggplot2)
# dplyr is a grammar of data manipulation - tidyverse!
library(dplyr)
# The goal of readr is to provide a way to read data (like csv, tsv, and fwf) - tidyverse!
library(readr)
# The goal of tidyr is to help you create tidy data - tidyverse!
library(tidyr)

# text mining library
library(tidytext)

# igraph is a library collection for graphs and analyzing networks - plot!
library(igraph)
# ggraph is an extension of ggplot2 aimed at supporting relational data structures
library(ggraph)

# a simple class for storing time-of-day values
library(hms)

# this package wraps the pattern of un-tidying data into a wide matrix
library(widyr)

# Package Text Mining
library(tm)

# library for wordcloud
library(wordcloud2)

# library for colorfull graphs
library("viridis")       

#### Searching for Tweets ####

# capture information on twitter about the search tag
info_tweets <- search_tweets(q = "#edgecomputing", n = 5000,
                                lang = "en",
                                include_rts = FALSE)

#### Clean the Data for Tweets ####

# check data to see if there are emojis
head(info_tweets$text)

# remove http elements manually
info_tweets$stripped_text <- gsub("http.*","",  info_tweets$text)
info_tweets$stripped_text <- gsub("https.*","", info_tweets$stripped_text)

# check data to see if the url have been removed
head(info_tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
info_tweets_clean <- info_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# check data to see if the punctuation have been removed
head(info_tweets_clean)

# plot the top 15 words -- notice any issues?
info_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")

# load list of stop words - from the tidytext package
data("stop_words") 
# view first 6 words
head(stop_words)

# number of rows before clean
nrow(info_tweets_clean)

# remove stop words from your list of words
cleaned_tweet_words <- info_tweets_clean %>%
  anti_join(stop_words)

# there should be fewer words now
nrow(cleaned_tweet_words)

# plot the top 15 words -- notice any issues?
cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = word)) +
  scale_color_viridis(option = "B") +
  #scale_fill_viridis(option = "D") +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

#### Explore Networks of Words ####

# Create paired words - remove punctuation, convert to lowercase, add id for each tweet!
info_tweets_paired_words <- info_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

# Count paired words 
info_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

# Separate paired words 
info_tweets_separated_words <- info_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

# and filter stop words
info_tweets_filtered <- info_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
tweets_words_counts <- info_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

# view first 6 words
head(tweets_words_counts)

# plot tweets change word network
# (plotting graph edges is currently broken)
tweets_words_counts %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - Tweet Data",
       subtitle = "Text mining twitter data ",
       x = "", y = "")

#### Word Cloud 2 - Single Words ####

# Count words and create de dataframe
count_dataframe_words = cleaned_tweet_words %>% count(word)
# Create word cloud
wordcloud2(count_dataframe_words, color = "random-dark", backgroundColor = "white")

#### Word Cloud 2 - Pair of words ####

# Count words and create de dataframe
count_dataframe_words_paired = info_tweets_paired_words %>% count(paired_words)
# Create word cloud
wordcloud2(count_dataframe_words_paired, color = "random-dark", backgroundColor = "white")

#### TODO - Create a graph of words ####
