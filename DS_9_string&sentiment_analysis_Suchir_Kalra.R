#Author: Suchir Kalra

##D3S DS-9 (Week 12)

#Sentiment Analysis on Tagore and Kipling

install.packages(c("tidytext", "imager", "tidyr", "dplyr", "gutenbergr", "magrittr"))
install.packages("textdata")

library(tidytext)
library(imager)
library(tidyr)
library(dplyr)
library(stringr)
library(gutenbergr)
library(ggplot2)
library(magrittr)
library(stringr)
library (textdata)

source("mirror_find.R")

good_mirror <- mirror_find()  #Finding a good mirror


#====================SETTING UP THE CORPUS======================#

#Adding the Gutenberg IDs to create a corpus 

kipling_rudyard <- gutenberg_download(c(236, 1858, 32977, 8147, 2334, 	2828, 2226, 8649, 30568), mirror = good_mirror, meta_fields = c("title", "author"))
  
tagore_rabindranath <- gutenberg_download(c(7971, 7951, 33525, 2518, 7164,7166, 34757,6522,40766, 6686), mirror = good_mirror, meta_fields = c("title", "author"))

#A word of caution!! --> Adding this --> gutenberg_works(author =="Tagore, Rabindranath") %>% ##ALERT: IT WILL PICK UP EVERYTHING

#Making a compact corpus from Kipling and Tagore
corpus <- kipling_rudyard %>%
  bind_rows(tagore_rabindranath) %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) #Tokenizes the text column into individual words.Each row now represents one word.
  
write.csv(corpus, "corpus.csv")
corpus <- read.csv("corpus.csv",stringsAsFactors = FALSE, header = TRUE)


#====================CORPUS HAS BEEN SET SUCCESSFULLY. NOTE: NOW, WE CAN STRAIGHT REFER TO CSV======================#

###Task 1: Do a word length analysis between Kipling & Tagore using String Manipulation

corpus <- corpus %>%
  mutate(word_length = str_length(word))

# Compare average word lengths by author
word_length_summary <- corpus %>%
  group_by(author) %>%
  summarise(avg_word_length = mean(word_length, na.rm = TRUE))

print(word_length_summary)

###Task 2: Find the longest words per Author

longest_words <- corpus %>%
  mutate(length = str_length(word)) %>%
  group_by(author) %>%
  top_n(10, length) %>%
  arrange(author, desc(length)) %>%
  distinct(word, .keep_all = TRUE)

ggplot(longest_words, aes(x = reorder(word, length), y = length, fill = author)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~author) +
  labs(title = "Longest Words Used by Each Author", x = "Word", y = "Length")

### Filter for the word "love" (case-insensitive match if needed)

love_by_author <- corpus %>%
  mutate(word = str_to_lower(word)) %>%  # ensure all lowercase for consistency
  filter(word == "love") %>%
  count(author, word)  # count how many times each author uses "love"

### Plot it
ggplot(love_by_author, aes(x = author, y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Frequency of the Word 'Love' by Author",
       x = "Author",
       y = "Count of 'love'") +
  theme_minimal()

###Intro to Sentiment Analysis 

#====================BELOW WE ARE GOING TO SETUP THE NRC SENTIMENT DIRECTORY=========================#

#Fetch a sentiment dictionary. Here we are using the NRC.

nrcjoy <- get_sentiments("nrc") %>%  ##NRC Word-Emotion Association Lexicon 
  filter(sentiment == "joy")

corpus %>%   #pipe - "and then" 
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

#Load the image below to refresh your understanding of the different kind of joins :

join_diagram <- load.image("https://www.powerbi-pro.com/wp-content/uploads/2018/05/051118_1848_PowerBIsech1.png")
plot(join_diagram, axes = FALSE)

joyless_or_neutral <- corpus %>%
   anti_join(nrcjoy) %>% 
  count(word, sort = TRUE) #You're left with all the words not associated with joy

#Sentiments in Corpus

corpussentiment <- corpus %>%
  inner_join(get_sentiments("bing")) %>% #bing is a scale of sentiments. Flags it according to positive and negative
  count(title, index = linenumber %/% 80, sentiment) %>% #Check sentiments every 80 lines
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)


ggplot(corpussentiment, aes(index, sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x")

corpussentiment_author <- corpus %>%
  inner_join(get_sentiments("bing")) %>% #bing is a scale of sentiments. Flags it according to positive and negative
  count(author, index = linenumber %/% 80, sentiment) %>% #Check sentiments every 80 lines
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(corpussentiment_author, aes(index, sentiment, fill = author)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~author, ncol = 2, scales = "free_x")


#Activity re-scaling the sentiment

#The above piece of code calculates sentiment by every 80 words. How can we
#rescale this into different units? What would be the code for every 40 words?
#What about every chapter? How do the graphs differ? What might be another
#useful scale to measure sentiment? How would we get there?

bing_word_counts <- corpus %>%
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#In theory, the function below should let me keep all of my columns,
#but when I run it the count seems irrelevant to the book. Why?

bing_word_counts_full <- corpus %>%
  inner_join(get_sentiments("bing")) %>% 
  group_by(title) %>% 
  add_count(word, sentiment, sort = TRUE) %>% 
  distinct_at(vars(word), .keep_all = TRUE) %>% 
  ungroup()


#We can now see the top ten words in each book across the corpus, and the
#relative distribution of positive and negative words in each text.

bing_word_counts_full %>%
  group_by(title) %>%
  top_n(10) %>%
   mutate(word = reorder(word, n)) %>% 
  ungroup() %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  facet_wrap(~title)+
  coord_flip()
  

