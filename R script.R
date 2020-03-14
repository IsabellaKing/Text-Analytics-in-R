# importing libraries
library(textreadr) # load documents
library(textdata)  # provide access to text-related data sets
library(tidyr)     # tidy the data
library(dplyr)     # data manipulation
library(stringr)   # extract words from a sentence
library(tidytext)  # text mining
library(tm)        # text mining
library(scales)    # scaling
library(reshape2)  # transform data between wide and long formats
library(wordcloud) # visualize the keyword as a word cloud
library(ggplot2)   # visualization tool
library(igraph)    # for bigram network
library(ggraph)    # for bigram network

# loading the .txt files
setwd('C:/Users/isabe/OneDrive/Desktop/MSBA-DD/Module B/Text Analytics/Individual Assignment')
CNN <- read_document('CNN.txt')
TIME <- read_document('TIME.txt')
BLB <- read_document('Bloomberg.txt')

# converting .txt files to dataframes
CNN_df <- tibble(line=1:95, text = CNN, stringsAsFactors=FALSE)
TIME_df <- tibble(line=1:35, text = TIME, stringsAsFactors=FALSE)
BLB_df <- tibble(line=1:104, text = BLB, stringsAsFactors=FALSE)

# remove nunerics in the text
CNN_df$text <- gsub("[0-9]+", "", CNN_df$text)
TIME_df$text <- gsub("[0-9]+", "", TIME_df$text)
BLB_df$text <- gsub("[0-9]+", "", BLB_df$text)

# toeknization
CNN_tokens <- CNN_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word") %>%
  count(line, word, sort=TRUE) %>%
  ungroup()
TIME_tokens <- TIME_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word") %>%
  count(line, word, sort=TRUE) %>%
  ungroup()
BLB_tokens <- BLB_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word") %>%
  count(line, word, sort=TRUE) %>%
  ungroup()

#############################################
############### Word Frequency ##############
#############################################
# combine three dataframes together
CNN_df$news <- 'CNN'
TIME_df$news <- 'TIME'
BLB_df$news <- 'BLB'
all_df <- rbind(CNN_df, TIME_df, BLB_df)

tidy_news <- all_df %>%
  group_by(news) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word") %>%
  count(word, sort = TRUE) 

tidy_news %>%
  filter(n > 6) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# count the word frequency
frequency <- bind_rows(mutate(CNN_tokens, news="CNN"),
                       mutate(TIME_tokens, news="TIME"),
                       mutate(BLB_tokens, news="Bloomberg")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(news, word) %>%
  group_by(news) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(news, proportion) %>%
  gather(news, proportion, TIME, Bloomberg)

#let's plot the correlograms:
ggplot(frequency, aes(x=proportion, y=CNN, 
                      color = abs(CNN- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~news, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "CNN", x=NULL)

# correlation test
cor.test(data=frequency[frequency$news=="Bloomberg",],
         ~proportion + CNN)

cor.test(data=frequency[frequency$news=="TIME",],
         ~proportion + CNN)

##################################################
############### Sentiment Analysis ###############
##################################################

# word cloud
CNN_tokens %>%
  anti_join(stop_words, by="word") %>%
  inner_join(get_sentiments("bing"), by="word") %>% # binary
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)

CNN_tokens %>%
  anti_join(stop_words, by="word") %>%
  inner_join(get_sentiments("nrc"), by="word") %>% # flavor
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale=c(0.9, 0,9),
                   fixed.asp=TRUE, title.size=0.9)

TIME_tokens %>%
  anti_join(stop_words, by="word") %>%
  inner_join(get_sentiments("bing"), by="word") %>% # binary
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale=c(0.9, 0.9),
                   fixed.asp=TRUE, title.size=0.9)

TIME_tokens %>%
  anti_join(stop_words, by="word") %>%
  inner_join(get_sentiments("nrc"), by="word") %>% # binary
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale=c(0.9, 0.9),
                   fixed.asp=TRUE, title.size=0.9)

BLB_tokens %>%
  anti_join(stop_words, by="word") %>%
  inner_join(get_sentiments("bing"), by="word") %>% # binary
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale=c(0.9, 0.9),
                   fixed.asp=TRUE, title.size=0.9)

BLB_tokens %>%
  anti_join(stop_words, by="word") %>%
  inner_join(get_sentiments("nrc"), by="word") %>% # binary
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale=c(0.9, 0,9),
                   fixed.asp=TRUE, title.size=0.9)

# positive and negative sentiments among 3 news reports
tidy_news <- all_df %>%
  group_by(news) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word")
  
# using bing
news_sentiment <- tidy_news %>%
  inner_join(get_sentiments("bing"), by="word") %>%  # no positive sentiment
  count(news, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(news_sentiment, aes(index, sentiment, fill = news)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~news, ncol = 2, scales = "free_y")

# using nrc
news_sentiment <- tidy_news %>%
  inner_join(get_sentiments("nrc"), by="word") %>%  #CNN and BLB still have some postive sentiments
  count(news, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(news_sentiment, aes(index, sentiment, fill = news)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~news, ncol = 2, scales = "free_y")

# comparing three sentiment lexicons in each news report seperately
CNN <- tidy_news %>% 
  filter(news == "CNN")

afinn <- CNN %>% 
  inner_join(get_sentiments("afinn"), by="word") %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(CNN %>% 
                            inner_join(get_sentiments("bing"), by="word") %>%
                            mutate(method = "Bing et al."),
                          CNN %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative")), by="word") %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

TIME <- tidy_news %>% 
  filter(news == "TIME")

afinn <- TIME %>% 
  inner_join(get_sentiments("afinn"), by="word") %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(TIME %>% 
                            inner_join(get_sentiments("bing"), by="word") %>%
                            mutate(method = "Bing et al."),
                          TIME %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative")), by="word") %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

BLB <- tidy_news %>% 
  filter(news == "BLB")

afinn <- BLB %>% 
  inner_join(get_sentiments("afinn"), by="word") %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(BLB %>% 
                            inner_join(get_sentiments("bing"), by="word") %>%
                            mutate(method = "Bing et al."),
                          BLB %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative")), by="word") %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


# Most common positive and negative words in each news report
CNN_bing_counts <- CNN_tokens %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

CNN_bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

CNN_nrc_counts <- CNN_tokens %>%
  inner_join(get_sentiments("nrc"), by="word") %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

CNN_nrc_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

TIME_bing_counts <- TIME_tokens %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

TIME_bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

TIME_nrc_counts <- TIME_tokens %>%
  inner_join(get_sentiments("nrc"), by="word") %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

TIME_nrc_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

BLB_bing_counts <- BLB_tokens %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

BLB_bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

BLB_nrc_counts <- BLB_tokens %>%
  inner_join(get_sentiments("nrc"), by="word") %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

BLB_nrc_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

###############################
####### TF-IDF framework#######
###############################

# ZIPF's law
all_tokens <- bind_rows(mutate(CNN_tokens, news="CNN"),
                        mutate(TIME_tokens, news="TIME"),
                        mutate(BLB_tokens, news="Bloomberg"))
                       
freq_by_rank <- all_tokens %>%
  group_by(news) %>%
  mutate(rank = row_number(),
         `term frequency` = n/sum(n))

#plotting ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=news))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

# TF_IDF
all_words <- all_tokens %>%
  bind_tf_idf(word, line, n)

all_words %>%
  arrange(desc(tf_idf))

# looking at the graphical apprach:
all_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(news) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=news))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~line, ncol=2, scales="free")+
  coord_flip()

#############################################
###### N-grams and tokenizing ###############
#############################################

CNN_bigrams <- CNN_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
TIME_bigrams <- TIME_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
BLB_bigrams <- BLB_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
all_bigrams <- all_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

CNN_bigrams%>%
  count(bigram, sort = TRUE) 
TIME_bigrams%>%
  count(bigram, sort = TRUE) 
BLB_bigrams%>%
  count(bigram, sort = TRUE) 
all_bigrams%>%
  count(bigram, sort = TRUE) 

CNN_bigrams_separated <- CNN_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
TIME_bigrams_separated <- TIME_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
BLB_bigrams_separated <- BLB_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
all_bigrams_separated <- all_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

CNN_bigrams_filtered <- CNN_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
TIME_bigrams_filtered <- TIME_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
BLB_bigrams_filtered <- BLB_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
all_bigrams_filtered <- all_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
CNN_bigram_counts <- CNN_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
TIME_bigram_counts <- TIME_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
BLB_bigram_counts <- BLB_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
all_bigram_counts <- all_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# Visualizing a bigram network
CNN_bigram_graph <- CNN_bigram_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

ggraph(CNN_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

TIME_bigram_graph <- TIME_bigram_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

ggraph(TIME_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

BLB_bigram_graph <- BLB_bigram_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

ggraph(BLB_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

all_bigram_graph <- all_bigram_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

ggraph(all_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


