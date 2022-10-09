#https://wanjirumaggie45.medium.com/the-power-of-social-media-analytics-twitter-text-mining-using-r-1fceb26ac32b



library(rtweet)  # Twitter API
library(readr) 
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)
# remove text junk

library(tibble) # rownames_to_column
library(stringr) # str_remove_all
library(tokenizers)# count_words
library(dplyr) # separate columns 
#vader lexicon 
library(vader)
#wordcloud
library(wordcloud) 
library(reshape2)



ukrainewar_tweets <-
  search_tweets(
    "#ukrainewar",
    n = 5000,
    include_rts = FALSE,
    lang = 'en',
    '-filter' = 'replies')


head(ukrainewar_tweets)
    

# remove http elements manually
ukrainewar_tweets$text <- gsub("http.*","",  ukrainewar_tweets$text)
ukrainewar_tweets$text <- gsub("https.*","", ukrainewar_tweets$text)


ukrainewar_tweet_id <-
  ukrainewar_tweets %>%
  mutate(
    # remove links
    text = str_remove_all(text, "https\\S*"),
    text = str_remove_all(text, "http\\S*"),
    text = str_remove_all(text, "t.co*"),
    # remove mentions
    text = str_remove_all(text, "@\\S*"),
    # remove annoying html stuff
    text = str_remove_all(text, "amp"),
    text = str_remove_all(text, "&S*"),
    text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"),
    text = str_replace_all(text, "<a(.*?)>", " "),
    text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),
    text = str_replace_all(text, "&#[:digit:]+;", " "),
    text = str_remove_all(text, "<[^>]*>"),
    # remove numbers
    text = str_remove_all(text, "[:digit:]"),
    # remove excess whitespace
    text = str_squish(text),
    text = str_trim(text),
    # remove RT for retweets -- keeping retweets in the data
    text = str_remove_all(text, "RT")
  ) %>%
  filter(count_words(text) > 1) %>%
  rownames_to_column("id")


#make sure to keep the date format without the time, just y/m/d
ukrainewar_tweet_id$created_at <- 
 stringr::str_split_fixed(ukrainewar_tweet_id$created_at, " ", 2)



#we can create a histogram based on the number of word on the tweets in order to see its distribution. 
ukrainewar_tweet_id %>%
  ggplot()+
  geom_histogram(color = "black", fill = "red",
    aes(x = display_text_width)
  ) + scale_x_continuous(trans = 'log1p')+
  labs(x = "Number of words in tweets",
       y = "Number of tweets",
       title = "Distribution #ukrainewar Tweets Based on its Number of Words")+
  theme(panel.background = element_blank())



# remove punctuation, convert to lowercase, add id for each tweet!
ukrainewar_tweet_clean <- ukrainewar_tweet_id %>%
  dplyr::select(id, text) %>%
  unnest_tokens(word, text)



# plot the top 20 words
ukrainewar_tweet_clean %>%
count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in #ukraine war tweets")




# load list of stop words - from the tidytext package
data("stop_words")


# remove stop words from your list of words
ukrainewar_tweet_words <- ukrainewar_tweet_clean %>%
  anti_join(stop_words)



nrow(ukrainewar_tweet_words)



# plot the top 10 words -- notice any issues?
ukrainewar_tweet_words %>%
count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of top 10 unique words found in #ukraine tweets",
       subtitle = "Stop words removed from the list")

#create a customized list of stopwords different from the words contained in the standarized list
custom_stop_words <- 
  tibble(word = c('russia', 'ukraine', 'war', 'ukrainewar', 'ukrainerussiawar', 'russian', 'ukrainian', 'ukrainerussianwar', 'russiaukrainewar',
                  'russians', 'ukrainians'))



ukrainewar_tweet_words2 <- ukrainewar_tweet_clean %>%
  anti_join(stop_words)%>%
  anti_join(custom_stop_words)


#let's plot the top 10 words again but without the custom stopwords

ukrainewar_tweet_words2 %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "blue") +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of top 10 unique words found in #ukraine tweets",
       subtitle = "Stop words removed from the list")+
  theme(panel.background = element_blank())
  





#add sentiment to each word with both qualitative and quantitative approach. 
ukrainewar_tweets_sent <- ukrainewar_tweet_words2%>%
  left_join(get_sentiments("afinn"))%>%
  left_join(get_sentiments("bing"))




ukrainewar_tweet_words2%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment,sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red","green"),
                   max.words = 150)





get_sentiments("afinn") %>% filter(value > 4)

ukrainewar_tweet_words2_afinn <- 
  ukrainewar_tweet_words2 %>%
  left_join(get_sentiments("afinn"), by = "word")


#vader. it gives me the classification of each word by quantitative score and qualitative one as well. 



ukrainewar_vader <- vader_df(ukrainewar_tweet_words2$word)


ukrainewar_vader_2 <- 
  ukrainewar_vader %>%
  rowid_to_column("id")%>%
  select(id, text, compound)%>%
  mutate(
    vader_sent = case_when(
      compound > 0.025 ~ "positive",
      compound < -0.025 ~ "negative",
      TRUE ~ "neutral"
    )
  )



ukrainewar_vader_2 %>%
  group_by(vader_sent)%>%
  summarize(AMT = n(), sent = sum(compound), AVG = mean(compound))



#lets plot a boxplot with the sentiment score on each of the three categories created

boxplot(compound ~ vader_sent,
        data = ukrainewar_vader_2,
        main = "Vader Sentiment Analysis #ukrainewar",
        xlab = "Sentiment",
        ylab = "Sentiment Score",
        col = "steelblue",
        border = "black")




#Create a new data set just with words with positive or negative sentiment 

ukrainewar_vader_3 <- ukrainewar_vader_2 %>%
  filter(vader_sent != "neutral")

ukrainewar_vader_4 <- ukrainewar_vader_3 %>%
  count(text, sort = TRUE) %>%
  top_n(10)%>%
  inner_join(ukrainewar_vader_3, by = "text")%>%
  select(-id)
  
 
ukrainewar_vader_6 <- ukrainewar_vader_4[!duplicated(ukrainewar_vader_4$text), ]



ggplot() +
  geom_col(data = ukrainewar_vader_6, aes(x = text, y = n, fill = vader_sent)) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words with +/- sentiment",
       title = "Count of Top 10 unique +/- Words Found in #ukraine Tweets",
       subtitle = "Stop words removed from the list") +
  theme(panel.background = element_blank())
  




#compare distributions according with the sentiment of the words - combined histogram


ggplot(ukrainewar_vader_3, aes(x = compound,  fill = vader_sent))+
  geom_histogram(aes(y = ..density..),color = "black", alpha=0.5, position = "identity",
  ) + geom_density(alpha = 0.5)+
  labs(x = "Sentiment Score of Words",
       y = "Density",
       title = "Distribution of Sentiment for Positive and Negative Words")+
  theme(panel.background = element_blank())



#we can create a worcloud just with VADER

ukrainewar_vader_3%>%
  count(text, vader_sent, sort = TRUE) %>%
  acast(text ~ vader_sent, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red","green"),
                   max.words = 150)









