#https://wanjirumaggie45.medium.com/the-power-of-social-media-analytics-twitter-text-mining-using-r-1fceb26ac32b



library(rtweet)  # Twitter API
library(readr) 
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)

worldcup_tweets <-
  search_tweets(
    "#worldcup2022",
    n = 5000,
    include_rts = FALSE,
    lang = 'en',
    '-filter' = 'replies')


head(worldcup_tweets)
    

# remove http elements manually
worldcup_tweets$text <- gsub("http.*","",  worldcup_tweets$text)
worldcup_tweets$text <- gsub("https.*","", worldcup_tweets$text)




# remove text junk

library(tibble) # rownames_to_column
library(stringr) # str_remove_all
library(tokenizers)# count_words



worldcup_tweet_id <-
  worldcup_tweets %>%
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



# remove punctuation, convert to lowercase, add id for each tweet!
worldcup_tweet_clean <- worldcup_tweet_id %>%
  dplyr::select(id, text) %>%
  unnest_tokens(word, text)



# plot the top 20 words
worldcup_tweet_clean %>%
count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in #worldcup2022 tweets")




# load list of stop words - from the tidytext package
data("stop_words")


# view first 10 words
head(stop_words, 10)

# remove stop words from your list of words
worldcup_tweet_words <- worldcup_tweet_clean %>%
  anti_join(stop_words)

nrow(worldcup_tweet_words)



# plot the top 10 words -- notice any issues?
worldcup_tweet_words %>%
count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of top 10 unique words found in #worldcup 2022 tweets",
       subtitle = "Stop words removed from the list")

#create a customized list of stopwords different from the words contained in the standarized list
custom_stop_words <- 
  tibble(word = c('qatar2022', 'qatar', 'og', 'futbol', 'cup', 'worldcup', 'indvsaus', 'team', 'teams', 'fifa','world'))



worldcup_tweet_words2 <- worldcup_tweet_clean %>%
  anti_join(stop_words)%>%
  anti_join(custom_stop_words)



worldcup_tweet_words2 %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of top 10 unique words found in #worldcup 2022 tweets",
       subtitle = "Stop words removed from the list")


#add sentiment to each word with both qualitative and quantitative approach. 
worldcup_tweets_sent <- worldcup_tweet_words2%>%
  left_join(get_sentiments("afinn"))%>%
  left_join(get_sentiments("bing"))



library(wordcloud) 
library(reshape2)
worldcup_tweet_words2%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment,sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("blue","purple"),
                   max.words = 150)





get_sentiments("afinn") %>% filter(value > 4)

worldcup_tweet_words2_afinn <- 
  worldcup_tweet_words2 %>%
  left_join(get_sentiments("afinn"), by = "word")


#vader. it gives me the classification of each word by quantitative score and qualitative one as well. 

library(vader)

vader_sent <- vader_df(worldcup_tweet_words2$word)


vader_sent2 <- 
  vader_sent %>%
  rowid_to_column("id")%>%
  select(id, text, compound)%>%
  mutate(
    vader_sent = case_when(
      compound > 0.025 ~ "positive",
      compound < -0.025 ~ "negative",
      TRUE ~ "neutral"
    )
  )


vader_sent2 %>%
  group_by(vader_sent)%>%
  summarize(AMT = n(), sent = sum(compound))






