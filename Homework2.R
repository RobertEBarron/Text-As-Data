

#Michael Schmitz

#Sentiment analysis using raw words


library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)


circle_raw <- read_file("Week_2/texts/A07594__Circle_of_Commerce.txt")
free_raw   <- read_file("Week_2/texts/B14801__Free_Trade.txt")




texts <- c(
  "Circle_of_Commerce" = circle_raw,
  "Free_Trade"         = free_raw)


corp <- corpus(texts)



toks <- tokens(
  corp,
  remove_punct   = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
)


toks <- tokens_tolower(toks)


custom_stop <- c(
  "vnto","haue","doo","hath","bee","ye","thee","hee","shall","hast","doe",
  "beene","thereof","thus" 
)

toks <- tokens_remove(toks, pattern = c(stopwords("en"), custom_stop))




bing <- get_sentiments("bing")


raw_count_summary <- toks %>%
  dfm() %>%
  tidytext::tidy() %>%
  rename(word = term) %>%
  inner_join(bing, by = "word") %>% 
  group_by(document, sentiment) %>% 
  summarise(n = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative) %>% 
  rename(raw_negative = negative, raw_positive = positive, raw_net_sent = net_sentiment)



#Step 2!

dfm_mat <- dfm(toks)


dfm_tfidf <- dfm_tfidf(dfm_mat)


dfm_tfidf_sentiment <- dfm_tfidf %>%
  tidytext::tidy() %>%        # document | term | tf_idf
  rename(word = term) %>%
  rename(tf_idf = count) %>% 
  inner_join(bing, by = "word") %>% 
  group_by(document, sentiment) %>% 
  summarise(value = sum(tf_idf)) %>% 
  ungroup() %>%
  pivot_wider(
    names_from = sentiment,
    values_from = value, values_fill = 0
  ) %>% 
  mutate(net_sentiment = positive - negative) %>% 
  rename(tfidf_negative = negative, tfidf_positive = positive, tfidf_net_sent = net_sentiment)

#merging raw count with tfidf

compiled_summary <- raw_count_summary %>% 
  left_join(dfm_tfidf_sentiment, by = "document")

#download csv file

readr::write_csv(
  compiled_summary,
  "compiled_summary.csv"
)


#analyzing the main drivers of negative sentiment

#Free Trade

dfm_tfidf %>%
  tidytext::tidy() %>%        
  rename(word = term) %>%
  rename(tf_idf = count) %>% 
  inner_join(bing, by = "word") %>% 
  filter(sentiment == "negative", document == "Free_Trade") %>% 
  arrange(desc(tf_idf))


#The main culprits for Free Trade are "false", "abuse", 'loose', "imposition", "strict", "lost"


#Circle of Commerce

dfm_tfidf %>%
  tidytext::tidy() %>%        
  rename(word = term) %>%
  rename(tf_idf = count) %>% 
  inner_join(bing, by = "word") %>% 
  filter(sentiment == "negative", document == "Circle_of_Commerce") %>% 
  arrange(desc(tf_idf))


#The main culprits for Free Trade are "ignorance", "impertinent", 'doubt', "imperfect", "danger", "complaint"




