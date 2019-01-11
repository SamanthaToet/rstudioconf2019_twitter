
# Load packages
library(rtweet) # interface with Twitter API
library(tidytext) # sentiment analysis
library(tidyverse) # ggplot
library(wordcloud)


# Connect to Twitter
# *~* API witchcraft *~*

# Get tweets 
conf_tweets <- search_tweets(
    "#rstudioconf", n = 10000, include_rts = FALSE) #rstudioconf

conf19_tweets <- search_tweets(
    "#rstudioconf2019", n = 10000, include_rts = FALSE) #rstudiconf2019

conf <- rbind(conf_tweets, conf19_tweets) %>%
    select(created_at, text) %>%
    mutate(id = c(1:73), text = tolower(text))

# Tidy
conf_tidy <- conf %>%
    unnest_tokens(word, text)

data(stop_words)
stop_words <- data.frame(word = c("https", "http", "amp", "t.co", "rstudioconf", 
                                  "rstudio", "rstudioconf2019", "conf", "2019"), 
                         lexicon = "custom") %>%
    bind_rows(stop_words)

conf_tidy <- conf_tidy %>%
    anti_join(stop_words) #small n

# Explore
conf_tidy %>%
    count(word, sort = TRUE)

conf_tidy %>%
    count(word, sort = TRUE) %>%
    filter(n > 2) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() #rstats, goals, and preparation 

# NRC sentiments
# joy
nrc_joy <- get_sentiments("nrc") %>%
    filter(sentiment == "joy")

conf_tidy %>%
    inner_join(nrc_joy) %>%
    count(word, sort = TRUE) # fun, improve, excited 

# anticipation
nrc_anticipation <- get_sentiments("nrc") %>%
    filter(sentiment == "anticipation")

conf_tidy %>%
    inner_join(nrc_anticipation) %>%
    count(word, sort = TRUE) # preparation, improve, wait

#all nrc
nrc <- get_sentiments("nrc")

conf_tidy %>%
    inner_join(nrc) %>%
    count(word, sentiment, sort = TRUE) %>%
    filter(n > 1) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col(aes(fill = sentiment)) +
    xlab(NULL) +
    coord_flip() +
    ggtitle("NRC sentiment counts")# anticipation, joy, and positivity

# Bing sentiments
bing_sentiment <- conf_tidy %>%
    inner_join(get_sentiments("bing")) %>%
    count(id, created_at, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)

ggplot(bing_sentiment, aes(created_at, sentiment)) +
    geom_line() +
    labs(title = "Bing sentiment scores over time", 
         x = "Date", y = "Sentiment") # more neg spikes as we get closer to the date  

bing_word_counts <- conf_tidy %>%
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
    labs(x = NULL,
         y = "Bing contribution to sentiment") +
    coord_flip()

# Wordcloud bc why not?
conf_tidy %>%
    anti_join(stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100, ordered.colors = TRUE))›

# next steps: n-grams, shiny app
