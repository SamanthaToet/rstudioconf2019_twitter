
# Load packages
library(rtweet) # interface with Twitter API
library(tidytext) # sentiment analysis
library(tidyverse) # ggplot


# Connect to Twitter
# ~*~ hidden twitter api magic ~*~


# Get tweets 
conf_tweets <- search_tweets(
    "#rstudioconf", n = 10000, include_rts = FALSE) #rstudioconf

conf19_tweets <- search_tweets(
    "#rstudioconf2019", n = 10000, include_rts = FALSE) #rstudiconf2019

conf <- rbind(conf_tweets, conf19_tweets) %>%
    select(created_at, text) %>%
    mutate(id = c(1:28), text = tolower(text))

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



