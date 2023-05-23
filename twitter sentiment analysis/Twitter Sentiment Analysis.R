# In this article we'll first extract tweets using Twitter API,
# store and clean the dataset
#classify texts into categories of positive/negative/neutral
# calculate a sentiment score for each category, explore different types of visualizations
# and gain insights from them




# twitteR and rtweet are R packages that provides access to the Twitter API
# we're using rtweet because twitteR is deprecating
library(rtweet)
library(dplyr)
library(tidyr)
# for text mining
library(tidytext)
# for sentiment library
library(textdata)
# string manipulation
library(stringr)
library(tm)
library(ggplot2)
library(lubridate)
library(xlsx)
library(wordcloud2)
library(wordcloud)



getwd()
setwd('/Users/Jessicaboomboom/Library/Preferences/org.R-project.R/R/rtweet/')

# call this function for instructions on obtaining and storing user access tokens
vignette("auth", package = "rtweet")

# declare variables and store API keys 
consumer_key <- 'LEd44b09dqKkgC4jU1dG1rAPR'
consumer_secret <- '1cugDVBvULGgUIYeF2VDX402o7urhMIKTyqiuJQ81kT4KrxfzG'
access_token <- '1648682292284637184-hRytlEFa13d519YMAWijJRGlT9RN85'
access_secret <- 'Ha6cg0bL80aM197pxEB9AGmrMPdxbyel5ogrDdy3knkbL'

#authentication process
consumer_key <- "tksU9B3NyxoHncLabxFIUPFOA"
consumer_secret <- "93n5kXzGggu17jM4BF3CUjFmpfHoepGJXLUAaJbSNjLKE6WSk4"

auth <- rtweet_app()
auth_as(auth)

# pull tweets with hashtags
# Repro_df <- search_tweets(q = "#ReproRights", lang = "en", include_rts = FALSE, n = 100)
# get a specific user's tweets
raw_df <- 
  get_timeline(user = "@MsMagazine", n = 1000, lang = "en", include_rts = T) 

# show proportion of replies/retweets/organic tweets
# organic tweets - 850
df_organic <- 
  raw_df %>% 
  filter(!str_detect(full_text, '^RT'))
# retweets - 149
df_retweets <- 
  raw_df %>% 
  filter(str_detect(full_text, '^RT'))
# replies - 34
df_replies <- 
  subset(raw_df, !is.na(raw_df$in_reply_to_status_id))
# create a df with the # of each type of tweets
tweet_types <- 
  data.frame(
    category = c("organic", "retweets", "replies"),
    count = c(850, 149, 34)
  )
# display as donut chart
tweet_types$fraction = round(tweet_types$count/sum(tweet_types$count),2)
tweet_types$percent = round(tweet_types$count/sum(tweet_types$count) *100,2)
tweet_types$ymax = cumsum(tweet_types$fraction)
tweet_types$ymin = c(0, head(tweet_types$ymax, n = -1))

legend_type <- paste(tweet_types$category, tweet_types$percent, "%")

ggplot(tweet_types, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=legend_type)) +
  geom_rect() +
  scale_fill_brewer(palette = 4) +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

# plot number of tweets by month & day
#line plot
ts_plot(df, "days")
# bar plot
ggplot(df, aes(x = created_at)) +
  geom_histogram(position = 'identity', bins = 150, 
                 color = 'black', fill = 'lightblue',
                  show.legend = F)
# from the plot we can see that between 2023 Feb and 2023 May, 
# @MsMagazine posted most often in March and May when comparing to other months. 

# tweets by day
ggplot(df, aes(x = wday(created_at, label = T))) +
  labs(y = "Count", x = "Day",
       title = "frequency of tweets over time") +
  geom_bar(stat = "count")



# data cleaning
df <- df %>% 
  filter(!str_detect(full_text, '^RT')) %>%  # remove retweets
  mutate(
    full_text = gsub("^\\s*|\\s*$", "", full_text), # remove leading & trailing whitespace
    full_text = iconv(full_text, "latin1", "ASCII", sub = ""), # remove emojis/dodgy unicode
    full_text = gsub("<(.*)>", "", full_text),  # remove unicodes
    full_text = gsub("\\.", "", full_text), # remove fullstops
    full_text = gsub("  ", " ", full_text), # replace double space with single space
    full_text = gsub("https(.*)*$", "", full_text), # remove url
    # full_text = gsub("[a-zA-Z0-9]+:", "", full_text), # remove ending colon
    full_text = gsub("^\\w+:\\w+\\s+", "", full_text), # remove colons
    full_text = gsub("@[a-z,A-Z]*", "", full_text), # remove other screenames
    full_text = gsub("#[a-z,A-Z]*", "", full_text), # remove hashtags
    full_text = gsub("[[:punct:]]", "", full_text),  # remove punctuation
    full_text = str_to_lower(full_text)
    )  
  
# remove stop words, tokenization
df_clean <- 
  df %>% 
  unnest_tokens(word, full_text) %>% 
  anti_join(stop_words)

# find 20 most frequent words in tweets
df_clean %>% 
  count(word, sort = TRUE) %>% 
  print(n = 20)

# visualize this info as barplot
df_clean %>% 
  count(word, sort = TRUE) %>% 
  head(20) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_col(aes(fill = n)) +
  labs(x = "Words", 
       y = "Count",
       title = "Frequency of Words by Count") +
  coord_flip() +
  geom_text(aes(label = n), size = 3, hjust = 1.2, color = "white")
  

# visualize word frequency info as wordclouds
df_clean %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>% 
  wordcloud2(color = 'random-dark', size = 0.5)

# sentiment analysis:
# classify words into positive/negative with "bing" & "nrc"
# and calculate each word's sentiment score by "afinn"
# need to download "textdata" library first
bing <-
  df_clean %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T)

nrc <-
  df_clean %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort = T)

afinn <-
  df_clean %>% 
  inner_join(get_sentiments("afinn")) %>% 
  count(word, value, sort = T)


# calculate the distribution of sentiment score with "afinn"
afinn_value <-
  afinn %>% 
  group_by(value) %>% 
  summarise(counts = sum(n))

# barplot on the distribution of sentiment score
afinn_value %>% 
  ggplot(aes(x = value, y = counts)) +
  geom_col(aes(fill = counts)) +
  labs(title = "frequency of sentiment score",
       x = "sentiment score",
       y = "frequency")

# barplot on the distribution of sentiment type
bing_value <-
  bing %>% 
  group_by(sentiment) %>% 
  summarise(counts = sum(n))

bing_value %>% 
  ggplot(aes(x = sentiment, y = counts)) +
  geom_col(aes(fill = counts)) +
  labs(title = "frequency of sentiment score",
       x = "sentiment score",
       y = "frequency")


# identify most common positive and negative words
bing %>% 
  group_by(sentiment) %>% 
  slice_max(n, n = 10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "", y = " ")
  





