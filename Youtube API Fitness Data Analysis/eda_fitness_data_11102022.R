require(curl)
require(jsonlite)
library(dplyr)
library(readxl)
library(xlsx)

getwd()
setwd("/Users/Jessicaboomboom/Desktop/Pilates project")

channels = read_excel("./channels_info.xlsx") %>% 
  janitor::clean_names()
videos_info = read_excel("./videos_info.xlsx") %>% 
  janitor::clean_names()
video_list = read_excel("./video_list.xlsx") %>% 
  janitor::clean_names()

# structure & summaries of df
str(channels)
summary(channels)
str(videos_info)
summary(videos_info)
str(video_list)
summary(video_list)

sum(is.null(channels))
sum(is.null(videos_info))
sum(is.null(video_list))

# data transformation - change data types
channels <- 
  channels %>% 
  mutate(
    view_count = as.numeric(view_count),
    subscriber_count = as.integer(subscriber_count),
    video_count = as.numeric(video_count)
  )

videos_info <-
  videos_info %>% 
  mutate(
    date = as.Date(date)
  )

video_list <- 
  video_list %>% 
  mutate(
    date = as.Date(date, format = "%Y-%m-%d"),
    like_count = as.numeric(like_count),
    comment_count = as.numeric(comment_count)
  )


# EDA
# channels
summary(channels)
sum(channels$video_count)
# barplot of channels df - subscribers
channels %>% 
  ggplot(aes(x = forcats::fct_reorder(name, subscriber_count), y = format(subscriber_count, scientific = F, big.mark = ','))) +
  geom_bar(stat = 'identity', width = 0.8, aes(fill = subscriber_count)) +
  coord_flip() +
  theme_grey() +
  scale_fill_gradient() +
  labs(title = 'Distribution of Subscribers',
       x = 'Channel', y = 'Counts') +
  theme(legend.position = 'none', axis.text=element_text(size=8, face = 'bold')) 

# barplot of channels df - videos
channels %>% 
  ggplot(aes(x = forcats::fct_reorder(name, video_count), y = video_count)) +
  geom_bar(stat = 'identity', width = 0.8, aes(fill = video_count)) +
  coord_flip() +
  theme_grey() +
  scale_fill_gradient() +
  labs(title = 'Distribution of Videos',
       x = 'Channel', y = 'Counts') +
  theme(legend.position = 'none', axis.text=element_text(size=8, face = 'bold')) +
  geom_hline(yintercept = mean(channels$video_count))


# video_list
summary(video_list)

# top 10 videos in likes
top_10_likes = video_list[order(video_list$like_count, decreasing = T),] %>% head(10) %>% 
  select(1:3,5:6) %>% 
  mutate(video = substr(video, 1,100))
# top 10 views in comments
top_10_comments = video_list[order(video_list$comment_count, decreasing = T),] %>% head(10) %>% 
  select(1:3,5:6) %>% 
  mutate(video = substr(video, 1,80))



# video likes for each channel in descending order
View(video_list %>% 
  group_by(channel) %>% 
  summarize(total_like = sum(like_count),
            mean_like = round(mean(like_count), digits = 0),
            min_like = min(like_count)) %>% 
  arrange(desc(total_like)))

# video comments for each channel in descending order
View(video_list %>% 
  group_by(channel) %>% 
  summarize(total_comment = sum(comment_count),
            mean_comment = round(mean(comment_count), digits = 0),
            min_comment = min(comment_count))%>% 
  arrange(desc(total_comment)))




# boxplot of like counts 
video_list %>% 
  ggplot(aes(x = forcats::fct_reorder(channel, like_count, .fun = sum, .desc = T), y = like_count, fill = factor(channel))) +
  geom_boxplot(
    alpha = 0.7,
    outlier.color = '#17a640',
    outlier.shape = 19,
    outlier.size = 4,
    width = 0.7, 
    color = '#8a8689'
  ) +
  theme_grey() +
  labs(title = 'Like Counts for YouTube Channels',
       y = 'Like Counts', x = '') +
  theme(
    legend.position = 'none',
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, face = 'bold')
  )


# detect outliers for like counts
video_list %>% 
  group_by(channel) %>% 
  mutate(is_outlier = ifelse(
    like_count > quantile(like_count, 0.75) + 1.5 * IQR(like_count), 'TRUE', 'FALSE')) %>% 
  filter(is_outlier == 'TRUE')


# frequency of videos published
View(video_list %>% 
  group_by(channel) %>% 
  summarize(day = 
              round(as.numeric(as.POSIXct(max(date)) - as.POSIXct(min(date)))/15, digits = 0)))



                   









