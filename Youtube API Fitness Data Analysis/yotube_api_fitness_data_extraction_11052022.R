require(curl)
require(jsonlite)
library(dplyr)
library(readxl)
library(xlsx)


# read the excel I have prepped 
setwd("/Users/Jessicaboomboom/Desktop")
channel_data = read_excel("youtube_fitness_data.xlsx") %>% 
  janitor::clean_names()
API_key = "YOUR API KEY HERE!"


# use the Channel - list method to fetch information about a channel
# set the "part" parameter to fetch these objects/info: snippet, contentDetails, statistics

# the snippet object contains basic details about the channel, such as its title, description, and thumbnail images
# the contentDetails object encapsulates information about the channel's content
# the statistics object encapsulates statistics for the channel

# use "fromJSON" function to convert the JSON content (from url) to an R list
# under the "items" property, it shows a list of channel-related info that match the request criteria: fetch snippet, contentDetails, statistics objects 
# we're interested in these info: channel title, statistics (viewCount, commentCount, subscriberCount, videoCount), playlist ID in this channel
# put all the info together in a dataframe

get_channel_info <-function(channel_id,API_key){
  
  url=paste0('https://www.googleapis.com/youtube/v3/channels?part=snippet,contentDetails,statistics&id=',channel_id,'&key=',API_key)
  result <- fromJSON(url) 

  channel_title = result$items$snippet$title
  view_count = result$items$statistics$viewCount
  subscriber_count = result$items$statistics$subscriberCount
  video_count = result$items$statistics$videoCount
  playlist_id = result$items$contentDetails$relatedPlaylists$uploads

  channel_info = data.frame(
    name = channel_title,
    view_count = view_count,
    subscriber_count = subscriber_count,
    video_count = video_count,
    playlist_id = playlist_id
  )
  return(channel_info)
}

# next step, write a function to loop through each channel id and put its info in a data
combine_channel_info <- function(API_key){
    channels_info = data.frame()
    for(i in 1:length(channel_data$channel_id)){
      each_channel_info = get_channel_info(channel_data$channel_id[i],API_key)
      channels_info = rbind(channels_info,each_channel_info)
    }
    return(channels_info)
}
channels = combine_channel_info(API_key)



# Based on the playlist IDs we have fetched from each channel
# we're gonna extract a list of videos with its videoID and video published date
get_videos_info <-function(playlist_id,API_key, number){

  url=paste0('https://www.googleapis.com/youtube/v3/playlistItems?part=contentDetails&playlistId=',playlist_id,'&key=',API_key,'&maxResults=',number)
  result <- fromJSON(url) 
  
  video_id = result$items$contentDetails$videoId
  date = result$items$contentDetails$videoPublishedAt
  
  videos_info = data.frame(
    video_id= video_id,
    date = date
  )
  
  return(videos_info)
}


# now let's loop through each channel and combine their videos info together
combine_videos_info <- function(API_key){
  videos_info = data.frame()
  for(i in 1:length(channels$playlist_id)){
    each_playlist_info = get_videos_info(channels$playlist_id[i],API_key,15)
    videos_info = rbind(videos_info,each_playlist_info)
  }
  return(videos_info)
}
videos = combine_videos_info(API_key)


# function to extract stats about each video
video_stats <-function(video_id,API_key){
  
  url=paste0("https://www.googleapis.com/youtube/v3/videos?part=snippet,statistics&id=",video_id,"&key=",API_key)
  result <- fromJSON(url) 
  
  channel_title = result$items$snippet$channelTitle
  video_name = title=result$items$snippet$title
  like_count = result$items$statistics$likeCount
  comment_count = result$items$statistics$commentCount
  date = result$items$snippet$publishedAt
  description=result$items$snippet$description
  
  videos = data.frame(
    channel = channel_title,
    video = video_name,
    date = date,
    description = description,
    #view_count = view_count,
    like_count = like_count,
    comment_count = comment_count
  )
  return(videos)
}


# now let's loop through each channel and combine their videos info together
video_list_info <- function(API_key){
  video_list = data.frame()
  for(i in 1:length(videos$video_id)){
    each_video_info = video_stats(videos$video_id[i],API_key)
    video_list = rbind(video_list,each_video_info)
  }
  return(video_list)
}
video_list = video_list_info(API_key)



# export the result tables in excel form
write.xlsx(channels, "channels_info.xlsx")
write.xlsx(videos, "videos_info.xlsx")
write.xlsx(video_list, "video_list.xlsx")








