#' reformat_tweets
#' 
#' Reformats the tibbles containing tweets from functions in either rtweet or twittnet and
#' makes them into a clean dataframe that can be written to a csv or other file
#' @param tweets a tibble of tweets
#' @return a dataframe of tweets 
#' @export

reformat_tweets<- function(tweets){
  newtweets<-tweets
  newtweets$hashtags<- sapply(tweets$hashtags, paste, collapse= " ")
  newtweets$symbols<- sapply(tweets$symbols, paste, collapse= " ")
  newtweets$urls_url<- sapply(tweets$urls_url, paste, collapse= " ")
  newtweets$urls_t.co<- sapply(tweets$urls_t.co, paste, collapse= " ")
  newtweets$urls_expanded_url<- sapply(tweets$urls_expanded_url, paste, collapse= " ")
  newtweets$media_url<- sapply(tweets$media_url, paste, collapse= " ")
  newtweets$media_t.co<- sapply(tweets$media_t.co, paste, collapse= " ")
  newtweets$media_expanded_url<- sapply(tweets$media_expanded_url, paste, collapse= " ")
  newtweets$media_type<- sapply(tweets$media_type, paste, collapse= " ")
  newtweets$ext_media_url<- sapply(tweets$ext_media_url, paste, collapse= " ")
  newtweets$ext_media_t.co<- sapply(tweets$ext_media_t.co, paste, collapse= " ")
  newtweets$ext_media_expanded_url<- sapply(tweets$ext_media_expanded_url, paste, collapse= " ")
  newtweets$mentions_user_id<- sapply(tweets$mentions_user_id, paste, collapse= " ")
  newtweets$mentions_screen_name<- sapply(tweets$mentions_screen_name, paste, collapse= " ")
  newtweets$geo_coords<- sapply(tweets$geo_coords, paste, collapse= " ")
  newtweets$coords_coords<- sapply(tweets$coords_coords, paste, collapse= " ")
  newtweets$bbox_coords<- sapply(tweets$bbox_coords, paste, collapse= " ")
  
  NAlocs<-which(is.na(as.matrix(tweets)))
  
  newtweets<-as.matrix(newtweets)
  newtweets[NAlocs]<-NA
  
  return(as.data.frame(newtweets))
  }
  

  
  