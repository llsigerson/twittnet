#' get_tweets
#'
#' This function collects all of a user's tweets for a given timeframe (default is last 60 days).
#' @param startday Earliest tweets to collect
#' @param stopday Latest tweets to collect
#' @param verbose whether the function will notify you if results are potentially problematic (i.e, private 
#' account, incorrect user ID, etc.).  
#' @return a tibble containing tweets
#' @examples get_tweets("YouTube", startday= Sys.Date()-5, verbose=T)
#' @export


get_tweets= function(user, startday=Sys.Date()-30, stopday=Sys.Date(), verbose=FALSE){
  
  #First, attempt to get a user's tweets
  tweets= suppressWarnings(try(get_timeline(user,n=3200, check=FALSE), silent = T))
  
  #Next, check for abnormal accounts and return appropriate output if they're found
  #Private account
  if(class(tweets)[1]=="try-error"){
    if(tweets[[1]]=="Error in UseMethod(\"as.integer64\") : \n  no applicable method for 'as.integer64' applied to an object of class \"list\"\n"){
      if(verbose){print("Private Account found")}
      return(NA)
    }
    }
  #Never sent a tweet or wrong ID
  if(ncol(tweets)==0){
    if(verbose){print("Never sent a tweet or wrong ID")}
    return(as_tibble(matrix(nrow=0,ncol=42)))
  }
  
  #Next, trim tweets using the startday and stopday specified
  trimmedtweets<- try(tweets[which(as.Date(tweets$created_at)>=startday&as.Date(tweets$created_at)<=stopday),])
  
  #Last, check that the function didn't hit the max number of tweets available for the participant
  if(nrow(trimmedtweets)>3000&verbose==T){
    print("More than 3000 tweets, may have hit the limit!")
  }
  return(trimmedtweets)
}
  
