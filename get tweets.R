
get_tweets= function(user, startday=Sys.Date()-60, stopday=Sys.Date()){
  
  tweets= try(get_timeline(user,n=4000, check=FALSE), silent = T)
  
  #First, deal with abnormal accounts
  while(class(tweets)[1]=="try-error"){
    #private account
    if(tweets[[1]]=="Error in UseMethod(\"as.integer64\") : \n  no applicable method for 'as.integer64' applied to an object of class \"list\"\n"){
      print("Private Account found")
      return(NA)
    }
    Sys.sleep(60)
    tweets= try(get_timeline(user,n=4000, check=FALSE), silent = T)
    
    }
  #Never sent a tweet
  if(is.null(tweets)){
    print("0 tweets")
    return(as_tibble(matrix(nrow=0,ncol=42)))
  }
  if(ncol(tweets)==0){
    print("tweets with 0 columns, wtf??")
    mistakentweets1<<- tweets
    Sys.sleep(30)
    tweets= try(get_timeline(user,n=4000, check=FALSE), silent = T)
    if(ncol(tweets)==0){
    print("0 tweets")
    return(as_tibble(matrix(nrow=0,ncol=42)))
    }
  }
  tweets2<- try(tweets[which(as.Date(tweets$created_at)>=startday&as.Date(tweets$created_at)<=stopday),])
  if(class(tweets2)[1]=="try-error"){mistakentweets2<<- tweets}
  return(tweets2)
  
}
  tweets<- get_tweets("llsigerson", startday = startday,stopday = stopday)
