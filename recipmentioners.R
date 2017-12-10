#New function: identify all other users who reciprocate 
#at least two mentions (including reply or retweet)
#with the given user: requires gettweets and getmentionees
#note that this version can only be used within the sociogram builder (with replies)
#note that this has been tested and found to achieve the exact same results as the original


recipmentioners<- function(user,preset.contacts=NULL,...){
  #first, check for some abnormal users
  userinfo<- lookup_users(user)
  if(ncol(userinfo)==0){return(NA)}
  if(userinfo$protected==TRUE){return(NA)}
  if(userinfo$statuses_count==0){
    return(return(as.tibble(matrix(nrow=0,ncol=0))))
    }
  
  user<- userinfo$user_id
  #get first batch of tweets from the user if they aren't in the archive, and check for abnormal users
  if(sum(archive$user_id==user)==0){
  tweets= get_tweets(user, ...)
  #if there's no tweets in the timeframe, return empty dataframe
  if(nrow(tweets)==0){return(as.tibble(matrix(nrow=0,ncol=0)))}
  archive<<- rbind(archive, tweets)
  }
  #get user's tweets from archive
  tweets<- archive[which(archive$user_id==user),]
  
  mentionees= getmentionees(tweets)
  
  mentions= tibble("contact"= as.character(mentionees$user_ID), "mentions_given"= mentionees$count, 
                       "mentions_received"= numeric(nrow(mentionees)))
  
  if(nrow(mentions)==0){return(as.tibble(matrix(nrow=0,ncol=0)))}
  
  #if contacts are pre-specified, restrict the mentions dataframe to them
  #if none are left, return a null dataframe
  if(!is.null(preset.contacts)){
    mentions<-mentions[which(mentions$contact%in%preset.contacts),]
    }
  
  #remove the user from the mentions list (can happen with ego retweets)
  if(user%in% mentions$contact){
  mentions<- mentions[-which(mentions$contact==user),]
  if(nrow(mentions)==0){return(as.tibble(matrix(nrow=0,ncol=0)))}
  }
  
  #remove contacts who were only mentioned once
  mentions<- mentions[mentions$mentions_given>1,]
  if(nrow(mentions)==0){return(as.tibble(matrix(nrow=0,ncol=0)))}
  
  #remove private  and suspended accounts
  info<- lookup_users(mentions$contact, token= get_tokens()[[60]])
  suspended<- if(nrow(info)<nrow(mentions)){
    mentions<- mentions[which(mentions$contact%in%info$user_id),]
    }
  
  privates<-which(info$protected==TRUE)
  if(length(privates)>0){
  mentions<- mentions[-privates,]
  }
  
  #If participant doesn't mention anyone (or any contacts), return empty dataframe
  if(nrow(mentions)==0){return(as.tibble(matrix(nrow=0,ncol=0)))}
  
  
  #loop that runs through each of the mentions and finds how much they're reciprocated
  print(c("nrow mentions:", nrow(mentions)))
  for (i in 1:nrow(mentions)){
    contact<- mentions$contact[i]
    print(c("contact number", i))
    #if the contact's tweets aren't in the archive, add them
    if(sum(archive$user_id==contact)==0){
      
      tweets2= get_tweets(contact,...)
      if(nrow(tweets2))
      archive<<- rbind(archive, tweets2)
    }
    #get contact's tweets from archive
    tweets2<- archive[which(archive$user_id==contact),]
    
    #If the user was mentioned by the contact, update the mentions_received column
    if(nrow(tweets2)>0){
      mentionees<-getmentionees(tweets2)
        if(nrow(mentionees)>0){
          if(user%in%mentionees$user_ID){
          mentions$mentions_received[i]<- mentionees$count[which(mentionees$user_ID==user)]
          }
        }
     #If the mentions received column is less than two, remove that contact's tweets from the archive
       if(mentions$mentions_received[i]<2){
        archive<<- archive[-which(archive$user_id==contact),]
        }
    }
    }
  mentions$min<-apply(mentions[,2:3], 1, min)
  mentions<- mentions[which(mentions$min>1),]
  return(mentions)
  
}
#archive<- get_timeline("llsigerson")[0,]
#results<- getreciprocators("llsigerson", startday=Sys.Date()-30)



