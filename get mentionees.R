#function to get all mentions (including retweets and replies)
#from a dataframe of a user's tweets
getmentionees= function(tweets){
  mentionees<- tibble("user_ID"=character(), "count"= numeric())
  for (i in 1:nrow(tweets)){
    if(!is.na(tweets$mentions_user_id[i])){
      IDlist<- unique(strsplit(tweets$mentions_user_id[[i]], fixed=T, split=" "))
      for (j in 1:length(IDlist)){
        ID= IDlist[[j]]
        #If the ID is already in the tibble, add one to the count value for it. If not, add it to the tibble
        if(ID%in%mentionees$user_ID){
          mentionees$count[which(mentionees$user_ID==ID)]<-mentionees$count[which(mentionees$user_ID==ID)]+1
        }
        else{
          mentionees=rbind(mentionees, tibble("user_ID"=ID, "count"=1))
          }
        }
    }
    
  }
  mentionees$user_ID<- as.character(mentionees$user_ID)
  #sort the output so that higher mentions come first
  mentionees<-mentionees[order(mentionees$count, decreasing = TRUE),]
  return(mentionees)
  
  
}

