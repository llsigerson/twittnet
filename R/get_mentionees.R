#' get_mentionees
#'
#' This function takes a tibble of tweets and records which users were mentioned, 
#' and how many times. Mentions include replies and retweets, but not quotes.
#' @return a tibble with two columns noting the user ID and how many times they were mentioned
#' @examples get_mentionees(tweets)
#' @export

get_mentionees= function(tweets){
  #build tibble to record mentionees
  mentionees<- tibble("user_ID"=character(), "count"= numeric())
  
  #Then, iterate through each row of the tweets and record the mentionees
  for (i in 1:nrow(tweets)){
    #this if statement checks that there are actually mentions in the tweet
    if(!is.na(tweets$mentions_user_id[i])){
      #create list of all unique ID's mentioned in the tweet
      IDlist<- unique(strsplit(tweets$mentions_user_id[[i]], fixed=T, split=" "))
      #Then, iterate through this list and update the mentionees tibble accordingly
      #If the ID is already in the tibble, add one to the count value for it. If not, add it to the tibble
      for (j in 1:length(IDlist)){
        ID= IDlist[[j]]
        if(ID%in%mentionees$user_ID){
          mentionees$count[which(mentionees$user_ID==ID)]<-mentionees$count[which(mentionees$user_ID==ID)]+1
        }
        else{
          mentionees=rbind(mentionees, tibble("user_ID"=ID, "count"=1))
          }
        }
    }
  }
  #last, sort the output so that higher mentions come first
  mentionees<-mentionees[order(mentionees$count, decreasing = TRUE),]
  return(mentionees)
}

