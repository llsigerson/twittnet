#' recip_mentioners
#' 
#' Tracks the mentions given and received for a user in a specified timeframe and archives the
#' tweets of the user and the people they talk to.
#' @param user a user screen name or ID
#' @param archive pre-existing archive of tweets that can be used rather than having to re-collect tweets
#' @param preset.contacts vector of user screen names or IDs. If specified, all other users will be ignored in results.
#' @param min.tie the minimum tie strength (number of mentions exchanged) to include in the results.
#' @param trim.arch logical. Whether to remove a user's tweets from the archive if their tie strength with 
#' the original user is less than the minimum tie strength specified.
#' @param verbose logical. How often the function should print updates on its progress.
#' @param sleepy logical. If true, the function waits 60 seconds after processing each of the user's contacts
#' @param ... additional arguments to pass to get_tweets within the function, i.e. to specify the timeframe
#' 
#' 
#' @return mentions: a tibble that tracks the user's mentions of others. Each row specifies a user ID of a contact,
#' the mentions given to that contact, the mentions received from that contact, and the minimum of these last two
#' (the indicator of tie strength in this function)
#' @return archive: a tibble containing all tweets sent by the user and their contacts in the specified time frame
#' 
#' @details contacts whose accounts are private or suspended will have an NA in the mentions received column
#' of the mentions tibble
#' @details Depending on how sociable the user is, this function can run for a long time and collect
#' a lot of data, which can raise a variety of issues. Higher values of min.tie or specifying 
#' preset.contacts can prevent this. 
#' If trim.arch is set to FALSE, the archive can easily get quite 
#' large and overwhelm your system's memory.
#' @details For most users, the function has no problem with the rate limit. However, for users with a huge number
#' of contacts (>300), the function can break. Setting sleep to TRUE can prevent this.
#' 
#' @export


recip_mentioners<- function(user,archive=NULL,preset.contacts=NULL, min.tie=2, verbose=FALSE, trim.arch=T, sleepy=F,...){
  #Given a user, outputs a tibble showing mentions given and received between the user and their
  #contacts in the timeframe. Also outputs an archive of the user and their contacts' tweets
  #Pre defined functions and values are on lines 7-88
  #Actual function sequence is on lines 90-132
  
  #pre defined functions and values:
  
  createMentions<- function(tweets){
    # creates an initial tibble of mentions and cleans up the contact list
    mentionees= get_mentionees(tweets)
    mentions= tibble("contact"= as.character(mentionees$user_ID), "mentions_given"= mentionees$count, 
                     "mentions_received"= numeric(nrow(mentionees)))
    #if contacts are pre-specified, restrict the mentions dataframe to them
    if(!is.null(preset.contacts)){
      mentions<-mentions[which(mentions$contact%in%preset.contacts),]
    }
    
    #remove the user from the mentions list (can happen with ego retweets)
    mentions<- mentions[!(mentions$contact==user),]
    
    #remove contacts who were mentioned fewer than the minimum tie strength specified
    mentions<- mentions[mentions$mentions_given>=min.tie,]
    return(mentions)
  }
  
  getReceivedMentions<- function(mentions, archive){
    #given a tibble of mentions, updates the mentions received colum
    #by checking tweets of all the listed contacts
    #also outputs updated archive
    if(verbose){print(c("Expected Number of contacts:", nrow(mentions)))}
    for (i in 1:nrow(mentions)){
      if(verbose){print(c("contact number:", i, "user ID:", mentions$contact[i]))}
      
      #first, check if the contact has a problem account and handle accordingly
      contact<- mentions$contact[i]
      contact.problems<- problem_account_check(contact)
      
      if(contact.problems$Wrong.or.Suspended|contact.problems$Private){
        mentions$mentions_received[i]<-NA
      }
      else if(contact.problems$No.tweets){
        mentions$mentions_received[i]<-0
      } 
      
      else{
      #if the contact's tweets aren't in the archive, add them
      if(!contact%in%archive$user_id){
        contact.tweets= get_tweets(contact,...)
        archive<- rbind(archive, contact.tweets)
        }
      #get contact's tweets from archive
      contact.tweets<- archive[archive$user_id==contact,]
      #If the user was mentioned by the contact, update the mentions_received column accordingly
      if(nrow(contact.tweets)>0){
        contact.mentionees<-get_mentionees(contact.tweets)
        if(nrow(contact.mentionees)>0){
          if(user%in%contact.mentionees$user_ID){
            mentions$mentions_received[i]<- contact.mentionees$count[which(contact.mentionees$user_ID==user)]
          }
        }
        #If trim archive is specified, and the mentions received column is less than the minimum tie, 
        #remove that contact's tweets from the archive
        if(trim.arch&mentions$mentions_received[i]<min.tie){
          archive<- archive[!archive$user_id==contact,]
        }
      }
      }
      if(sleepy){
        print("sleeping")
        Sys.sleep(60)}
    }
    return(list("mentions"=mentions, "archive"=archive))
  }
  
  #empty mentions tibble, which will be returned in any case where the user has no mentions
  empty.mentions<- tibble("contact"= character(0), "mentions_given"=numeric(0),
                          "mentions_received"=numeric(0), "min"=numeric(0))
  
  
  #Begin actual function sequence here
  #First, check if the specified account is a problem account and return appropriate output if so
  user.problems<- problem_account_check(user)
  if(TRUE%in%user.problems[,2:4]){
    if(verbose){print(user.problems)}
    if(user.problems$No.tweets){
      return(list("mentions"=empty.mentions, "archive"=get_timeline("25073877")[0,]))
    }
    else{
      return(list("mentions"=tibble("contact"= NA, "mentions_given"= NA,"mentions_received"= NA, "min"=NA),
                  "archive"=get_timeline("25073877")[0,]))
    }
  }
  
  #In case a screen name was entered, convert it to a user id
  if(is.na(suppressWarnings(as.numeric(user)))){
    user<- lookup_users(user)$user_id
  }
  
  #Make archive from empty tibble of tweets if it isn't included in the function call
  if(is.null(archive)){
    archive<- get_timeline("25073877")[0,]
  }
  
  #get first batch of tweets from the user if they aren't in the archive
  if(!user%in%archive$user_id){
    tweets= get_tweets(user, ...)
    #if there's no tweets in the timeframe, return empty.mentions tibble
    if(nrow(tweets)==0){return(list("mentions"=empty.mentions, "archive"=0))}
    archive<- rbind(archive, tweets)
    }
 
  #get user's tweets from archive
  tweets<- archive[which(archive$user_id==user),]
  
  #Create mentions tibble end, the function if it's empty
  mentions<-createMentions(tweets)
  if(nrow(mentions)==0){
    return(list("mentions"=mentions, "archive"= archive))
  }
  
  #update the Mentions received column and archive
  prelim.results<- getReceivedMentions(mentions, archive)
  mentions<- prelim.results$mentions
  #Add in "min" column, trim mentions, and return list
  mentions$min<-apply(mentions[,2:3], 1, min, na.rm=TRUE)
  mentions<- mentions[!mentions$min<min.tie,]
  return(list("mentions"=mentions, "archive"= prelim.results$archive))
  }




