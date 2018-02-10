#' recip_followers
#' 
#' Finds the reciprocated followers of a given user. Handles rate limits and various problematic accounts, including
#' mistaken ID's, private accounts, suspended accounts, or accounts with too many friends or followers to process
#' in a timely way (this can be defined by the user).
#' @param user a user screen name or ID
#' @param max.followers the maximum number of followers an account can have before it's considered a "large account" 
#' and not processed.
#' @param max.friends similar to max.followers, but for friends.
#' @param verbose whether the function should provide updates on its progress.
#' 
#' @return a character vector of user IDs who are reciprocated followers with the user. If this vector has a length
#' of 0, then the user has no reciprocated followers



recip_followers= function(user,  max.followers=300000, max.friends=20000, verbose=F){
  #sub functions: modified versions of get_friends and get_followers that handle rate limit issues
  #they wait, then try again until a good result is returned
  robust_get_followers<- function(user,n=75000,...){
    followers<- try(get_followers(user, n=n, retryonratelimit = T, verbose = T), silent = T)
    while(class(followers)[1]=="try-error"){
        Sys.sleep(30)
        followers<- try(suppressWarnings(get_followers(user, n=n, retryonratelimit = T, parse = T)), silent = T)
      }
    return(followers)
  }
  robust_get_friends<- function(user, n=5000,...){
    friends<- suppressWarnings(get_friends(user, n=n, retryonratelimit = T, verbose = T))
    while(class(friends)[1]=="list"){
      if(verbose){print("No available tokens, sleeping for 5 minutes")}
      Sys.sleep(300)
      friends<- get_friends(user)
    }
    return(friends)
  }
  
  #first, handle various problems with the user's account
  
  problem.check.result<- problem_account_check(user, return_info = T)
  user.problems<- problem.check.result$account.problems
  user.info<- problem.check.result$user.info
  #0 friends or followers
  if(TRUE%in%user.problems$No.friends.or.followers){
    return(character(0))
  }
  
  #handle mistaken ID or suspended account
  if(TRUE %in%user.problems[,2:3]){
    return(user.problems)
  }
  #large account
  if(user.info$followers_count>max.followers|user.info$friends_count>max.friends){
    if(verbose){print("Large Account found")}
    return(user.problems)
    }
  
  #If the account has none of these problems, next, we get followers
  followers= robust_get_followers(user)
  
  #Loop to get complete followers list if it's more than the 5000 per standard query
  nextcursor<- next_cursor(followers)
  while(nextcursor!="0"){
    #print(">5k followers loop working")
    newfollowers<- robust_get_followers(user, page=nextcursor)
    followers<- rbind(newfollowers,followers)
    print(dim(followers))
    nextcursor<- followers[[2]]$next_cursor_str
    print(c("followers nextcursor:",nextcursor))
   }
 
  #Now we get the friends
  friends= robust_get_friends(user)
 
  #if the number of friends is a multiple of 5000, (the max allowed in one query)
  #keep querying until it isn't anymore
  nextcursor<- next_cursor(friends)
  
  while(nextcursor!="0"){
    newfriends<- robust_get_friends(user, page= nextcursor)
    friends= rbind(newfriends,friends)
    nextcursor<- next_cursor(friends)
  }
  
  friends<- friends$user_id
  followers<- followers$user_id
  followrecip<- intersect(friends,followers)
  
  if(verbose){print(c("# of reciprocators:",length(followrecip)))}
  return(followrecip)
}

