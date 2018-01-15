recipfollowers= function(user,  maxfollowers=300000, maxfriends=20000, verbose=F){
  #first, handle various problems with the user's account
  user.problems<- problem_account_check(user)
  #handle mistaken ID or suspended account
  if(user.problems$){
    if(verbose){print(user.problems)}
    return(NA)
  }
  #0 friends or followers
  if(user.problems$){
    if(verbose){print(user.problems)}
    return(character(0))
  }
  #private account
  if(user.problems$){
    if(verbose){print(user.problems)}
    return(NA)
  }
  #large account
  if(user.info$followers_count>maxfollowers|user.info$friends_count>maxfriends){
    if(verbose){print("Large Account found")}
    return(NA)
    }
  
  #If the account has none of these problems, next, we get followers
  followers= get_followers2(user)
  
 
 
  #Loop to get complete followers list if it's more than the 5000 per standard query
  nextcursor<<-next_cursor(followers)
  while(nextcursor!="0"&class(followers)[1]!="try-error"){
    #print(">5k followers loop working")
    newfollowers<- get_followers2(user, page=nextcursor)
    followers<- rbind(newfollowers,followers)
    nextcursor<<- next_cursor(followers)
    
  }
 
  #Now we get the friends
  friends= get_friends2(user)
 
  #if the number of friends is a multiple of 5000, (the max allowed in one query)
  #keep querying until it isn't anymore
  nextcursor<<- next_cursor(friends)
  
  while(nextcursor!="0"){
    newfriends<- get_friends2(user, page= nextcursor)
    friends= rbind(newfriends,friends)
    nextcursor<<- next_cursor(friends)
  }
  
  friends<- friends$user_id
  followers<- followers$user_id
  followrecip<- intersect(friends,followers)
  
  if(verbose){print(c("# of reciprocators:",length(followrecip)))}
  return(followrecip)
}

