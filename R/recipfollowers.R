recipfollowers= function(user,  maxfollowers=300000, maxfriends=20000){
  
  #first, check for strange users, with a token that's not in the top three 
  #(which can get exhausted by other functions)
  infotokenloc<- sample(3:length(get_tokens()), 1)
  info<- lookup_users(user, token=get_tokens()[[infotokenloc]])
  #First, check for a mistaken ID, using the lookup users function
  if(ncol(info)==0){
  print("mistaken ID")
  return(NA)
  }
  
  #then, check for an account that's either private, has 0 followers or friends, or is a "large" account
  if(info$protected==T){ 
    if(!exists("privateaccounts")){
      privateaccounts<<-user
      }    
    else{  
    privateaccounts<<- c(user, privateaccounts)
       
    }
    #print("private account found")
    return(NA)
  }
  
  else if(info$followers_count==0|info$friends_count==0){
      #print("0 friends or followers")
      return(character(0))
    }
    
  if(info$followers_count>maxfollowers|info$friends_count>maxfriends){
    if(!exists("largeaccounts")){
      largeaccounts<<-user
      }
    else{
    largeaccounts<<- c(largeaccounts, user)
    }
    #print("Large Account found")
    return(NA)
    
  }
  
  #Next, we get followers
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
  
  #print(c("# of reciprocators:",length(followrecip)))
  return(followrecip)
}

