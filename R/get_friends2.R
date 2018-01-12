
get_friends2<- function(user,...){
  #if the token and count variables we need aren't there, make them
  if(!exists("get_friends2token")){
    get_friends2token<<- get_tokens()[[1]]
    get_friends2count<<-1
    
  }
  
  friends<- try(get_friends(user, token= get_friends2token,...), silent=TRUE)
  
  ntokens=length(get_tokens())
  
  #loop to handle rate limit issues
  # when I hit the rate limit with the get_friends function when specifying a token
  # it returns a dataframe with null dimensions, so I use this to identify the problem.
  loopcount=1
  while(is.null(dim(friends))){
    #on the first loop, check that the problem isn't a mistaken ID (which returns the same thing)
    #if it is, return a list of a null dataframe (which would be returned in the get_friends function)
    if(loopcount==1){
      if(suppressMessages(ncol(lookup_users(user, token= get_tokens()[[4]])))==0){
        print("mistaken ID")
        return(NA)
      }
    }
    loopcount<- loopcount+1
    #retry the get_friends function, with the next token 
    #(updating the get_friends2 count and get_friends2token variables) 
    if(get_friends2count==ntokens){get_friends2count<<- 3}
    else{get_friends2count<<-get_friends2count+1}
    #print(c(c("Friends Token number:", get_friends2count)))
    
    get_friends2token<<- get_tokens()[[get_friends2count]]
    friends<- try(get_friends(user, token= get_friends2token,...), silent=TRUE)
    }
  #for handling private accounts
  if(is.null(friends$user_id)){
    print("private account found")
    return(NA)
  }
  
  return(friends)
}


