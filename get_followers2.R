
get_followers2<- function(username,...){
  #if the token and count variables we need aren't there, make them
  if(!exists("get_followers2token")){
    get_followers2token<<- get_tokens()[[1]]
    get_followers2count<<-1
    }
  followers<- try(get_followers(username, token= get_followers2token,...), silent = TRUE)
  ntokens=length(get_tokens())
  
  #this loop handles the extremely rare case where the token has no 
  #rate limit left for rate limit checking
   # if(class(followers)=="try-error"){
   #   if(get_followers2count==ntokens){get_followers2count<<- 1}
   #   else{get_followers2count<<-get_followers2count+1}
   #   print(c(c("Followers Token number:", get_followers2count)))
   # 
   #   get_followers2token<<- get_tokens()[[get_followers2count]]
   #   followers<- get_followers(username, token= get_followers2token,...)
   #   }
   # 
  #check for private account
  if(class(followers)[1]=="try-error"){
    print("private account")
    return(NA)
  }
  #handle rate limit. 
  while(nrow(followers)==0){
    #retry the get_followers function, with the next token 
    #(updating the get_followers2 count and get_followers2token variables) 
    if(get_followers2count==ntokens){get_followers2count<<- 3}
    else{get_followers2count<<-get_followers2count+1}
    #print(c(c("Followers Token number:", get_followers2count)))
    
    get_followers2token<<- get_tokens()[[get_followers2count]]
    followers<- get_followers(username, token= get_followers2token,...)
    
    
  }
  
  return(followers)
}

