#' problem_account_check
#' 
#' Checks for various problems with accounts that preclude further
#' data collection by other functions in the package
#' 
#' @param users A character vector of user IDs or screen names
#' @return a logical tibble with 5 columns. Each row notes the user and whether 
#' the account has any problematic features
#' 
#' @details Due to rate limits, this function doesn't work with more than 5000 users at a time. If this is
#' attempted, the function fails.
#' 
#' @export

problem_account_check<- function(users, return_info=F){
  if(length(users)>5000){
    stop("More than 5000 users, cannot generate accurate results")
  }
  user.info<- lookup_users(users)
  #create blank tibble
  blank.var<- logical(length(users))
  account.problems<- tibble("User"= users, "Wrong.or.Suspended"= blank.var, "Private"= blank.var,
                            "No.tweets"= blank.var, "No.friends.or.followers"=blank.var)
  #iterate through users vector and update tibble accordingly
  for(i in 1:length(users)){
    #identify location of user's info in userinfo
    loc<- which(user.info$user_id==users[i]|user.info$screen_name==users[i])
    #If the given user has wrong info or a suspended account, they won't appear in the
    #result.
    if(length(loc)==0){
      account.problems$Wrong.or.Suspended[i]<-TRUE
      account.problems[i,3:5]<-NA
    }
    else{
      account.problems$Private[loc]<-user.info$protected[loc]
      if(user.info$statuses_count[loc]==0){account.problems$No.tweets[loc]<- TRUE}
      if(user.info$friends_count[loc]==0|user.info$followers_count[loc]==0){
        account.problems$No.friends.or.followers[loc]<-TRUE
      }
    }
  }
  if(return_info){
    return(list("account.problems"=account.problems, "user.info"=user.info))
  }
  return(account.problems)
}

