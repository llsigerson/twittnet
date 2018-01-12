#' recip_mentioners_network
#' 
#' Maps out a user's 1.5 level ego network (who are their contacts, and whether these contacts 
#' know each other), based on reciprocated mentions. Repeatedly uses the recip_mentioners function,
#' compiling an archive of all tweets sent by the user and their contacts in the timeframe. Tie strength
#' is defined as the minimum number of mentions exchanged between two users.
#' 
#' @param user a user screen name or ID
#' @param verbose logical. How often the function should print updates on its progress.
#' @param archive.to.csv logical. If set to TRUE, the archive will be written to a csv file
#' in the working directory rather than returned in the workspace. 
#' @param sleepy logical. Used to avoid rate limit issues for users with huge number of contacts (>300).
#' If true, the function waits 60 seconds after processing each of the user's 
#' contacts in the first use of the recip_mentioners function (used throughout this function).
#' In subsequent uses, sleepiness isn't required because the tweets have already been collected.
#' @param ... additional arguments to pass to recip_mentioners and get_tweets within the function.
#' 
#' @return user: the user ID of the original user
#' @return ncontacts: how many contacts the user has
#' @return funcstart: start time of function
#' @return funcstop: stop time of function
#' @return account.problems: the output of the problem_account_check function for the original user
#' @return sociogram: a matrix with identical rows and columns showing the tie strength between the user
#' and their contacts. The user's connection with themselves is marked as NA.
#' @return archive: a tibble containing all tweets sent by the user and their contacts in the specified 
#' time frame. 
#' @details Note: this function ignores accounts that are private or suspended.
#' @export  



recip_mentioners_network<- function(user, verbose=F, archive.to.csv=F, sleepy=F, ...){
  
  #predefined functions and values
  
  makeSociogram<- function(mentions){
    #converts the output of recip_mentioners to a sociogram for further processing
    #Remove suspended or private accounts
    mentions<-mentions[!is.na(mentions$mentions_received),]
    sociogram<- as.matrix(c(NA, mentions$min), nrow=ncontacts, ncol=1)
    colnames(sociogram)<- user
    rownames(sociogram)<- c(user, mentions$contact)
    return(sociogram)
  }
  
  
  
  
  funcstart<- Sys.time()
  #if screen name, convert to user ID
  if(is.na(suppressWarnings(as.numeric(user)))){
    user<- lookup_users(user)$user_id
  }
   
  #First, check if the specified account is a problem account and return appropriate output if so
  user.problems<- problem_account_check(user)
  if(TRUE%in%user.problems[,2:4]){
    if(verbose){print(user.problems)}
    if(user.problems$No.tweets){
      return(list("user"=user, "ncontacts"=0, "funcstart"=funcstart, "funcstop"=Sys.time(),
                  "account.problems"=user.problems,"sociogram"= matrix(NA,0,0),  
             "archive"=get_timeline("25073877")[0,]))
    }
    else{
      return(list("user"=user, "ncontacts"=NA, "funcstart"=funcstart, "funcstop"=Sys.time(),
                  "account.problems"=user.problems,"sociogram"= matrix(NA,0,0),  
                  "archive"=get_timeline("25073877")[0,]))
    }
  }
  
  #then, get user's mentions and store the results
  print("Obtaining user's mentions")
  mentionspre<- recip_mentioners(user, verbose=T, sleepy=sleepy,...)
  mentions<- mentionspre$mentions
  archive<- mentionspre$archive
  
  #if there are no contacts, return appropriate values
  if(nrow(mentions)==0){
    return(list("user"=user, "ncontacts"=0, "funcstart"=funcstart, "funcstop"=Sys.time(),
                "account.problems"=user.problems,"sociogram"= matrix(NA,0,0),"archive"=archive))
  }
  
  #Convert mentions to a sociogram
  sociogram<- makeSociogram(mentions)
  
  #if there are no contacts, return appropriate values
  if(nrow(sociogram)==1){
    return(list("user"=user, "ncontacts"=0, "funcstart"=funcstart, "funcstop"=Sys.time(),
                "account.problems"=user.problems,"sociogram"= matrix(NA,0,0),"archive"=archive))
  }
  
  #This loop goes through each new contact and adds a column to the sociogram for each
  #it continues to update the archive throughout
  contacts<- rownames(sociogram)[-1]
  if(verbose){print(c("beginning contacts loop, ncontacts=", length(contacts)))}
  for (i in 1:length(contacts)){
    if(verbose){print(c("contact:", i, "User ID:",contacts[i]))}
    #get the contact's mentions, limiting to the original user's contacts, and using the archive
    contact.mentionspre<- recip_mentioners(contacts[i], preset.contacts=rownames(sociogram),
                                          archive=archive,trim.arch = F, sleepy=F)
    archive<- contact.mentionspre$archive
    contact.mentions<- contact.mentionspre$mentions
    newcol<- numeric(nrow(sociogram))
    
    #go through the contact's mentions and update the new column accordingly
    if(nrow(contact.mentions)>0){
      for (j in 1: nrow(contact.mentions)){
        loc<-which(rownames(sociogram)==contact.mentions$contact[j])
        if(length(loc)>0){
          newcol[loc]<- contact.mentions$min[j]
        }
      }
    }
    sociogram<-cbind(sociogram, newcol)
    }
  
  for (k in 1:nrow(sociogram)){
   sociogram[k,k]<-NA
  }
  colnames(sociogram)<- rownames(sociogram)
  sociogram[1,]<- sociogram[,1]
  
  #return output, either writing the archive to a csv or saving it in the list, depending on the call
  if(archive.to.csv){
    write.csv(reformat_tweets(archive), paste(users[i], "recip_mentioners.archive.csv"))
    return(list("user"=user, "ncontacts"=  nrow(sociogram)-1, "funcstart"=funcstart, "funcstop"=Sys.time(),
                "account.problems"=user.problems, "sociogram"=sociogram, "archive"= "Written to CSV"))
  }
  else{
  return(list("user"=user, "ncontacts"=  nrow(sociogram)-1, "funcstart"=funcstart, "funcstop"=Sys.time(),
              "account.problems"=user.problems, "sociogram"=sociogram, "archive"=archive))
  }
  }







