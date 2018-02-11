#' recip_followers_network
#' 
#' Maps out a user's 1.5 level ego network (who are their contacts, and whether these contacts 
#' know each other), based on reciprocated following. Repeatedly uses the recip_followers function to accomplish this.
#' Tie strength is binary: either two users are connected or they are unconnected.
#' 
#' @param user a user screen name or ID
#' @param max.contacts the maximum number of contacts (reciprocated followers) to process. If there are more than this
#' number, than a random sample of the contacts (n=max.contacts) will be used.
#' @param verbose whether the function should provide updates on its progress.
#' 
#' @return user: the user ID of the original user
#' @return ncontacts: how many contacts the user has
#' @return funcstart: start time of function
#' @return funcstop: stop time of function
#' @return account.problems: information about potential problems with the account
#' @return sociogram: a matrix with identical rows and columns showing the connections among the user and their
#' contacts. The user's connection with themselves is marked as "self".
#' @details If one of the user's contacts has a private or large account, the function will check their connections
#' by looking at data from other users in the network whose data can be collected.
#' However, the connections between two accounts that are large or private can't be mapped, so it is marked with an 
#' NA value.

    

   
recip_followers_network<- function(user, max.contacts=500, verbose=F){
    funcstart<- Sys.time()
    
    #convert screen name to user ID (if not already)
    if(is.na(suppressWarnings(as.numeric(user)))){
      user<- lookup_users(user)$user_id
    }
    #Get reciprocating followers of user, allowing searching of more IDs than with 
    #the standard version of recipfollowers
    recipids<- recip_followers(user, max.followers = 500000, max.friends=50000)
    
    #Handle various problematic accounts
    #Accounts where the data can't be collected
    if(class(recipids)[1]=="tbl_df"){
      return(list("user"=user,"ncontacts"=NA, "funcstart"=funcstart, "funcstop"=Sys.time(), 
                    "account.problems"=recipids, "sociogram"=NA))  
      }
    #Account with 0 reciprocated followers
    if(length(recipids)==0){
      sociogram<- matrix(data="Self", dimnames = list(user, user))
      return(list("user"=user,"ncontacts"=0, "funcstart"=funcstart, "funcstop"=Sys.time(), 
                  "account.problems"="None", "sociogram"=sociogram))
    }
    
    #If there are none of these issues, proceed 
    #if the person has more than the number of max reciprocaters, get a random sample of them instead.
    if(length(recipids)>max.contacts){
    recipids<- sample(recipids, max.contacts)
    }
    ncontacts<- length(recipids)
    #build sociogram
    sociogram<- matrix(nrow=length(recipids), ncol=1, data=rep.int("Connected", length(recipids)))
    rownames(sociogram)<- recipids
    sociogram<- rbind(NA,sociogram)
    rownames(sociogram)[1]<- user
    colnames(sociogram)<-user
    
    #Loop to see how many reciprocators reciprocate with each other
    NAaccounts<- numeric(0)
    print(c("Beginning loop, number of contacts:", ncontacts))
    for (i in 2:nrow(sociogram)){
      user2=rownames(sociogram)[i]
      if(verbose){
        print(c("contact number:", i-1))
        print(c("user:",user2))
          }
      
      recipids2<- recip_followers(user2, max.friends = 5000, max.followers=5000)
      newcol= rep.int("Unconnected", ncontacts+1)
      #first, check if it's a problematic account
      if(class(recipids2)[1]=="tbl_df"){
            newcol[1:length(ncontacts)]<-NA
            }
          
          #then, proceed to matching (if there's at least one reciprocated follower)
          else if (length(recipids2)>0){
          for (j in 1:length(recipids2)){
            loc<-which(recipids==recipids2[j])
            if(length(loc)>0){
              newcol[loc+1]<- "Connected"
          }
        }
        }
      sociogram<-cbind(sociogram, newcol)
    }
    
    colnames(sociogram)<- rownames(sociogram)
    sociogram[1,]<- sociogram[,1]
    for (k in 1:nrow(sociogram)){
     sociogram[k,k]<-"Self"
    }
    
    #for the column of each NA account, replace it with the corresponding row
    if(length(NAaccounts>0)){
    for (l in 1:length(NAaccounts)){
      sociogram[,NAaccounts[l]]<-sociogram[NAaccounts[l],]
      
    }
    }
    
   
    return(list("user"=user,"ncontacts"=ncontacts, "funcstart"=funcstart, "funcstop"=Sys.time(), 
                "Account.problems"="None", "sociogram"=sociogram))
    }

    
    
    
