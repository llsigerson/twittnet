    
    
    #Function to build sociomatrix based on reciprocating following (dependent on recipfollowers function)
   
recipfollowers_network<- function(user){
    funcstart<- Sys.time()
    #convert to user ID rather than screen name   
    infotokenloc<- sample(3:length(get_tokens()), 1) 
    userid<- lookup_users(user, token=get_tokens()[[infotokenloc]])$user_id
    
    #Get reciprocating followers of user, allowing searching of more IDs than with 
    #the standard version of recipfollowers
    recipids<- recipfollowers(userid, maxfollowers = 500000, maxfriends=50000)
    
    #If the person either has a large account or a private account, or a mistaken ID, return appropriatecustomized values
    if(anyNA(recipids)){
      if(userid%in%privateaccounts){
        return(list("user"=userid,"ncontacts"=NA, "funcstart"=funcstart, "funcstop"=Sys.time(), 
                    "error"="Private Account", "sociogram"=NA))  
      }
      else if(userid%in%largeaccounts){
        return(list("user"=userid,"ncontacts"=NA, "funcstart"=funcstart, "funcstop"=Sys.time(), 
                    "error"="Large Account", "sociogram"=NA))  
      }
     #not currently in use
      # else{
      #  return(list("user"=userid,"ncontacts"=NA, "funcstart"=funcstart, "funcstop"=Sys.time(), 
       #             "error"="Mistaken ID", "sociogram"=NA)) 
      #}
    }
    #if the person has 0 reciprocated followers, return customized values
    if(length(recipids)==0){
      
      sociogram<- matrix(data="Self")
      rownames(sociogram)<- userid
      colnames(sociogram)<- userid
      return(list("user"=userid,"ncontacts"=0, "funcstart"=funcstart, "funcstop"=Sys.time(), 
                  "error"="None", "sociogram"=sociogram))
    }
    ncontacts<- length(recipids)
    #if the person has more than 500 reciprocaters, get a random sample of 500 of them.
    if(length(recipids)>500){
    recipids<- sample(recipids, 500)
    }
    
    #build sociogram
    sociogram<- matrix(nrow=length(recipids), ncol=1, data=rep.int("Connected", length(recipids)))
    rownames(sociogram)<- recipids
    sociogram<- rbind(NA,sociogram)
    rownames(sociogram)[1]<- userid
    colnames(sociogram)<-userid
    contacts<- nrow(sociogram)
    
    
    #Loop to see how many reciprocators reciprocate with each other
    NAaccounts<- numeric(0)
    print(c("Beginning loop, number of contacts:", contacts))
    for (i in 2:nrow(sociogram)){
      user2=rownames(sociogram)[i]
      #cat(c("\ncontact:",user2))
      #print(c("contact number:", i-1))
      
      recipids2<- recipfollowers(user2)
      
      newcol= rep.int("Unconnected", contacts)
        
        
        if(length(recipids2)>0){
          #first, check if it's a private or large account (indicated by an NA)
          if(anyNA(recipids2)){
            newcol[1:contacts]<-NA
            NAaccounts<- append(NAaccounts,i)
            
          }
          
          #then, proceed to matching
          else{
          for (j in 1:length(recipids2)){
            loc<-which(recipids==recipids2[j])
            if(length(loc)>0){
              newcol[loc+1]<- "Connected"
          }
        }
        }
        }
      sociogram<-cbind(sociogram, newcol)
    }
    
    #print(dim(sociogram))  
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
    
   
    return(list("user"=userid,"ncontacts"=ncontacts, "funcstart"=funcstart, "funcstop"=Sys.time(), 
                "error"="None", "sociogram"=sociogram))
    }

    
    
    
