#build sociogram based on mentions, dependent on getreciprocators function


recipmentioners_network<- function(user, ...){
  funcstart<- Sys.time()
  #convert to user ID rather than screen name   
  infotokenloc<- sample(3:length(get_tokens()), 1) 
  userid<- lookup_users(user, token=get_tokens()[[infotokenloc]])$user_id
  mentions<- recipmentioners(userid, ...)
  #handle abnormal accounts
  if(anyNA(mentions)){
    return(list("user"=userid, "ncontacts"=  NA, "funcstart"=funcstart, "funcstop"=Sys.time(),
                "error"="None", "sociogram"=as.tibble(matrix(NA,0,0))))
    }
  if(nrow(mentions)==0){
    print("no reciprocators, returning null sociogram")
    return(as.tibble(matrix(NA,0,0)))
  }
  
  
  contacts<- nrow(mentions)+1
  sociogram<<- rbind(c(userid, NA), mentions[,c(1,4)])
  colnames(sociogram)<- c("contact", "strength")
  
  sociogram$contact<<- as.character(sociogram$contact)
  
  #This loop goes through each new contact and adds a column to the sociogram for each
  #print("beginning loop")
  for (i in 2:contacts){
    #get new mentions, 
    print(sociogram$contact[i])
    mentions2<- recipmentioners(sociogram$contact[i], preset.contacts=sociogram$contact,...)
    
    
    newcol<- numeric(contacts)
    
    if(nrow(mentions2)>0){
      for (j in 1: nrow(mentions2)){
        loc<-which(mentions$contact==mentions2$contact[j])
        if(length(loc)>0){
          newcol[loc+1]<- mentions2$min[j]
        }
      }
    }
    sociogram<-cbind(sociogram, newcol)
    
  }
  #convert sociogram to matrix
  oldsociogram<- sociogram
  #rownames(sociogram)<- sociogram[,1]
  sociogram<- as.matrix(sapply(sociogram[, 2:ncol(sociogram)], as.numeric), 
                        dimnames= list(sociogram[,1],sociogram[,1]))
  rownames(sociogram)<- oldsociogram[,1]
  colnames(sociogram)<- rownames(sociogram)
  for (k in 1:nrow(sociogram)){
   sociogram[k,k]<-NA
  }
  sociogram[1, 1:ncol(sociogram)]<- sociogram[,1]
  print("made it here")
  return(list("user"=userid, "ncontacts"=  nrow(sociogram)-1, "funcstart"=funcstart, "funcstop"=Sys.time(),
              "error"="None", "sociogram"=sociogram))
  }


sociogram<-recipmentioners_network("llsigerson", startday=Sys.Date()-60)




