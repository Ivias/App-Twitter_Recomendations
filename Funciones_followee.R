followersGetFrame<-function(frame,user0){
  for (i in 1:nrow(frame)){#nrow(frame)
   if(i!=1){ Sys.sleep(50)}
    nombre<-frame$followee[i]
    usuario<-getUser(nombre)
    print(paste(i,usuario$screenName,sep=","))
    followers<-usuario$getFollowers(n=1000)
    followers<-twListToDF(followers)
    followers<-followers[order(followers$followersCount, decreasing=TRUE),]
    followers<-followers[1:10,c("screenName","followersCount")]
    rownames(followers) <- 1:nrow(followers)
    followers$followee<-usuario$screenName
    followers$tick<-1
    colnames(followers)<-c("follower","followersCount","followee","tick")
    frame<-rbind(frame,followers)
    #Vamos guardando el dataframe
    saveRDS(frame,file=paste0("followersFrame_",user0,".RDa"))
  }
  return(frame)
}





FolloweeGetFrame<-function(frame,user0){
  #Vector de control de nombres
  vector<-c()
  for (i in 101:1100){#de 101 hasta 1100
    if((i-100)%%40==0){Sys.sleep(300)}
    if(i!=101){ Sys.sleep(80)}
      
    nombre<-frame$follower[i]
    #Por si acaso da problemas el usuario al hacer getuser(nombre)
    try.getUser = function(x)
    {
      y = NA
      try_error = tryCatch(getUser(x), error=function(e) e)
      if (!inherits(try_error, "error"))
        y = getUser(x)
      return(y)
    }
    
    usuario<-try.getUser(nombre)
    #Comprobamos que el usuario no es NA tambien
    if (is.na(match(nombre,vector)) && !is.na(usuario)){
      vector<-append(vector,nombre) 
    
      #Comprobamos que no esté protegido y que siga a alguien
      if(usuario$protected==FALSE && usuario$friendsCount > 0){
        followees<-usuario$getFriends(n=1000)
        followees<-twListToDF(followees)
        followees<-followees[order(followees$followersCount, decreasing=TRUE),]
        if (nrow(followees)>100){
          followees<-followees[1:100,c("screenName","followersCount")]
        }else{
          followees<-followees[,c("screenName","followersCount")]
        }
        print(paste(i,usuario$screenName,nrow(followees),sep=","))
        rownames(followees) <- 1:nrow(followees)
        followees$followee<-usuario$screenName
        followees$tick<-1
        colnames(followees)<-c("followee","followersCount","follower","tick")
        frame<-rbind(frame,followees)
        saveRDS(frame,file=paste0("followeesFrame_",user0,".RDa"))
      }
    }
  }
  return(frame)
}



