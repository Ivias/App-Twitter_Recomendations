
login<-function(){
  
  consumer_key <- "g13r94mxybuWkAQAGoscF21Mh"
  
  consumer_secret <- "SvhjHCoq7C1QTWM0FCb0E3tvu7nT4iQ7BYzKQZ2yhPI1MUo70P"
  
  access_token <- "423527229-FRjmWn2gnfsobgZ4WZ35vNSXr1ddkuXbbOnHUq0U"
  
  access_secret <- "Clrqm1dSE7lYMDaSpgP71cjzhHmQGzmTJzhdAWXnMlCaW"
  
  options(httr_oauth_cache=T)
  
  # keep this order of arguments
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  
}

#A?n no savemos xq esta funcion funciona mejor q la anterior.
extracTags= function(vec){
  
  hash.pattern = "#[[:alpha:]]+"
  have.hash = grep(x = vec, pattern = hash.pattern)
  
  hash.matches = gregexpr(pattern = hash.pattern, text = vec[have.hash])
  extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
  
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  if (length(extracted.hash)>0){
    df = data.frame(table(sapply(unlist(extracted.hash), try.tolower)))
    colnames(df) = c("tag","freq")
    df = df[order(df$freq,decreasing = TRUE),]
    return(df)
  }
}

#Para extraer las menciones
extractMentions = function(vec){
  
  mention.pattern = "@\\w+"
  #hash.pattern = "@[[:alpha:]]+"
  have.mention = grep(x = vec, pattern = mention.pattern)
  mention.matches = gregexpr(pattern = mention.pattern, text = vec[have.mention])
  df = regmatches(x = vec[have.mention], m = mention.matches)
  #Pasamos la lista a un dataframe
  df<-data.frame(table(unlist(df)))
  #A?adimos los nombres a las columnas
  colnames(df) = c("users","freq")
  #Ordenamos por freq.
  df = df[order(df$freq,decreasing = TRUE),]
  #Devolvenos el dataframe con las menciones
  return(df)
}

#Creamos el dataframe users-hashtags base de la información
generarFrameUsuariosTags <- function(mentions, df.tagsUser1, df.tweetsUser1){
  df.tagsUsers<-df.tagsUser1
  df.tweetsUsers<-df.tweetsUser1
  #Comenzamos en 2 pues el usuario1 ya está mapeado

  for(i in 2:nrow(mentions)) { #nrow(mentions)..aprox 120 3.5h
    if(i!=2){Sys.sleep(90)}
    print(paste0(i,",",as.character(mentions[[i,"users"]])))
    tml <- userTimeline(mentions[[i,"users"]], n = 1000,includeRts=TRUE)#antes era 3200
    if( length(tml) > 1 ) {
      tml <- twListToDF(tml)
      tweets <- tml$text
      df.tweets<-data.frame(tweets)
      df.tweets$user<-mentions[[i,"users"]]
      df.tweetsUsers <- rbind(df.tweetsUsers,df.tweets)
      
      #Almacenamos la información de los tweets
      saveRDS(df.tweetsUsers,file=paste0("Tweets_frame_",mentions[[1,"users"]],".RDa"))
      
      #Extraemos los hashtags de los tweets
      frameObtenido <- extracTags(tweets)
      if (!is.null(frameObtenido)){
        frameObtenido$usuario <- as.character(mentions[i,"users"])
        frameObtenido$tick <- 1
        
        
        if (class(frameObtenido)=="data.frame"){
          #Seleccionamos unicamente los primeros 20 hastag
          frameObtenido<-frameObtenido[1:20, ]
          frameObtenido<-addNotas(frameObtenido)
          df.tagsUsers <- rbind(df.tagsUsers,frameObtenido)
          
          #Almacenamos la informaci?n de los Tags
          saveRDS(df.tagsUsers,file=paste0("Tags_frame_",mentions[[1,"users"]],".RDa"))
          
        }
      }
    }
    if(i%%6==0){#loginTwitter()
      Sys.sleep(240) 
    }
    
  }
  lista<-list(df.tagsUsers,df.tweetsUsers)
  return(lista)
}

limpiarNoHash<-function(dataframe){
  #Inicia un vector vac?o
  quita=c()
  for (i in 1:length(dataframe$tag)){
    if ((substring(dataframe$tag[i],1,1)=="#")==TRUE) {
      quita<-c(quita,i)
    }
  }
  
  #Para quitar
  dataframe<-dataframe[quita,]
  return(dataframe)
}



eliminarNoASCII<-function(vector){
  # convert string to vector of words
  dat2 <- unlist(strsplit(storageTweets, split=", "))
  # find indices of words with non-ASCII characters
  dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
  # subset original vector of words to exclude words with non-ASCII char
  dat4 <- dat2[-dat3]
  # convert vector back to a string
  dat5 <- paste(dat4, collapse = ", ")
  return(dat5)
}

addNotas<-function(frame){
  
  #A?adimos una columna de notas
  maximo<-max(frame$freq)
  nota<-apply(frame, 1, function(x) round(as.numeric(x[2])*10/maximo,2))
  frame$nota<-nota
  return(frame)
}