


generadorMatricesTags<-function(vUser){
  
  #---1.	Recogida de hasta un límite impuesto por la API de 3200 tweets de un usuario.
  
  usuario<-vUser #e.j: @jordievole
  
  #Obtenemos la información del usuario
  tw <- userTimeline(vUser, n = 3200,includeRts=TRUE)
  
  #Pasamos la información a un dataframe para poder trabajar
  tw <- twListToDF(tw)
  
  #Obtnemos los tweets del usuario almacenados en un vector
  tweets <- tw$text
  
  #Creamos el dataframe con los tweets del primer usuario
  df.tweetsUser1<-data.frame(tweets)
  
  #Añadimos una columna con el nombre del usuario
  df.tweetsUser1$user<-usuario
  
  #---2.	Extracción de menciones de los tweets
  
  #Obtenemos las menciones del usuario llamando a la funcion extract.mentions(vec)
  mentions<-extractMentions(tweets)
  
  #Reordenamos los indices
  rownames(mentions) <- 1:nrow(mentions)
  
  #Acotamos a 20 (parametrizable) mencionespara que no sea demasiado grande la lista
  mentions<-mentions[1:20,]
  
  #Recogemos las menciones con una freq.>10
  mentions<-mentions[mentions$freq > 10,]
  mentions<-data.frame(users=mentions[,1])
  
  #Obtenemos la lista de tags en los tweets llamando a la función extracTags
  tags<-extracTags(tweets)
  
  #Nos quedamos únicamente con aquellos tags que tienen una freq. >10 (parametrizable)
  tags<-tags[tags$freq > 10,]
  
  #Reordenamos indices
  rownames(tags) <- 1:nrow(tags)
  
  #----------Paso 3 Creamos un dataframe para trabajar -------------->
  
  #definimos un dataframe copiando hases
  tagsUsuario1 <-tags
  
  #Añadimos una columna con las calificaciones de los tags
  tagsUsuario1 <-addNotas(tagsUsuario1)
  
  
  #añadimos una columna usuario con el nombre del usuario objeto (parametrizable)
  tagsUsuario1$usuario <- usuario
  
  #Añadimos la columna tick inicializándola a 1
  tagsUsuario1$tick <- 1
  
  #<--------------Paso 5.1 Intentamos meter algo más de 20 usuarios-------->
  
  #Datos del usuario
  user<-getUser(vUser)
  
  #Obtenemos los followees
  friends<-user$getFriends()
  
  #Pasamos a un dataframe la información
  friends<-twListToDF(friends)
  
  #Ordenamos por popularidad
  friends<-friends[order(friends$followersCount, decreasing=TRUE),]
  
  #Escogemos los 100 más populares
  friends<-data.frame(friends[1:100,c("screenName")])
  
  #nombramos la columna para que sea compatible con las dataframe mentions
  colnames(friends)<-"users"
  
  #Añadimos @ a los nombres de los users
  friends["users"]<-sapply(friends["users"], function(x) paste0("@",friends$screenName))
  
  #Añadimos este dataframe a mentions
  finalUsers <- rbind(mentions,friends)
  
  print ("Hemos creado la lista mentions + friends")
  
  #Eliminamos duplicidades
  finalUsers<-unique(finalUsers)
  
  #Reordenamos los indices
  rownames(finalUsers) <- 1:nrow(finalUsers)
  
  #----------Paso 4 Creamos un finalFrame con los tags del usuario + los de las menciones -------------->
  
  print ("Comienza la ejecución de la funcion: tabla.usuariostags()")
  
  #Por cada usuario recogemos sus 20 tags más frecuentes y sus tweets
  #Devuelve lista con el dataframe y un vector de tweets
  listado <- tabla.usuariostags(finalUsers,tagsUsuario1, df.tweetsUser1 )
  
  print ("Terminada la ejecución de la función: tabla.usuariostags()")
  
  #Recogemos el dataframe tags~users
  tagsUsuarios<-listado[[1]]
  
  
  #Recogemos el dataframe tweets~users
  storageTweets<-listado[[2]]
  
  #DEBEMOS VER COMO GUARDAR EN MONGOdb
  
  #<--------------Paso 5 Limpiamos el dataframe tagsUsuarios------------------->
  
  print ("Limpiamos el dataframe tagsUsuarios")
  
  #Limpiamos el dataframe de valores NA
  tagsUsuarios<-na.omit(tagsUsuarios)
  
  #Limpiamos el dataframe de valores que no empiecen por #, con la funcion limpiarNoHash(dataframe)
  tagsUsuarios<-limpiarNoHash(tagsUsuarios)
  
  #Reorganizamos los indices del dataframe tagsUsuarios
  rownames(tagsUsuarios) <- 1:nrow(tagsUsuarios)
  
  #<--------------Paso 6 Creamos la matrizFinal, que será usada en el filtrado colaborativo.-------->
  
  #Eliminamos duplicidades
  tagsUsuarios<-unique(tagsUsuarios)
  
  print ("Invocamos acast() para generar la matriz real")
  
  #Creamos la matriz  con calificaciones
  matrizReal <- acast(tagsUsuarios, usuario~tag, mean, value.var="nota",fill=0)
  
  print ("Invocamos acast() para generar la matriz binaria")
  
  #Creamos la matriz  binaria (1-0)
  matrizBinaria<-acast(tagsUsuarios, usuario~tag, length,  value.var="tick")
  
  #Guardamos los nombres de los usuarios de la matriz real
  usuariosReal<-rownames(matrizReal)
   
  #Guardamos los nombres de los usuarios de la matriz binaria
  usuariosBinaria<-rownames(matrizBinaria)
  
  #A partir de la segunda columna guardamos una nueva matriz real con valores numéricos
  matrizReal<-apply(matrizReal[,1:ncol(matrizReal)], 2, as.numeric)
  
  #A partir de la segunda columna guardamos una nueva matriz binaria con valores numéricos
  matrizBinaria<-apply(matrizBinaria[,1:ncol(matrizBinaria)], 2, as.numeric)
  
  #Incorporamos los usuarios antes guardados, como los nombres de las filas
  rownames(matrizReal)<-usuariosReal
  rownames(matrizBinaria)<-usuariosBinaria
  
  print ("Generamos rb - binaryRatingMatrix")
  
  #Generamos la matriz binaria de trabajo
  rb = as(matrizBinaria,"binaryRatingMatrix")
  
  print ("Generamos r - realRatingMatriz")
  
  #Generamos la matriz real de trabajo
  matrizReal[matrizReal=="0"]<-NA
  r = as(matrizReal,"realRatingMatrix")
  
  #Creamos una lista con las matrices generadas
  lista<-list(rb,r,tagsUsuarios,storageTweets)
  
  print ("Fin de la llamada")
  
  #Devolvemos la lista a la llamada principal
  return(lista)
  
}

addNotas<-function(frame){
  #Añadimos una columna de notas
  maximo<-max(frame$freq)
  nota<-apply(frame, 1, function(x) round(as.numeric(x[2])*10/maximo,2))
  frame$nota<-nota
  return(frame)
}

generadorMatricesUsers<-function(vUser){
  
  #----Paso 1 - Obtenemos los datos del usuario
  
  user<-getUser(vUser)
  
  print (paste0("Obtenemos los followees de ",vUser))
  
  #Los 100 seguidos de mayor popularidad
  friends<-user$getFriends()
  friends<-twListToDF(friends)
  friends<-friends[order(friends$followersCount, decreasing=TRUE),]
  friends<-friends[1:100,c("screenName","followersCount")]
  
  #Reordenamos los indices
  rownames(friends) <- 1:nrow(friends)
  
  #Añadimos las culumnas con el vUser y un control para generar la matriz final.
  friends$usuario<-vUser
  friends$tick<-1
  
  #Nombremos las columnas del dataframe generado
  colnames(friends)<-c("followee","followersCount","follower","tick")
  
  #---------Paso 2, por cada uno de los amigos debemos encontrar sus seguidores
  
  print (paste0("Obtenemos los 10-Top followers de los followees de ",vUser))
  
  #Obtenemos los 'seguidores de los seguidos'
  frameSeguidores<-followersGetFrame(friends,vUser)
  
  print ("Completamos buscando los 100-Top followees de cada uno de los followers de la tabla anterior")
  
  #Para cada seguidor del dataframe buscamos los más seguidos
  FrameCompleto<-FolloweeGetFrame(frameSeguidores,vUser)
  
  print (paste0("Guardamos el dataframe generado en FrameRecoUsers_",vUser,"RDa")) 

  #Guardamos la información generada
  saveRDS(FrameCompleto,file=paste0("FrameRecoUsers_",vUser,".RDa"))
  
  print("Generamos la matriz binaria")

  #Creamos la matriz  binaria
  matrizFollow<-acast(FrameCompleto, follower~followee, length, value.var="tick", fill = 0)
  
  #Guardamos los nombres de los usuarios
  usuarios<-rownames(matrizFollow)
  
  #A partir de la segunda columna guardamos una nueva matriz con valores numéricos
  matrizFollow<-apply(matrizFollow[,1:ncol(matrizFollow)], 2, as.numeric)
  
  #Incorporamos los usuarios antes gurdados como los nombres de fila
  rownames(matrizFollow)<-usuarios
  
  print ("Finalmente generamos la matriz binaryRatingMatrix")
  
  #Se genera la matriz binaryRatingMatrix que se usa como fuente base de información
  rb_usu = as(matrizFollow,"binaryRatingMatrix")
  
  lista<-list(rb_usu,FrameCompleto)
  
  #Devolvemos la matriz generada
  return(lista)
  
}