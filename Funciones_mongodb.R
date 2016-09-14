require(mongolite)

mongodbStorageVector<-function(vector,dbase,coleccion,usuario){
 
  df<-data.frame(vector)
  df$user<-usuario

  #iniciamos la conexión con Mongodb
  m <- mongo(collection = coleccion, db=dbase)
  #Insertamos los datos
  m$insert(df)
  print("Insert OK")
}

mongodbStorageDataFrame<-function(data, dbase, coleccion){
  
  #iniciamos la conexión con Mongodb
  m <- mongo(collection = coleccion, db=dbase)
  #Insertamos los datos
  m$insert(data)
  print("Insert OK")
}

mongodbGetData<-function(dbase,coleccion,usuario){

  #iniciamos la conexión a Mongodb
  m <- mongo(collection = coleccion, db=dbase)
  #Insertamos los datos
  #Preparamos la query con la variable usuario
  q <- paste0('{"user" : "',usuario,'"}')
  out <- m$find(query=q)
  #Devolvemos los datos de la consulta en un dataframe
  return(out)
}

#mongodbStorageData(tweet,"twitter","tweets","jordiEvole")

#tweets_jordi<-mongodbGetData("twitter","tweets","jordiEvole")
