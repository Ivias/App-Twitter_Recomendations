require(mongolite)

mongodbStorageVector<-function(vector,dbase,coleccion,usuario){
 
  df<-data.frame(vector)
  df$user<-usuario

  #iniciamos la conexi?n con Mongodb
  m <- mongo(collection = coleccion, db=dbase)
  #Insertamos los datos
  m$insert(df)
  print("Insert OK")
}

mongodbStorageDataFrame<-function(data, dbase, coleccion){
  
  #iniciamos la conexi?n con Mongodb
  m <- mongo(collection = coleccion, db=dbase)
  #Insertamos los datos
  m$insert(data)
  print("Insert OK")
}

mongodbGetData<-function(dbase,coleccion,usuario){

  #iniciamos la conexi?n a Mongodb
  m <- mongo(collection = coleccion, db=dbase)
  #Insertamos los datos
  #Preparamos la query con la variable usuario
  q <- paste0('{"user" : "',usuario,'"}')
  out <- m$find(query=q)
  
  #Por si acaso no es <user> la clave, sino <usuario>
  if(length(out)==0){
    q <- paste0('{"usuario" : "',usuario,'"}')
    out <- m$find(query=q)
  }
  #Devolvemos los datos de la consulta en un dataframe
  return(out)
}

#mongodbStorageData(tweet,"twitter","tweets","jordiEvole")

#tweets_jordi<-mongodbGetData("twitter","tweets","jordiEvole")
