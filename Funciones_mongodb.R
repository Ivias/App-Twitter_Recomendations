require(mongolite)

#Función que almacena en MogoDB un vector
mongodbStorageVector<-function(vector,dbase,coleccion,usuario){
 
  df<-data.frame(vector)
  df$user<-usuario

  #iniciamos la conexión con Mongodb
  m <- mongo(collection = coleccion, db=dbase)
  #Insertamos los datos
  m$insert(df)
  print("Insert OK")
}

#Función que almacena en MogoDB un dataframe
mongodbStorageDataFrame<-function(data, dbase, coleccion){
  
  #iniciamos la conexión con Mongodb
  m <- mongo(collection = coleccion, db=dbase)
  #Insertamos los datos
  m$insert(data)
  print("Insert OK")
}

#Función que recupera de MogoDB un dataframe con la consulta
mongodbGetData<-function(dbase,coleccion,usuario){

  #iniciamos la conexión a Mongodb
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
