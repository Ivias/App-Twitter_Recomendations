
#Función de recomendación de hashtags
recomenTagsdb<-function(vUser, vType, vAlgorithm, vN){
  
  if (vType=="Binaria"){
    fileName<-paste0("rb_",vUser,".RDa")
  }else{
    fileName<-paste0("r_",vUser,".RDa")
  }

  if (file.exists(fileName)){
    data<-readRDS(fileName)
    
    #Reducimos la dispersión de la matriz
    data<-afinarMatriz(data,5,2)
    #usuario<-paste0("@",vUser)
    
    pos<-which(rownames(data) ==vUser, arr.ind = T)
    lim<-nrow(data)
    
    #Definimos los distintos sistemas de recomendación
    
    if (vAlgorithm=="POPULAR"){
      recomender <- Recommender(data[c(1:(pos-1),(pos+1):lim)], method = "POPULAR")
    }else if(vAlgorithm=="UBCF"){
      recomender <- Recommender(data[c(1:(pos-1),(pos+1):lim)], method = "UBCF", param = list(nn = 5))
    }else if(vAlgorithm=="IBCF"){
      recomender <- Recommender(data[c(1:(pos-1),(pos+1):lim)], method = "IBCF", param = list(k = 10))
    }
    
    #Predecimos las Top-10 recomendaciones
    recommendations <- predict(recomender, data[pos], n=vN)
    
    
    #Visualización de recomendaciones
    #as(recommendations, "list")
    lista<-list(recommendations,data)
  
  }else{ lista=FALSE}
  
  #Devolvemos las recomendaciones y la matriz afinada
  return(lista)
}


#Función que evalua los resultados de la recomendación de hashtags
evalueAlgorithmsTagsdb<-function(vUser, vType, vK, vAlgorithm){
  if (vType=="Binaria"){
    
    fileName<-paste0("rb_",vUser,".RDa")
    
    #Leemos el fichero de datos
    data<-readRDS(fileName)
    
    #Reducimos la dispersión de la matriz
    rb<-afinarMatriz(data,5,2)
    
    #rb<-readRDS(fileName)
    
    #Creamos una lista con los modelos a evaluar para la matriz binaria.
    if (vAlgorithm=="TODOS"){
      models_to_evaluate <- list(  
        random = list(name = "RANDOM", param=NULL),
        PUPULAR = list(name = "POPULAR", param = NULL),
        IBCF_per = list(name = "IBCF", param = list(method = "pearson", k=10)),
        IBCF_jac = list(name = "IBCF", param = list(method = "jaccard", k=10)),
        UBCF_jac = list(name = "UBCF", param = list(method= "jaccard", nn=5)),
        UBCF_cos = list(name = "UBCF", param = list(method = "cosine", nn=5))
      )
    }else if(vAlgorithm=="IBCF"){
      models_to_evaluate <- list(  
        IBCF_per = list(name = "IBCF", param = list(method = "pearson", k=10)),
        IBCF_jac = list(name = "IBCF", param = list(method = "jaccard", k=10))
      )
    }else if(vAlgorithm=="UBCF"){
      models_to_evaluate <- list(  
        UBCF_jac = list(name = "UBCF", param = list(method= "jaccard", nn=5)),
        UBCF_cos = list(name = "UBCF", param = list(method = "cosine", nn=5))
      )
    }else if(vAlgorithm=="POPULAR"){
      models_to_evaluate <- list(  
        PUPULAR = list(name = "POPULAR", param = NULL)
      )
    }else {
      models_to_evaluate <- list(  
        random = list(name = "RANDOM", param=NULL)
      )
    }
    
    
    #Definimos el esquema de evaluación binario
    eval_sets <- evaluationScheme(data = rb, method = "cross-validation", k = vK, given = 4)
    
    
    
  }else{
    
    fileName<-paste0("r_",vUser,".RDa")
    
    data<-readRDS(fileName)
    
    #Reducimos la dispersión de la matriz
    r<-afinarMatriz(data,5,2)
    
    #Creamos una lista con los modelos a evaluar para la matriz real
    models_to_evaluate <- list(  
      random = list(name = "RANDOM", param=NULL),
      PUPULAR = list(name = "POPULAR", param = NULL),
      IBCF_per = list(name = "IBCF", param = list(method = "pearson", k=10)),
      IBCF_jac = list(name = "IBCF", param = list(method = "jaccard", k=10)),
      IBCF_cos = list(name = "IBCF", param = list(method = "cosine", k=10)),  
      UBCF_jacn = list(name = "UBCF", param = list(method= "jaccard", nn=5)),
      UBCF_cosn = list(name = "UBCF", param = list(method = "cosine", nn=5))
    )
    
    #Definimos el esquema de evaluación real
    eval_sets <- evaluationScheme(data = r, method = "cross-validation", k = vK, given = 4, goodRating=3)
    
  }
  #Número de recomendaciones a emitir
  n_recommendations <- c(1, 3, 5, 10, 15, 20)
  
  #Lista de resultados de las evaluaciones
  list_results <- evaluate(x = eval_sets, method = models_to_evaluate, n = n_recommendations)
  

  #Devolvemos los resultados en una lista
  return(list_results)
}


#Función que evalua los resultados de la recomendación de ussuarios
evalueAlgorithmsUsersdb<-function(vUser, vK, vAlgorithm){
  
  
  fileName<-paste0("rb_follow_",vUser,".RDa")
  data<-readRDS(fileName)
  
  #Reducimos la dispersión de la matriz
  rb<-afinarMatriz(data,20,10)
  
  #rb<-readRDS(fileName)
  
  #Creamos una lista con los modelos a evaluar para la matriz binaria.
  if (vAlgorithm=="TODOS"){
    models_to_evaluate <- list(  
      random = list(name = "RANDOM", param=NULL),
      PUPULAR = list(name = "POPULAR", param = NULL),
      IBCF_jac = list(name = "IBCF", param = list(method = "jaccard")),
      IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),  
      UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),  
      UBCF_jac = list(name = "UBCF", param = list(method = "jaccard"))
    )
    
  }else if(vAlgorithm=="IBCF"){
    models_to_evaluate <- list(  
      IBCF_jac = list(name = "IBCF", param = list(method = "jaccard")),
      IBCF_cos = list(name = "IBCF", param = list(method = "cosine"))  
    )
  }else if(vAlgorithm=="UBCF"){
    models_to_evaluate <- list(  
      UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),  
      UBCF_jac = list(name = "UBCF", param = list(method = "jaccard"))
    )
  }else if(vAlgorithm=="POPULAR"){
    models_to_evaluate <- list(  
      PUPULAR = list(name = "POPULAR", param = NULL)
    )
  }else {
    models_to_evaluate <- list(  
      random = list(name = "RANDOM", param=NULL)
    )
  }
  
  
  #Definimos el esquema de evaluación
  eval_sets <- evaluationScheme(data = rb, method = "cross-validation", k = vK, given = 10)
  
  #Definimos el número de recomendaciones
  n_recommendations <- c(1, 3, 5, 10, 15, 20)
  
  #Lista con los resultados de la evaluación de los algoritmos
  list_results <- evaluate(x = eval_sets, method = models_to_evaluate, n = n_recommendations)
  return(list_results)
}

#Función que forma una tabla para mostrar gráficamente una lista
formarTabla<-function(vList,vType){
  
  columna<-unlist(as(vList,"list"))
  frame<-data.frame(columna)
  #frame$valor<-0
  #for (i in 1:10){
  #frame$valor[i]<-11-i
  #}
  rownames(frame)<-c(1:nrow(frame))
  colnames(frame)<-vType
  return(frame)
}

#Función que emite recomendaciones de usuarios
recomenUsersdb<-function(vUser, vAlgorithm, vN){
  
  
  fileName<-paste0("rb_follow_",vUser,".RDa")
  
  if (file.exists(fileName)){
    
    data<-readRDS(fileName)
    
    #Eliminamos el caracter del usuario para poder trabajar con la matriz
    vUser<-substring(vUser,2)
    
    #Reducimos la dispersión de la matriz
    data<-afinarMatriz(data,20,10)
    
    #Buscamos la posición del usuario en la matriz
    pos<-which(rownames(data) ==vUser, arr.ind = T)
    lim<-nrow(data)
    
    #Definimos los distintos sistemas de recomendación
    
    if (vAlgorithm=="POPULAR"){
      recomender <- Recommender(data[c(1:(pos-1),(pos+1):lim)], method = "POPULAR")
    }else if(vAlgorithm=="UBCF"){
      #recomender <- Recommender(data[c(1:(pos-1),(pos+1):lim)], method = "UBCF", param = list(nn = 5))
      recomender <- Recommender(data[c(1:(pos-1),(pos+1):lim)], method = "UBCF")
    }else if(vAlgorithm=="IBCF"){
      #recomender <- Recommender(data[c(1:(pos-1),(pos+1):lim)], method = "IBCF", param = list(k = 10))
      recomender <- Recommender(data[c(1:(pos-1),(pos+1):lim)], method = "IBCF")
    }
    
    #Predecimos las Top-10 recomendaciones
    recommendations <- predict(recomender, data[pos], n=vN)
    
    
    #Visualización de recomendaciones
    # as(recommendations, "list")
    lista<-list(recommendations,data)
  
  }else{lista=FALSE}
  
  #Devolvemos las recomendaciones
  return(lista)
  
}

#Función que reduce los valores de dispersión de una matriz mediante afinamiento
afinarMatriz<-function(vMatrix,vMinRow,vMinColum){
  #matriz<- vMatrix[rowCounts(vMatrix)>vMinRow,colCounts(vMatrix)>vMinColum]
  
  while (min(rowCounts(vMatrix)) < vMinRow || min(colCounts(vMatrix)) < vMinColum) {
    vMatrix <- vMatrix[rowCounts(vMatrix)>vMinRow,colCounts(vMatrix)>vMinColum]
  }
  return(vMatrix)
}

#Función que añade un usuario analizado a la lista para recomendar y evaluar
addUserToList<-function(vListaIn, vUser){
  listaOut<-c(vListaIn,vUser)
  return(listaOut)
}

