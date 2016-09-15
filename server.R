source("globalTFM.R")

shinyServer(function(input, output, session) {
  
  observeEvent(input$Action1, {
    
    print (input$usuarioAnalisis)

    #Introducimos una barra de progreso
    dat <- data.frame(x = numeric(0), y = numeric(0))

    withProgress(message = 'Procesando', value = 0, {
      # Number of times we'll go through the loop
      n <- 100

      for (i in 1:n) {
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        incProgress(1/n, detail = paste("resultados %", i))

        #Paramos el progreso hasta que devuelva resultados la consulta
        if(i==50){

          if (input$analisisType=="Hashtags"){

              #Mostramos un aviso de los que puede durar el proceso
              output$textoSalida<-renderText({"Tiempo aprox. proceso 6h.."})
              
              
              # try.generadorMatricesTags = function(x)
              # {
              #   y = "Problema de conectividad, revisar consola para ver el detalle"
              #   try_error = tryCatch(generadorMatricesTags(x), error=function(e) e)
              #   if (!inherits(try_error, "error"))
              #     y = generadorMatricesTags(x)
              #   
              #   print(y)
              #   
              #   return(y)
              # }
              # 
              
              #Ejecutamos las instrucciones
              #listaMatrices<-try.generadorMatricesTags(input$usuarioAnalisis)
              listaMatrices<-generadorMatricesTags(input$usuarioAnalisis)
         
              matrizBinariaTags<-listaMatrices[[1]]
              saveRDS(matrizBinariaTags,file=paste0("rb_",input$usuarioAnalisis,".RDa"))

              matrizRealTags<-listaMatrices[[2]]
              saveRDS(matrizRealTags,file=paste0("r_",input$usuarioAnalisis,".RDa"))
              
              print("Se han guardado las matrices binaryRatingMatrix y realRatingMatrix en archivos .RDa")
              
              frameTagsUsuarios<-listaMatrices[[3]]
              saveRDS(frameTagsUsuarios,file=paste0("tagsUsersFrame_",input$usuarioAnalisis,".RDa"))
              
              frameTweets<-listaMatrices[[4]]
              saveRDS(frameTweets,file=paste0("tweetsUsersFrame_",input$usuarioAnalisis,".RDa"))
              
              print("Se han guardado los dataframes user-tweets y user-tags en archivos .RDa")
              
              #Comprobamos si queremos almacenar en MongoDB
              if(input$mongodb=="SI"){
                mongodbStorageDataFrame(frameTagsUsuarios,"twitter",paste0("tagsUsersFrame_",substring(input$usuarioAnalisis,2)))
                mongodbStorageDataFrame(frameTweets,"twitter",paste0("tweetsUsersFrame_",substring(input$usuarioAnalisis,2)))
                }
              
              print("Se han añadido los dataframes user-tweets y user-tags a la BBDD Mongodb, db=twitter")
              
              #Añadimos el usuario a la lista desplegable
              nuevaLista<-addUserToList(input$usuarioAnalisis)
              
              #Print de la nueva lista
              print(nuevaLista)
              
              print("Se añade el usuario a la lista de evaluación")
              
              #Guardamos la nueva lista
              saveRDS(nuevaLista,file="lst.usuariosEval.RDa")
              
              #Añadimos el usuario a la lista "usuario" para recomendaciones
              updateSelectInput(session, "usuario",choices = nuevaLista, selected=nuevaLista[length(nuevaLista)])
              
              print("Fin del análisis de Hashtags")
              
          }else if(input$analisisType=="Usuarios"){

            #Mostramos un aviso de los que puede durar el proceso
            output$textoSalida<-renderText({"Tiempo aprox. proceso 6h.."})

            
              #Ejecutamos las instrucciones
              listaMatrizUsers<-generadorMatricesUsers(input$usuarioAnalisis)
              
              #Extraemos la matriz usuarios~usuarios
               matrizUsers<-listaMatrizUsers[[1]]
              #Guardamos la matriz obtenida
              saveRDS(matrizUsers,file=paste0("rb_follow_",input$usuarioAnalisis,".RDa"))
              
              #Guardamos el dataframe base de análisis
              frameUsersUsuarios<-listaMatrizUsers[[2]]
              saveRDS(frameUsersUsuarios,file=paste0("followeesFrame_",input$usuarioAnalisis,".RDa"))
              
              #Añadimos el usuario a la lista desplegable
              nuevaLista<-addUserToList(input$usuarioAnalisis)
              
              #Print de la nueva lista
              print(nuevaLista)
              
              print("Se añade el usuario para lista de evaluación")
              
              #Guardamos la nueva lista
              saveRDS(nuevaLista,file="lst.usuariosEval.RDa")
              
              #Añadimos el usuario a la lista "usuario" para recomendaciones
              updateSelectInput(session, "usuario",choices = nuevaLista, selected=nuevaLista[length(nuevaLista)])
              

              #Comprobamos si queremos almacenar en MongoDB
              if(input$mongodb=="SI"){
                mongodbStorageDataFrame(frameUsersUsuarios,"twitter",paste0("followeesFrame_",input$usuarioAnalisis))

            }
          }
        }

        # Pausa de progreso
        Sys.sleep(0.02)
      }
    })
    if (input$analisisType=="Hashtags"){
        output$textoEtiq5<-renderText(usuarioMostrar)
        output$plot1<-renderPlot({image(matrizBinariaTags, main = "Dispersión de la Matriz binaria sin afinar")})
        output$textoEtiq6<-renderText(usuarioMostrar)
        output$plot2<-renderPlot({image(matrizRealTags, main = "Dispersión de la Matriz real sin afinar")})
        output$textoSalida<-renderText(paste0("Se añaden las nuevas ..ratingMatrix (Hashtags) de ",input$usuarioAnalisis,"a la BBDD."))
        output$textoExito<-renderText(paste0("Se ha completado la generación de la matriz Hashtags y añadido el usuario ",input$usuarioAnalisis," a la lista. Listo para ejecutar Recomendador"))
        
        }else  if (input$analisisType=="Usuarios"){
        output$textoEtiq5<-renderText(usuarioMostrar)
        output$plot1<-renderPlot({image(matrizUsers, main = "Dispersión de la Matriz binaria sin afinar")})
        output$textoSalida<-renderText(paste0("Se añade las nueva binaryRatingMatrix (Usuarios) de ",input$usuarioAnalisis,"a la BBDD."))
        output$textoExito<-renderText(paste0("Se ha completado la generación de la matriz de Usuarios y añadido el usuario ",input$usuarioAnalisis," a la lista. Listo para ejecutar Recomendador"))
        
        }

  })
  

  
  observeEvent(input$Action2, {

    print (input$algoritmo)
    
    #Guardamos la entrada del usuario para evitar los mensajes reactivos.
    usuarioMostrar<-input$usuario
    
    #Introducimos una barra de progreso
    dat <- data.frame(x = numeric(0), y = numeric(0))
    
    withProgress(message = 'Procesando', value = 0, {
      # Number of times we'll go through the loop
      n <- 100
      
      for (i in 1:n) {
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        incProgress(1/n, detail = paste("resultados %", i))
        
        #Paramos el progreso hasta que devuelva resultados la consulta
        if(i==50){
          #Ejecutamos las instrucciones
          if (input$algoritmo!="TODOS"){ 
            if (input$recotipo=="Hashtags"){
              recoTags<-recomenTagsdb(input$usuario, input$tipoMatriz, input$algoritmo, input$n)
              recomendacionesTags<-recoTags[[1]]
              matrizTags<-recoTags[[2]]
              rep1<-formarTabla(recomendacionesTags,paste0("Hashtags (",input$algoritmo,")"))
              
            } else if(input$recotipo=="Usuarios"){
              recoUsers<-recomenUsersdb(input$usuario, input$algoritmo, input$n)
              recomendacionesUsers<-recoUsers[[1]]
              matrizUsers<-recoUsers[[2]]
              rep2<-formarTabla(recomendacionesUsers,paste0("Usuarios (",input$algoritmo,")"))
              
            }else if(input$recotipo=="Ambos"){
              recoTags<-recomenTagsdb(input$usuario, input$tipoMatriz, input$algoritmo, input$n)
              recomendacionesTags<-recoTags[[1]]
              matrizTags<-recoTags[[2]]
              rep1<-formarTabla(recomendacionesTags,paste0("Hashtags (",input$algoritmo,")"))
              
              recoUsers<-recomenUsersdb(input$usuario, input$algoritmo, input$n)
              recomendacionesUsers<-recoUsers[[1]]
              matrizUsers<-recoUsers[[2]]
              rep2<-formarTabla(recomendacionesUsers,paste0("Usuarios (",input$algoritmo,")"))
              
            }
          }
         
        }
        
        # Pausa de barra de progreso
        Sys.sleep(0.02)
      }
      })
    
    #Presentamos las graficas
    if (input$algoritmo!="TODOS"){
      if (input$recotipo=="Ambos"){
          output$tabla1<-renderTable({as(rep1,"matrix")})
          output$tabla2<-renderTable({as(rep2,"matrix")})
          output$textoEtiq5<-renderText(usuarioMostrar)
          output$plot1<-renderPlot({image(matrizTags, main = "Dispersión de la Matriz(Hashtags) afinada")})
          output$textoEtiq6<-renderText(usuarioMostrar)
          output$plot2<-renderPlot({image(matrizUsers, main = "Dispersión de la Matriz(Usuarios) afinada")})
          output$textoSalida<-renderText("Recomendaciones encontradas.")
          output$textoError<-renderText("")
          output$textoEtiq1<-renderText(usuarioMostrar)
          output$textoEtiq2<-renderText(usuarioMostrar)
      }else if(input$recotipo=="Hashtags"){
          output$tabla1<-renderTable({as(rep1,"matrix")})
          output$textoEtiq5<-renderText(usuarioMostrar)
          output$plot1<-renderPlot({image(matrizTags, main = "Dispersión de la Matriz(Hashtags) afinada")})
          output$textoSalida<-renderText("Recomendaciones encontradas.")
          output$textoError<-renderText("")
          output$textoEtiq1<-renderText(usuarioMostrar)
      }else if(input$recotipo=="Usuarios"){
         output$tabla2<-renderTable({as(rep2,"matrix")}) 
         output$textoEtiq6<-renderText(usuarioMostrar)
         output$plot2<-renderPlot({image(matrizUsers, main = "Dispersión de la Matriz(Usuarios) afinada")})
         output$textoSalida<-renderText("Recomendaciones encontradas.")
         output$textoError<-renderText("")
         output$textoEtiq2<-renderText(usuarioMostrar)
         }
      }else{
         
         output$textoError<-renderText("La opción de algoritmo <TODOS> sólo se usa para evaluación, seleccione otro algoritmo.")
        
      }
    })
    
  observeEvent(input$Action3, {
    print (input$algoritmo)
    
    #Guardamos la entrada del usuario para evitar los mensajes reactivos.
    usuarioMostrar<-input$usuario
    
    #Introducimos una barra de progreso
    dat <- data.frame(x = numeric(0), y = numeric(0))
    
    withProgress(message = 'Procesando', value = 0, {
      # Number of times we'll go through the loop
      n <- 100
      
      for (i in 1:n) {
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        incProgress(1/n, detail = paste("resultados %", i))
        
        
        #Paramos el progreso hasta que devuelva resultados la consulta
        if(i==50){
          #Ejecutamos las instrucciones
          if(input$recotipo=="Hashtags"){
              evaluacionTags<-evalueAlgorithmsTagsdb(input$usuario,input$tipoMatriz, input$k, input$algoritmo)
          }else if(input$recotipo=="Usuarios"){
              evaluacionUsers<-evalueAlgorithmsUsersdb(input$usuario, input$k, input$algoritmo)
          }else if(input$recotipo=="Ambos"){
            evaluacionTags<-evalueAlgorithmsTagsdb(input$usuario,input$tipoMatriz, input$k, input$algoritmo)
            evaluacionUsers<-evalueAlgorithmsUsersdb(input$usuario, input$k, input$algoritmo)
          }
        }
        
        # Pausa de progreso
        Sys.sleep(0.02)
      }
    })
    #Presentamos las graficas
    if(input$recotipo=="Hashtags"){
      output$plot3<-renderPlot({plot(evaluacionTags, annotate = 1, legend = "bottomright")
        title("ROC curve - Hashtags")})
      output$plot4<-renderPlot({plot(evaluacionTags, "prec/rec", ylim = c(0,1), annotate = 1, legend = "topright")
        title("Precision/recall - Hashtags")
      output$textoSalida<-renderText("Gráficas de evaluación mostradas.")})
      output$text<-renderText("")
      output$textoEtiq3<-renderText(usuarioMostrar)
    
    }else if(input$recotipo=="Usuarios"){
      output$plot5<-renderPlot({plot(evaluacionUsers, annotate = 1, legend = "bottomright")
        title("ROC curve - Users")})
      output$plot6<-renderPlot({plot(evaluacionUsers, "prec/rec", ylim = c(0,1), annotate = 1, legend = "topright")
        title("Precision/recall - Users")
        output$textoSalida<-renderText("Gráficas de evaluación mostradas.")})
        output$text<-renderText("")
        output$textoEtiq4<-renderText(usuarioMostrar)
    }else if(input$recotipo=="Ambos"){
      output$plot3<-renderPlot({plot(evaluacionTags, annotate = 1, legend = "bottomright")
        title("ROC curve - Hashtags")})
      output$plot4<-renderPlot({plot(evaluacionTags, "prec/rec", ylim = c(0,1), annotate = 1, legend = "topright")
        title("Precision/recall - Hashtags")})
      output$plot5<-renderPlot({plot(evaluacionUsers, annotate = 1, legend = "bottomright")
        title("ROC curve - Users")})
      output$plot6<-renderPlot({plot(evaluacionUsers, "prec/rec", ylim = c(0,1), annotate = 1, legend = "topright")
        title("Precision/recall - Users")
        output$textoSalida<-renderText("Gráficas de evaluación mostradas.")})
      output$textoError<-renderText("")
      output$textoEtiq3<-renderText(usuarioMostrar)
      output$textoEtiq4<-renderText(usuarioMostrar)
    }
  })
  
  #Manejamos el botón de cierre de aplicación
  observeEvent(input$stop, {
    stopApp()
  })
  
  #Manejamos el botón de refresco de aplicación
  observeEvent(input$refresh, {
   shinyjs::js$refresh()
  })
  
  #Crerramos App Shiny
  })
  


