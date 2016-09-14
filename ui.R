cargaListaUsers<-function(vFile){
  lista<-readRDS(file=vFile)
  return(lista)
}

listaUsuariosEval<-cargaListaUsers("lst.usuariosEval.RDa")


shinyUI(fluidPage(
  
  
  titlePanel("Recommender System in Twitter"),
  navbarPage(title="TFM"),
  
  #Usamos javascript mediante el package shinyjs para refrescar la app
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
  navbarMenu(  
              actionButton("stop", label="Quit", icon = icon("circle-o-notch")),
              actionButton("refresh", label="Refresh", icon = icon("fa fa-refresh"))
  ), 
  
  
  fluidRow(
    column(3, wellPanel(
      strong("Usuarios a analizar",style = "font-family: 'times'; font-size:24px"),
      br(),
      br(),
      textInput("usuarioAnalisis", "Usuario", "@salvados"),
      selectInput("analisisType", label="Tipo de recomendación", 
                  choices=list("Hashtags"="Hashtags","Usuarios"="Usuarios"),selected="Hashtags"),
      selectInput("mongodb", label="Almacenar dataframe en Mongodb", 
                  choices=list("SI"="SI","NO"="NO"),selected="NO"),
      br(),
      actionButton("Action1","Generar Datos"),
      br()
     
    ),wellPanel(
      strong("Visualización de usuarios",style = "font-family: 'times'; font-size:24px"),
      br(),
      br(),
      selectInput("usuario", label="Usuario de estudio", 
                 choices=listaUsuariosEval, selected=1),
      #selectInput("usuario", label="Usuario de estudio", 
                 # choices=list("@jordievole"="jordievole","@_anapastor_"="_anapastor_"),selected="jordievole"),
      selectInput("recotipo", label="Tipo de recomendación",
                  choices=list("Hashtags"="Hashtags","Usuarios"="Usuarios","Ambos"="Ambos"),selected="Hasthtags"),
      selectInput("tipoMatriz", label="Tipo de matriz", 
                  choices=list("Binaria"="Binaria","Real (Hashtags)"="Real"),selected="Binaria"),
      selectInput("algoritmo", label="Algoritmo", 
                  choices=list("POPULAR"="POPULAR","UBCF"="UBCF","IBCF"="IBCF","TODOS (Evaluacion)"="TODOS"),selected="POPULAR"),
      sliderInput("n","Número de recomendaciones", value = 10, min = 1, max = 20),
      actionButton("Action2","Recomendaciones"),
      br(),
      br(),
      strong("Evaluación K-Fold",style = "font-family: 'times'; font-size:18px"),
      br(),
      br(),
      sliderInput("k","Valor de K", value = 4, min = 2, max = 10),
      actionButton("Action3","Evaluación de resultados"),
      br()
    ),
    wellPanel(
      p("TFM - Twitter Recommender System"),
      p("Autor: Ivan Arenal")
    )),
    
    column(3, 
      uiOutput("tabla1", width = 400, height = 800),
      plotOutput("plot3", width = 390, height = 500),
      plotOutput("plot4", width = 390, height = 500),
      verbatimTextOutput("textoSalida")
      
    ),
    column(3, 
      uiOutput("tabla2", width = 400, height = 800),
      plotOutput("plot5", width = 390, height = 500),
      plotOutput("plot6", width = 390, height = 500)
      
           
    ),
    column(2, 
      textOutput("text"),
      plotOutput("plot1", width = 400, height = 400),
      plotOutput("plot2", width = 400, height = 400)
     
    )
    
)

)
)