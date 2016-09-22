#Cargamos la lista de usuarios de evaluación
listaUsuariosEval<-readRDS("lst.usuariosEval.RDa")
#saveRDS(listaUsuariosEval[1:2],file="lst.usuariosEval.RDa")

shinyUI(fluidPage(

  #Añadimos estilos CSS a algunos cuadros tipo textOutput
  tags$style(type='text/css', '#textoError {background-color: white; color: red;}'),
  tags$style(type='text/css', '#textoExito {background-color: white; color: green;}'),
  tags$style(type='text/css', '#textoEtiq1 {background-color: rgb(238,238,238); color: black; text-align: center;font: 18px arial, sans-serif;}'),
  tags$style(type='text/css', '#textoEtiq2 {background-color: rgb(238,238,238); color: black; text-align: center;font: 18px arial, sans-serif;}'),
  tags$style(type='text/css', '#textoEtiq3 {background-color: rgb(238,238,238); color: black; text-align: center;font: 18px arial, sans-serif;}'),
  tags$style(type='text/css', '#textoEtiq4 {background-color: rgb(238,238,238); color: black; text-align: center;font: 18px arial, sans-serif;}'),
  tags$style(type='text/css', '#textoEtiq5 {background-color: rgb(238,238,238); color: black; text-align: center;font: 18px arial, sans-serif;}'),
  tags$style(type='text/css', '#textoEtiq6 {background-color: rgb(238,238,238); color: black; text-align: center;font: 18px arial, sans-serif;}'),
  
  #tags$style(type='text/css', '#text {background-color: rgba(255,255,0,0.40); color: red;}'),
  titlePanel("Twitter - Sistema de Recomendaciones"),
  navbarPage(title = "TFM - Big Data and Visual Analytics "),
  
  #Usamos javascript mediante el package shinyjs para refrescar la app
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
  navbarMenu(  
    actionButton("stop", label="Quit", icon = icon("circle-o-notch")),
    actionButton("refresh", label="Refresh", icon = icon("fa fa-refresh"))
  ), 
  
  
  fluidRow(
    column(3, wellPanel(
      strong("Usuario a analizar",style = "font-family: 'times'; font-size:24px"),
      br(),
      br(),
      textInput("usuarioAnalisis", "Usuario", "@salvadostv"),
      selectInput("analisisType", label="Tipo de recomendación", 
                  choices=list("Hashtags"="Hashtags","Usuarios"="Usuarios"),selected="Hashtags"),
      selectInput("mongodb", label="Almacenar dataframe en Mongodb", 
                  choices=list("SI"="SI","NO"="NO"),selected="NO"),
      br(),
      actionButton("Action1","Generar Datos"),
      br()
      
    ),wellPanel(
      strong("Recomendaciones y Evaluación",style = "font-family: 'times'; font-size:24px"),
      br(),
      br(),
      selectInput("usuario", label="Usuario de estudio", 
                  choices=listaUsuariosEval, selected=1),
      selectInput("recotipo", label="Tipo de recomendación",
                  choices=list("Hashtags"="Hashtags","Usuarios"="Usuarios","Ambos"="Ambos"),selected="Hasthtags"),
      selectInput("tipoMatriz", label="Tipo de matriz", 
                  choices=list("Binaria"="Binaria","Real (Hashtags)"="Real"),selected="Binaria"),
      selectInput("algoritmo", label="Algoritmo", 
                  choices=list("POPULAR"="POPULAR","UBCF"="UBCF","IBCF"="IBCF","TODOS (Evaluación)"="TODOS"),selected="POPULAR"),
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
           textOutput("textoEtiq1"),
           br(),
           uiOutput("tabla1", width = 400, height = 800),
           br(),
           textOutput("textoEtiq3"),
           plotOutput("plot3", width = 390, height = 400),
           plotOutput("plot4", width = 390, height = 400),
           verbatimTextOutput("textoSalida")
           
    ),
    column(3, 
           textOutput("textoEtiq2"),
           br(),
           uiOutput("tabla2", width = 400, height = 800),
           br(),
           textOutput("textoEtiq4"),
           plotOutput("plot5", width = 390, height = 400),
           plotOutput("plot6", width = 390, height = 400)
           
           
    ),
    column(3, 
           textOutput("textoError"),
           textOutput("textoExito"),
           textOutput("textoEtiq5"),
           plotOutput("plot1", width = 400, height = 400),
           textOutput("textoEtiq6"),
           plotOutput("plot2", width = 400, height = 400)
           
    )
    
  )
  
)
)