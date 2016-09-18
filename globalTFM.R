library(tm)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(twitteR)
library(httr)
library(plyr)
library(recommenderlab)
library(shiny)
library(ggplot2)
library(reshape2)
library(lattice)
library(shinyjs)
library(V8) #Requerida por shinyjs
library(mongolite)



#Cargamos las librerías de funciones.
source("Funciones_matrizBase.R")
source("Funciones_generales.R")
source("Funciones_mongodb.R")
source("Funciones_followee.R")
source("Funciones_recomEval.R")

#Path de trabajo
setwd("C:/users/HOME/Documents/R/TFM_BigData/")

#Login de la App en Twitter
login()





