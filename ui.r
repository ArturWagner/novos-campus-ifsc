#Pacotes
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
dados3 <- read.csv2("dados3.csv")

shinyUI(dashboardPage(
  skin = "green",
  #Titulo,header
  dashboardHeader(title = "Aplicativo para localizar um novo campus IFSC", titleWidth = 500),
  #Barra lateral
  dashboardSidebar(disable = FALSE,sliderInput("quant", "Quantidade de Campus",
                                               min=0, max=5, value=0),
                                   selectInput("peso","Fator de localização",choices =c(names(dados3)[8],names(dados3)[9])),
                                   actionButton("go","Nova Pesquisa",style='margin:15px;') 
                   ),
  #Aplicativo
  dashboardBody(
    box(
      width = 12,
      status = "success",
      leafletOutput("mapa",height = "700px"))
  )
)
)

