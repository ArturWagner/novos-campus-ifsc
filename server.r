library(shiny)
library(shinydashboard)
library(leaflet)
library(maptools)
require(tbart)
sc <-
  readOGR("42MUE250GC_SIR.shp",
          "42MUE250GC_SIR",
          verbose = FALSE)
dados3 <-
  read.csv2("dados3.csv")

shinyServer(function(input, output) {
  greenLeafIcon <- makeIcon(
    iconUrl = "logo.png",
    iconWidth = 38,
    iconHeight = 95,
    iconAnchorX = 22,
    iconAnchorY = 94
  )
  palette1 <- colorBin(
    c('#e6ffee',
      '#b3ffcc',
      '#80ffaa',
      '#4dff88',
      '#1aff66',
      '#00b33c'
    ),
    bins = c(0, 10000, 20000, 50000, 100000, 600000)
  )
  
  popup1 <-
    paste0(
      "<br>Campus: ",
      dados3$CAMPUS,
      "<br>Municipio: ",
      dados3$Municipio,
      "<br>População: ",
      dados3$Populacao
    )
  mapa <-
    readShapeSpatial("42MUE250GC_SIR.shp")
  dados <- merge(dados3, mapa, by = "CD_GEOCMU")
  peso <- dados$Populacao
  dados2 <- subset(dados, CAMPUS != "NA", select = c(x, y))
  pexist <- as.integer(rownames(dados2))
  output$mapa <- renderLeaflet(
    leaflet() %>%
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      setView(-48.592424,-27.596631, zoom = 10) %>%
      addPolygons(
        data = sc,
        fillColor = ~palette1(peso),
        fillOpacity = 0.8,
        color = "white",
        weight = 2.0,
        popup = popup1
      ) %>%
      addMarkers(lng = dados2$x,
                 lat = dados2$y)
  )
  observeEvent(input$go, {
    palette1 <- colorBin(
      c('#e6ffee',
        '#b3ffcc',
        '#80ffaa',
        '#4dff88',
        '#1aff66',
        '#00b33c'
      ),
      bins = c(0, 10000, 20000, 50000, 100000, 600000)
    )
    palette2 <- colorBin(
      c('#e6ffee',
        '#b3ffcc',
        '#80ffaa',
        '#4dff88',
        '#1aff66',
        '#00b33c'
      ),
      bins = c(0, 4, 8, 12, 16, 20)
    )
    
    popup2 <-
      paste0(
        "<br>Campus: ",
        dados3$CAMPUS,
        "<br>Municipio: ",
        dados3$Municipio,
        "<br>População: ",
        dados3$Populacao
      )
    popup3 <-
      paste0(
        "<br>Campus: ",
        dados3$CAMPUS,
        "<br>Municipio: ",
        dados3$Municipio,
        "<br>Indice de Analfabetismo: ",
        dados3$Analfabetismo
      )
    if (input$peso == "Populacao") {
      peso <- dados$Populacao
      palette <- palette1
      popup5 <- popup2
    } else{
      peso <- dados$Analfabetismo
      palette <- palette2
      popup5 <- popup3
    }
    
    
    p1 <- length(pexist) + input$quant  #<-0-5
    m2 <- cbind(dados$x, dados$y)
    
    # Calculo da distancia com peso
    d <- dist(m2)
    d2 <- as.matrix(d)
    d3 <- d2 * peso #selectinput$peso
    alocado <- allocate(m2,
                        p = p1,
                        force = pexist,
                        metric = d3)
    novos <- setdiff(unique(alocado), pexist)
    
    #Mapa
    output$mapa <- renderLeaflet(
      leaflet() %>%
        addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                 attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
        setView(-48.592424,-27.596631, zoom = 6) %>%
        addPolygons(
          data = sc,
          fillColor = ~ palette(peso),
          fillOpacity = 0.8,
          color = "white",
          weight = 2.0,
          popup = popup5
        ) %>%
        addMarkers(lng = dados2$x,
                   lat = dados2$y) %>%
        addMarkers(
          lng = m2[novos, 1] ,
          lat = m2[novos, 2] ,
          icon = greenLeafIcon
        )
    )
  })
})
