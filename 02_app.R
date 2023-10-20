#? Created on 2023-10-16 17:54:50.33935 by Jyri Lillemets with R version 4.3.1 (2023-06-16).
#? This script ...

# Loe laiendused
library('magrittr')
library('dplyr')
library('sf')
library('leaflet')
library('shiny')
library('RColorBrewer')

# Loe andmed
Põllud <- readRDS('põllud.Rds')
Ehitised <- readRDS('ehitised.Rds')
Ettev <- readRDS('ettevõtted.Rds')
Liigid <- Ehitised$loomaliigid %>% unique %>% strsplit(';') %>% unlist %>% unique %>% na.omit %>% .[1:10]
Kultuurid <- unique(Põllud$kultuur)[1:10]
Tegevused <- unique(Ettev$tegevusala)

# Interface
ui <- bootstrapPage(
  tags$style(type = 'text/css', 'html, body {width:100%;height:100%}'),
  leafletOutput('kaart', width = '100%', height = '100%'),
  absolutePanel(top = 10, right = 10,
                radioButtons(inputId = 'maak', 
                             label = "Maakond", 
                             choices = unique(Põllud$maakond), 
                             selected = 'Saare maakond'), 
                checkboxGroupInput(inputId = 'põlluk', 
                                   label = "Põllukultuur", 
                                   choices = Kultuurid, 
                                   selected = NA),
                checkboxGroupInput(inputId = 'ehit', 
                                   label = "Kasvatatavad loomad", 
                                   choices = Liigid, 
                                   selected = NA),
                checkboxGroupInput(inputId = 'tegevusala', 
                                   label = "Ettevõtted", 
                                   choices = Tegevused, 
                                   selected = NA),
                checkboxInput('legend', "Kuva värvide tähendused", FALSE)
  )
)

# Server
server <- function(input, output) {
  
  # Aluskaart
  output$kaart <- renderLeaflet({
    P <- st_bbox(Põllud) %>% unname
    leaflet(Põllud) %>%
      addProviderTiles('CartoDB.Positron') %>% 
      fitBounds(P[1], P[2], P[3], P[4])
  })
  
  # Valikud
  lisaPõllud <- reactive({
    Põllud %>%
      filter(maakond %in% input$maak) %>% 
      filter(kultuur %in% input$põlluk)
  })
  lisaEhitised <- reactive({
    Ehitised %>%
      filter(maakond %in% input$maak) %>% 
      filter(grepl(paste(input$ehit, collapse = '|'), loomaliigid, ignore.case = T))
  })
  lisaEttevõtted <- reactive({
    Ettev %>%
      filter(maakond %in% input$maak) %>% 
      filter(tegevusala %in% input$tegevusala)
  })
  
  # Värvid
  Värvi <- colorFactor(brewer.pal(length(unique(Põllud$kultuur)), 'Set1'), 
                       Põllud$kultuur)
  
  # Kuvatav
  observe({
    leafletProxy('kaart', data = lisaPõllud()) %>% clearShapes() %>%
      addCircles(stroke = F, fillOpacity = .8, fillColor = ~Värvi(kultuur), 
                 radius = ~sqrt(pindala*1e4/pi))
  })
  observe({
    leafletProxy('kaart', data = lisaEhitised()) %>% clearMarkers()
    if (!is.null(input$ehit)) {
      leafletProxy('kaart', data = lisaEhitised()) %>% 
        addCircleMarkers(stroke = F, fillOpacity = .8, fillColor = 'red', 
                         radius = 2)
    }
  })
  observe({
    leafletProxy('kaart', data = lisaEttevõtted()) %>% clearMarkers() %>%
      addCircleMarkers(stroke = F, fillOpacity = .8, fillColor = 'blue', 
                 radius = 2)
  })
  
  # Selgitused
  observe({
    proxy <- leafletProxy('kaart', data = Põllud)
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% addLegend(position = 'bottomleft', title = "kultuur", 
                          pal = Värvi, values = ~kultuur)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
