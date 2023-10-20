#? Created on 2023-10-20 09:49:35.566847 by Jyri Lillemets with R version 4.3.1 (2023-06-16).
#? This script ...

# Loe laiendused
library('magrittr')
library('dplyr')
library('sf')
library('leaflet')
library('leafpop')
library('htmlwidgets')
library('ggsci')

# Loe andmed
Põllud <- readRDS('andmed/põllud.Rds')
Ehitised <- readRDS('andmed/ehitised.Rds')
Ettev <- readRDS('andmed/ettevõtted.Rds')
Maakonnad <- read_sf('~/maps/maakond_shp') %>% st_transform(4326)

# Määra värvid
värvidPõllud <- colorFactor(pal_frontiers()(length(unique(Põllud$taimeliik))),
                            Põllud$taimeliik)
värvidEhitised <- colorFactor(pal_simpsons()(length(unique(Ehitised$loomaliik))),
                              Ehitised$loomaliik)
värvidEttevõtted <- colorFactor(pal_jama()(length(unique(Ettev$tegevusala))),
                                Ettev$tegevusala)

# Tekita kaart
teeKaart <- function() {
  leaflet(options = leafletOptions(
    zoomControl = T, zoomSnap = .1, zoomDelta = .1)) %>% 
    addProviderTiles('CartoDB.Positron') %>% 
    ## Valdkonnad
    ### Rohumaa
    addCircles(data = Põllud %>% filter(taimeliik == 'Rohumaa'), group = 'Rohumaa', 
               fillColor = ~värvidPõllud(taimeliik), 
               radius = ~sqrt(pindala*1e4/pi), 
               popup = popupTable(Põllud %>% filter(taimeliik == 'Rohumaa')),
               stroke = F, fillOpacity = .5) %>% 
    ### Taimekasvatus
    #TODO Lisada toidutööstus
    addCircles(data = Põllud %>% filter(taimeliik != 'Rohumaa'), group = 'Taimekasvatus', 
               fillColor = ~värvidPõllud(taimeliik), 
               radius = ~sqrt(pindala*1e4/pi), 
               popup = popupTable(Põllud %>% filter(taimeliik != 'Rohumaa')),
               stroke = F, fillOpacity = .5) %>% 
    ### Loomakasvatus
    #TODO Lisada toidutööstus
    addCircles(data = Ehitised %>% filter(!(loomaliik %in% 'Kalad ja vähid')), group = 'Looma- ja linnukasvatus', 
               fillColor = ~värvidEhitised(loomaliik), 
               radius = ~ifelse(is.na(lü), 100, lü),
               popup = popupTable(Ehitised %>% filter(!(loomaliik %in% 'Kalad ja vähid'))), 
               stroke = F, fillOpacity = .5) %>% 
    ### Kalapüük
    addCircles(data = Ettev %>% filter(substr(emtak, 1, 3) == '031'), group = 'Kalapüük', 
               fillColor = ~värvidEttevõtted(tegevusala), 
               radius = ~ifelse(is.na(käive), 100, sqrt(käive)),
               popup = popupTable(Ettev %>% filter(substr(emtak, 1, 3) == '031')), 
               stroke = F, fillOpacity = .5) %>% 
    ### Vesiviljelus
    addCircles(data = Ehitised %>% filter(loomaliik %in% 'Kalad ja vähid'), group = 'Vesiviljelus', 
               fillColor = ~värvidEhitised(loomaliik), 
               radius = ~ifelse(is.na(lü), 100, lü),
               popup = popupTable(Ehitised %>% filter(loomaliik %in% 'Kalad ja vähid')), 
               stroke = F, fillOpacity = .5) %>% 
    addCircles(data = Ettev %>% filter(substr(emtak, 1, 3) == '032'), group = 'Vesiviljelus', 
               fillColor = ~värvidEttevõtted(tegevusala), 
               radius = ~ifelse(is.na(käive), 100, sqrt(käive)),
               popup = popupTable(Ettev %>% filter(substr(emtak, 1, 3) == '032')), 
               stroke = F, fillOpacity = .5) %>% 
    ### Metsamajandus
    #TODO Lisada puidutööstus
    addCircles(data = Ettev %>% filter(substr(emtak, 1, 2) == '02'), group = 'Metsamajandus', 
               fillColor = ~värvidEttevõtted(tegevusala), 
               radius = ~ifelse(is.na(käive), 100, sqrt(käive)),
               popup = popupTable(Ettev %>% filter(substr(emtak, 1, 2) == '02')), 
               stroke = F, fillOpacity = .5) %>% 
    ## Maakonnad
    addPolylines(data = Maakonnad, group = 'Maakonnapiirid', 
                 weight = 1, color = 'gray') %>% 
    ## Selgitused
    addLegend(group = 'Ettevõtte tegevusala', title = "Ettevõtte tegevusala", 
              pal = värvidEttevõtted, values = Ettev$tegevusala, 
              opacity = .5, position = 'bottomleft') %>% 
    addLegend(group = 'Loomakasvatusehitised', title = "Loomakasvatusehitised", 
              pal = värvidEhitised, values = Ehitised$loomaliik, 
              opacity = .5, position = 'bottomleft') %>% 
    addLegend(group = 'Põllud', title = "Põllud", 
              pal = värvidPõllud, values = Põllud$taimeliik, 
              opacity = .5, position = 'bottomleft') %>% 
    ## Valikud
    addLayersControl(
      ### Valdkonnad
      baseGroups = c(
        "Rohumaa", "Taimekasvatus", 
        "Looma- ja linnukasvatus", 
        "Kalapüük", "Vesiviljelus", 
        "Metsamajandus"), 
      ### Selgitused
      overlayGroups = c("Põllud", "Loomakasvatusehitised", "Ettevõtte tegevusala", "Maakonnapiirid")) %>% 
    ## Pisipilt nurgas
    addMiniMap(toggleDisplay = T, mapOptions = list(provider = 'CartoDB.Positron'))
}

# Kuva kaart
teeKaart()

# Salvesta
saveWidget(teeKaart(), 'index.html', title = "Varud ja väärtusahelad")
