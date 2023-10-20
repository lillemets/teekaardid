#? Created on 2023-10-19 09:02:02.387989 by Jyri Lillemets with R version 4.3.1 (2023-06-16).
#? This script ...

# Load packages
library('magrittr')
library('dplyr')
library('sf')


# Sisesta ja korrasta (2023-10-18 11:27:19.011679) ----------

## Sisesta
Ettev <- read.csv2('andmed/batch001.csv', dec = '.', na.strings = '', colClasses = c(tegevusala_kood = 'character'))

## Korrasta
### Teisenda kroonid
Ettev[which(Ettev$valuuta == 'EEK'), c('kaive', 'kasum')] %<>%  `/`(15.6466)
Ettev[which(Ettev$kapitali_valuuta == 'EEK'), 'kapital'] %<>%  `/`(15.6466)
### Jäta ainult tegutsevad ettevõtted
Ettev %<>% filter(ettevotte_olek == 'Registrisse kantud')
### Korrasta kuupäev
Ettev$esmakande_kpv %<>% as.Date('%d.%m.%Y') %>% as.character
### Vali ja korrasta tunnused
Ettev %<>% select(
  ärikood = ettevotte_registrikood, 
  ärinimi = ettevotte_nimetus, 
  esmakanne = esmakande_kpv, 
  emtak = tegevusala_kood, 
  põhitegevusala = pohitegevusala, 
  vorm = oiguslik_vorm, 
  haldusüksus = haldusyksus, 
  aadress, 
  postiindeks, 
  kapital, 
  käive = kaive, 
  kasum, 
  töötajad = tootajate_arv, 
  maj_algus, 
  maj_lõpp = maj_lopp, 
)
Ettev %<>% mutate(põhitegevusala = põhitegevusala == 'Y')
### Eemalda kordused
Ettev <- Ettev[!duplicated(Ettev), ]

## Määra tegevusala
Emtak <- read.csv('emtak.csv', colClasses = c(emtak = 'character'))
Ettev$tegevusala <- Emtak$tegevusala[match(as.numeric(substr(Ettev$emtak, 1, 3)), as.numeric(Emtak$emtak))]

## Jäta vaid asjakohased tegevusalad
Ettev %<>% filter(substr(emtak, 1, 2) %in% c('02', '03'))


# Lisa koordinaadid (2023-10-18 11:46:43.60381) ----------

## Määra aadress
kuiOn <- function(x) ifelse(!is.na(x), paste(',', x), '')
Ettev$koguaadress <- paste0('Estonia', 
                            kuiOn(Ettev$haldusüksus),  
                            kuiOn(Ettev$postiindeks), 
                            kuiOn(Ettev$aadress)) %>% 
  sub('tn', '', .) %>% gsub('\\s+', ' ', .)

## Tekita kordumatute aadressite loend
Aadressid <- unique(Ettev$koguaadress)

## Find coordinates
library('ggmap')
register_google(readLines('~/documents/mapsapi'))
#! Kuus on tasuta 40 tuhat päringut ja piiriks seadsin 10 tuhat päevas.
if (file.exists('andmed/koordinaadid.Rds')) {
  Koord <- readRDS('andmed/koordinaadid.Rds') 
} else {
  Koord <- geocode(Aadressid, output = 'more')
  Koord$algaadress <- Aadressid
  Koord %<>% subset(grepl('estonia$', Koord$address))
  saveRDS(Koord, 'andmed/koordinaadid.Rds')
}

## Lisa koordinaadid andmetabelile
Ettev[, c('lon', 'lat')] <- Koord[match(Ettev$koguaadress, Koord$algaadress), 
                                  c('lon', 'lat')]
Ettev %<>% filter(!is.na(Ettev$lat))
Ettev %<>% st_as_sf(coords = c('lon', 'lat'), crs = 4326)


# Lisa maakond (2023-10-19 18:00:25.781304) ----------

## Sisesta maakondade piirid
Maakonnad <- read_sf('~/maps/maakond_shp')
Maakonnad %<>% st_transform(4326)

## Lisa
mknd <- st_intersects(Ettev, Maakonnad)
mknd %<>% lapply(`[`, 1) %>% as.numeric # Arvesse läheb ainult esimene maakond mitmest!
Ettev$maakond <- Maakonnad$MNIMI[mknd]

## Jäta vaid asjakohased maakonnad
misMaakonnad <- c('Saare maakond', 'Pärnu maakond', 'Rapla maakond', 'Järva maakond', 'Lääne-Viru maakond')
Ettev %<>% filter(maakond %in% misMaakonnad)


# Salvesta (2023-10-20 17:17:04.960744) ----------

## Salvesta
saveRDS(Ettev %>% select(ärikood, vorm, emtak, põhitegevusala, tegevusala, haldusüksus, aadress, käive, töötajad), 
        'andmed/ettevõtted.Rds')
