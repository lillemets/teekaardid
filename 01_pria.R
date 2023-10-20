#? Created on 2023-10-16 17:37:06.543603 by Jyri Lillemets with R version 4.3.1 (2023-06-16).
#? This script ...

# Load packages
library('magrittr')
library('dplyr')
library('sf')


# Maakonnad (2023-10-18 11:22:35.024642) ----------

## Sisesta maakondade piirid
Maakonnad <- read_sf('~/maps/maakond_shp')

## Määra maakonnad, mille andmed jätta
misMaakonnad <- c('Saare maakond', 'Pärnu maakond', 'Rapla maakond', 'Järva maakond', 'Lääne-Viru maakond')


# Põllud (2023-10-16 17:37:27.380394) ----------

## Sisesta põllud
Põllud <- read_sf('~/maps/pria_põllud.gpkg')

## Vali tunnused
Põllud %<>% select(
  id = pollu_id, 
  ärikood = taotleja_registrikood, 
  ärinimi = taotleja_nimi, 
  maakasutus = taotletud_maakasutus, 
  kultuur = taotletud_kultuur, 
  pindala = pindala_ha, 
  aasta = taotlusaasta)

## Korrasta kultuurid
### Vaheta PRIA nimetused Eestis kasutatud FADN nimetuste vastu
library('readxl')
Vastavus <- read_excel('~/research/pria/tootmistüüp/tunnused.xlsx', skip = 1)
Põllud$kultuur <- Vastavus$fadnnimi[match(Põllud$kultuur, Vastavus$prianimi)]
### Koonda vahetatud nimetusi
Vasted <- read.table(text = '
Rohumaa;Lühiajaline rohumaa / Temporary grass - ha
Rohumaa;Püsirohumaa, v.a looduslik rohumaa / Pasture and meadow, excluding rough grazing - ha
Kartul;Kartul (sh varajane- ja seemnekartul) / Potatoes (including early potatoes and seed potatoes) - ha
Teravili;Nisu ja speltanisu / Common wheat and spelt - ha
Köögivli ja maasikad;Põllul kasvatatav avamaa köögivili, melonid ja maasikad / Fresh vegetables, melons and strawberries outdoor or under low protective cover: Open field - ha
Kesa;Kesa
Viljad ja marjad;Väikeviljad ja marjad / Berry platnations - ha
Rohumaa;Looduslik rohumaa / Rough grazing - ha
Muud taimed;Puuviljaistandikud / Fruit plantations - ha
Kaunvili;Kaunvili ja valgukutuurid terade (sh seemne ning tera- ja kaunvilja segude) tootmiseks / Dried pulses and protein crops for the production of grain (including seed and mixtures of cereals and pulses) - ha
Teravili;Muu terade saamiseks kasvatatav teravili / Other cereals for the production of grain - ha
Teravili;Rukis / Rye - ha
Teravili;Oder / Barley - ha
Teravili;Raps ja rüps / Rape and turnip rape - ha
Teravili;Kaer ja suvisegavili / Oats and summer cereal mixes - ha
Muud taimed;Ravim-, vürtsi- ja lõhnataimed / Aromatic plants, medicinal and culinary plants - ha
Söödakultuur;Haljasmais / Green maize - ha
Söödakultuur;Sööda- ja muguljuurvili (v.a seeme) / Fodder roots and brassicas (excluding seed) - ha
Õlikultuur;Muud õliseemnekultuurid (k.a õlikanep) / Other oil seed crops (oil hemp) - ha
Muud taimed;Puukoolid / Nurseries - ha
Muud taimed;Kanep (kiukanep) / Fibre hemp - ha
Viljad ja marjad;Viinamarjad / Grapes - ha
Muud taimed;Avamaal või madalal katmikalal kasvatatavad lilled ja ilutaimed (v.a puukoolid) / Flowers and ornamental plants (excluding nurseries): outdoor or under low (not accessible) protective cover - ha
Õlikultuur;Linaseemned (õlilina) / Linseed (oil flax) - ha
Muud taimed;Muud haljalt koristatud taimed / Other plants harvested green - ha
', sep = ';'
)
Põllud$taimeliik <- Vasted[, 1][match(Põllud$kultuur, Vasted[, 2])]

## Vali maakonnad

### Lisa põldudele maakond
mknd <- Põllud %>% st_transform(st_crs(Maakonnad)) %>% st_intersects(Maakonnad)
mknd %<>% lapply(`[`, 1) %>% as.numeric # Arvesse läheb ainult esimene maakond mitmest!
Põllud$maakond <- Maakonnad$MNIMI[mknd]

### Vali asjakohased maakonnad
Põllud %<>% filter(tolower(maakond) %in% tolower(misMaakonnad))

## Sobita kuvamiseks
### Jäta ainult põldude keskpunktid
Põllud %<>% st_make_valid
Põllud %<>% st_centroid
### Teisenda koordinaatsüsteem vormingusse WGS 84
Põllud %<>% st_transform(4326)

## Salvesta
saveRDS(Põllud %>% select(ärikood, kultuur, taimeliik, pindala), 
        'andmed/põllud.Rds')


# Loomakasvatusehitised (2023-10-17 18:00:04.243834) ----------

## Sisesta loomakasvatusehitised
Ehitised <- read_sf('~/maps/pria_ehitised.gpkg')

## Vali tunnused
Ehitised %<>% select(
  id = asuk_id, 
  nr = pohi_nr, 
  ärikood = reg_arikood, 
  kehtiv = staatus_tekst, 
  loomaliigid = loomaliigid, 
  maakond = aadr_maakond_tekst)

## Jäta vaid asjakohane
Ehitised %<>% filter(kehtiv == 'Kehtiv' & !is.na(loomaliigid))
Ehitised %<>% filter(!grepl('MESILANE', loomaliigid))

## Korrasta loomaliigid
#Ehitised$loomaliigid %>% table %>% sort %>% names
### Koonda nimetused
kasOn <- function(pattern) grepl(pattern, Ehitised$loomaliigid, ignore.case = T)
Ehitised$loomaliik <- case_when(
  kasOn('VEIS') ~ 'Veis', 
  kasOn('LAMMAS|KITS') ~ 'Lammas või kits', 
  kasOn('MUNAKANA') ~ 'Munakana', 
  kasOn('HOBUNE') ~ 'Hobune', 
  kasOn('SIGA') ~ 'Siga', 
  kasOn('BROILER') ~ 'Broiler', 
  #kasOn('MESILANE') ~ 'Mesilane', 
  kasOn('VUTT|PART|HANI|LIND|KALKUN|JAANALIND|FAASAN|TUVI|EMU|TEDER') ~ 'Muu lind',
  kasOn('VIKERFORELL|JOEVAHK|ELUSKALA|ANGERJAS|TUUR|SIIG|AHVEN|AAFRIKA_ANGERSAGA|VALGEAMUUR|KARPKALA|ARKTIKA_PAALIA|LINASK|JOEFORELL|VALGEAMUUR|ARKTIKA_PAALIA|KOHA|SAGA|ELUSKALA|HAUG|ANGERJAS|PAKSLAUP|AAFRIKA_ANGERSAGA|SIIG|KOI|VENE_TUUR|MERIFORELL|LOHE') ~ 'Kalad ja vähid',
  .default = 'Muu looma')

## Lisa suurus ja tootmistüüp
Tootjad <- readRDS('~/research/pria/pria_2020.Rds')
Tootjad %<>% filter(!is.na(ärikood))
Ehitised[, c('lü', 'tüüp.kirjeldus')] <- Tootjad[match(Ehitised$ärikood, Tootjad$ärikood), c('lü', 'tüüp.kirjeldus')]

## Korrasta maakondade nimetused
Ehitised$maakond <- Maakonnad$MNIMI[match(tolower(Ehitised$maakond), tolower(Maakonnad$MNIMI))]

### Vali asjakohased maakonnad
Ehitised %<>% filter(tolower(maakond) %in% tolower(misMaakonnad))

## Sobita kuvamiseks
### Teisenda koordinaatsüsteem vormingusse WGS 84
Ehitised %<>% st_transform(4326)

## Salvesta
saveRDS(Ehitised %>% select(ärikood, tüüp.kirjeldus, loomaliigid, loomaliik, lü), 
        'andmed/ehitised.Rds')

