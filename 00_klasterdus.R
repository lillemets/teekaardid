#? Created on 2023-10-20 12:25:55.16152 by Jyri Lillemets with R version 4.3.1 (2023-06-16).
#? This script ...

# Load packages
library('magrittr')
library('dplyr')
library('sf')

# Sisesta loomakasvatusehitised
Ehitised <- read_sf('~/maps/pria_ehitised.gpkg')

# Loo maatriks
Kordumatud <- Ehitised$loomaliigid %>% unique %>% strsplit(';') %>% unlist %>% unique %>% na.omit
Tabel <- sapply(Kordumatud, function(x) sapply(Ehitised$loomaliigid, function(y) grepl(x, y)))

# Korrasta
Mat <- Tabel + 0
#Mat <- aggregate(Mat, list(rownames(Mat)), sum)
#rownames(Mat) <- Mat[, 1]
#Mat[, 1] <- NULL
#Mat <- Mat[rowSums(Mat) > 1, ]

# Pööra
Mat %<>% t

# Leia ja kuva jaotus
Kaugused <- dist(Mat)
Klastrid <- hclust(Kaugused, method = 'ward.D2')
plot(Klastrid)
