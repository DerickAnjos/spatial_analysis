# Instalando os pacotes necessários

pacotes <- c('tmap', 'tidyverse', 'rgdal', 'ggplot', 'raster', 'plotly', 
             'maptools', 'sf', 'rgeos', 'sp', 'adehabitatHR', 'broom', 'knitr', 
             'kableExtra', 'gridExtra', 'RColorBrewer', 'profvis', 'png', 'grid',
             'magick', 'rgl', 'devtools', 'GISTools')

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Indicação da aula: O pacote rayshader que está no CRAN, no momento, possui 
# alguns bugs. A versão que está no GitHub do autor do pacote já é mais 
# funcional.
devtools::install_github('tylermorganwall/rayshader')

library(rayshader)
