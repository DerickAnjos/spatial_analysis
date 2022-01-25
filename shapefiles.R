# Instalação e carregamento dos pacotes para o script de Shapefiles

pacotes <- c("rgdal","raster","tmap","maptools","tidyverse","broom","knitr",
             "kableExtra","RColorBrewer")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Carregando o objeto shapefile -----------------------------------------------

# Carregando um objeto do tipo shapefile
shp_sp <- readOGR(dsn = 'shapefile_sp', layer = 'estado_sp')

# Classe do objeto (primeiro passo para todo objeto adquirido para Análise 
# Espacial, pois classes diferentes possuem atributos e tratamentos distintos)
class(shp_sp)
typeof(shp_sp) # o objeto está no S4

# Características básicas do objeto
summary(shp_sp)


# Acessando a base de dados e outros componentes do objeto shp_sp -------------

# Para acessar a base de dados de um shapefile devemos utilizar o '@', devido a
# essa classe de objeto está no ambiente S4
shp_sp@data

shp_sp@data %>% 
  kable() %>% 
  kable_styling(bootstrap_options = 'striped', 
                full_width = T, 
                font_size = 12)

# Para acessar as variáveis da base de dados atrelada ao shapefile devemos 
# utilizar o operador '$'
shp_sp@data$NM_MUNICIP
shp_sp$NM_MUNICIP

# Para acessar os demais componentes, utilizamos o '@'
shp_sp@polygons # posição geográfica dos polígonos
shp_sp@plotOrder # ordem de plotagem dos polígonos
shp_sp@bbox # limites das posições geográfica dos polígonos (bounding box)
shp_sp@proj4string@projargs # sistema de projeção geográfica do shapefile


# Plotagem mais básica possível do shapefile
plot(shp_sp)


# Manipulação de dados em shapefiles -------------------------------------------

# Calculando as areas dos municípios de São Paulo (função 'area()')
raster::area(shp_sp) # está em mm²
raster::area(shp_sp) / 1000000 # está em km²

shp_sp@data['area_aprox'] <- raster::area(shp_sp) / 1000000

shp_sp@data %>% 
  kable() %>% 
  kable_styling(bootstrap_options = 'striped', 
                full_width = T, 
                font_size = 12)

# Inserindo dados externos 

load('dados_sp.RData')

dados_sp %>% 
  kable() %>% 
  kable_styling(bootstrap_options = 'striped', 
                full_width = T, 
                font_size = 12)

# Para combinar os dados do objeto dados_sp com a base de dados do nosso 
# shapefile, podemos utilizar a função merge()
shp_dados_sp <- merge(x = shp_sp, y = dados_sp, 
                      by.x = 'CD_GEOCMU', by.y = 'codigo')

class(shp_dados_sp)
shp_dados_sp %>% 
  kable() %>% 
  kable_styling(bootstrap_options = 'striped', 
                full_width = T, 
                font_size = 12)


# Salvando o objeto Shapefile criado --------------------------------------

writeOGR(obj = shp_dados_sp, 
         layer = 'novo_shapefile', 
         driver = 'ESRI Shapefile', 
         dsn = 'dir_novo_shapefile')

# Plotando os dados do banco de dados do objeto shp_sp_dados, utilizando o 
# ggplot2

shp_dados_sp@data %>% 
  ggplot() +
  geom_histogram(aes(x = idh), fill = 'darkblue', color = 'white') +
  labs(x = 'IDH', y = "Frequência") +
  theme_bw()

# Como fazer a plotagem espacial???


# Visualização de dados espaciais ---------------------------------------------

# Utilizando o ggplot2

# Passo 1: transformar o shapefile num objeto do tipo Data Frame (utilizando a 
# função tidy do pacote broom) e, depois, importar os dados que já estavam no 
# shapefile para o novo objeto do Data Frame

shp_dados_df <- tidy(shp_dados_sp, region = 'CD_GEOCMU') %>% 
  rename(CD_GEOCMU = id) %>% 
  left_join(shp_dados_sp@data, by = 'CD_GEOCMU')

# Passo 2: Plotagem
shp_dados_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = idh), 
               color = 'black') +
  labs(x = 'Longitude', y = 'Latitude', fill = 'IDH') +
  scale_fill_viridis_b() +
  theme_bw()


# Utilizando o tmap
tm_shape(shp = shp_dados_sp) + 
  tm_fill(col = 'idh', palette = 'Blues')

# Paletas de cores disponíveis
display.brewer.all()

# Reconstruindo o mapa utilizando outra paleta de cores e propondo 4 variações
# de cores (quartil)

tm_shape(shp = shp_dados_sp) +
  tm_fill(col = 'idh', style = 'quantile', 
          n = 4, palette = 'Spectral')

# Assim como no ggplot2, podemos utilizar paletas de cores mais amigáveis para 
# pessoas com daltonismo. As opções são: viridis, cividis, plasma, inferno e 
# magma

tm_shape(shp = shp_dados_sp) +
  tm_fill(col = 'idh', style = 'quantile', 
          n = 4, 
          palette = 'viridis')

# Adicionando um histograma ao mapa
tm_shape(shp = shp_dados_sp) +
  tm_fill(col = 'idh', style = 'quantile', 
          n = 4, 
          palette = 'cividis', 
          legend.hist = TRUE)

# Reposicionando o histograma
tm_shape(shp = shp_dados_sp) +
  tm_fill(col = 'idh', style = 'quantile', 
          n=4, palette = 'plasma', legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE)

# Posicionando manualmente o histograma e adicionando título
tm_shape(shp = shp_dados_sp) +
  tm_fill(col = 'idh', style = 'quantile', n = 4, palette = 'magma', 
          legend.hist = T) + 
  tm_layout(legend.text.size = 0.7,
            legend.title.size = 0.5,
            legend.hist.size = 0.5,
            legend.hist.height = 0.2,
            legend.hist.width = 0.3, 
            frame = FALSE, 
            main.title = 'A distribuição do IDH nas cidades de SP')

# Adicionando uma bússola e borda aos polígonos
tm_shape(shp = shp_dados_sp) +
  tm_fill(col = 'idh', style = 'quantile', n = 4, palette = 'inferno', 
          legend.hist = T) +
  tm_layout(legend.text.size = 0.7, 
            legend.title.size = 0.5, 
            legend.hist.size = 0.5,
            legend.hist.height = 0.2, 
            legend.hist.width = 0.3, 
            frame = FALSE, 
            main.title = 'A distribuição do IDH nas cidades de SP') +
  tm_borders(alpha = 0.5) +
  tm_compass(type = '8star', 
             show.labels = 3, size = 5, text.size = 0.5, position = c('right',
                                                                      'top'))


# 3 - Desmembrando shapefiles ---------------------------------------------

# Carregando um novo shapefile
shp_mundo <- readOGR(dsn = 'shapefile_mundo', layer = 'mundo',
                     encoding = 'UTF8')

# Visualizando o shapefile 
tm_shape(shp = shp_mundo) +
  tm_borders()

# Observando as variáveis da base de dados do obj shp_mundo
shp_mundo@data %>% 
  kable() %>% 
  kable_styling(bootstrap_options = 'striped', 
                full_width = T, 
                font_size = 12)

# Supondo que a intenção seja trabalhar apenas com o gráfico referente a 
# América do Sul, a var 'contnnt' faz essa distinção e nos ajudará a realizar 
# recorte

shp_amsul <- shp_mundo[shp_mundo@data$contnnt == 'South America', ]

class(shp_amsul)

# Plotanto o mapa da America do Sul
tm_shape(shp = shp_amsul) +
  tm_borders()


# 4 - Combinando Shapefiles -----------------------------------------------

# Supondo que a intenção seja traabalhar com o mapa dos países que estão no 
# Mercosul

# Carregando os shapefiles necessários
shp_argentina <- readOGR(dsn = 'shapefile_mercosul', 
                         layer = 'argentina_shapefile', encoding = 'UTF8')
shp_brasil <- readOGR(dsn = 'shapefile_mercosul', 
                         layer = 'brasil_shapefile', encoding = 'UTF8')
shp_paraguai <- readOGR(dsn = 'shapefile_mercosul', 
                      layer = 'paraguai_shapefile', encoding = 'UTF8')
shp_venezuela <- readOGR(dsn = 'shapefile_mercosul', 
                      layer = 'venezuela_shapefile', encoding = 'UTF8')

# A combinação desses objetos pode ser feita pela função bind() do pacote raster

shp_mercosul <- bind(shp_argentina, 
                     shp_brasil, 
                     shp_paraguai, 
                     shp_venezuela)

class(shp_mercosul)

# Avaliando a base de dados do objeto criado
shp_mercosul@data %>% 
  kable() %>% 
  kable_styling(bootstrap_options = 'striped', 
                full_width = T, 
                font_size = 12)

# Plotando o mapa
tm_shape(shp = shp_mercosul) +
  tm_borders(lwd = 1) +
  tm_fill(col = 'mercosul', palette = 'plasma') +
  tm_layout(legend.width = 0.8)



# FIM ---------------------------------------------------------------------