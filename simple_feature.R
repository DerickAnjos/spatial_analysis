# Instalação e carregamento dos pacotes para o script de Simple Feature

pacotes <- c("tidyverse","sf","tmap","rgdal","rgeos","adehabitatHR","knitr",
             "kableExtra")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# 1 - Criando um objeto sf (simple feature) a partir de um BD -----------------

# Carregando a base de dados
load('shoppings.RData')

# Observando a classe do objeto shoppings
class(shoppings)

shoppings %>% 
  kable() %>% 
  kable_styling(bootstrap_options = 'striped', 
                full_width = T, 
                font_size = 12)

# Criando o objeto do tipo sf
sf_shoppings <- st_as_sf(x = shoppings, coords = c('longitude', 'latitude'), 
                         crs = 4326)

# Classe do objeto criado
class(sf_shoppings) # do tipo sf e data.frame 

# É possível notar que o objeto sf é um data.frame georreferenciado que não 
# possui polígonos associado, nem a necessidade de se utilizar o operador '@'
# para acessar seus dados, pois a classe está no ambiente S3

# Plotando o objeto de forma espacial
tm_shape(shp = sf_shoppings) +
  tm_dots(size = 0.5)


# Nota: Funções attributes() e attr() -----------------------------------------

# Tudo no R é um objeto. Porém, alguns outputs parecem ser inacessíveis para o 
# usuário, como:
sf_shoppings$geometry

# O comando acima leva a uma resposta extremamente mais resumida do que o
# esperado.

# O R por vezes pode, intencionalmente, pode apresentar alguns objetos e omitir
# ou mesmo deixar fora do alcance do usuário alguns dados. uma forma de extrair 
# mais dos objetos do R pode ser a função attributes()
attributes(sf_shoppings$geometry)

# Toda vez que for necessário debruçar sobre alguns desses atributos, podemos 
# utilizar a função attr(), argumentando which:
attr(sf_shoppings$geometry, which = 'crs')
attr(sf_shoppings$geometry, which = 'bbox')
attr(sf_shoppings$geometry, which = 'class')


# Plotando o objeto sf  -------------------------------------------------------

# Adicionando uma camada de um mapa do Leafleet que considere a bounding box do 
# objeto sf_shoppings:

# Utilizar a  função tmap_mode() para alterar o modo de exibição - com camada 
# de mapa leafleet por trás ('view') ou a plotagem 'solta', apenas do banco de 
# dados ('plot')
tmap_mode('view')

tm_shape(shp = sf_shoppings) +
  tm_dots(col = 'aquamarine4', 
          size = 0.2,
          border.col = 'black', 
          alpha = 0.8)

tmap_mode('plot')


# Combinando um projeto Simple Feature com Shapefile ----------------------

# Carregando o Shapefile do município de São Paulo
shp_saopaulo <- readOGR(dsn = 'shapefile_municipio', layer = 'municipio_sp',
                        encoding = 'UTF8')

# Explorando mais profundamente nosso objeto Shapefile
shp_saopaulo@proj4string

# Datum: South America Datum 1969; Projeção: Transverse Mercator; Zona: 23S.

# Visualização gráfica do objeto
tm_shape(shp = shp_saopaulo) +
  tm_borders()

# Combinando o objeto shp_saopaulo com o objeto sf_shopping
tm_shape(shp = shp_saopaulo) +
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sf_shoppings) +
  tm_dots(size = 0.2, col = 'regiao', palette = 'viridis')


# 3 - Buffer Analysis -----------------------------------------------------

# O buffering é uma técnica para se medir distâncias para fora de um dado ponto
# geográfico

# A aplicação técnica de buffering pode ser feita com o uso da função gBuffer()
# do pacote rgeos
buffer_shoppings <- gBuffer(spgeom = sf_shoppings, 
                            width = 1500, byid = TRUE)

# A função gBuffer() não funciona com objetos do tipo sf. Para utilizá-la, 
# teremos que transformar o objeto sf_shoppings para o tipo Spatial points (sp)

# Para tal, primeiramente, precisaremos isolar as coordenadas de longitude 
# e latitude do data frame original shoppings
coordenadas_shoppings <- cbind(shoppings$longitude, shoppings$latitude)

# Após, utilizaremos a função SpatialPoints() para criar um objeto do tipo sp
sp_shoppings <- SpatialPoints(coords = coordenadas_shoppings, 
                              proj4string = CRS('+proj=longlat'))

class(sp_shoppings)

sp_shoppings@coords
sp_shoppings@bbox
sp_shoppings@proj4string

plot(sp_shoppings)

# A função SpatialPòints() não permite a existência concomitante de um data
# frame em um objeto sp. Mais abaixo iremos utilizar a função 
# SpatialPointsDataFrame(), que quebra essa lógica.

# Visualizando o objeto
tm_shape(shp = sp_shoppings)+
 tm_dots(size = 0.2, col = 'darkblue')

# Aplicando a função gBuffer novamente
buffer_shoppings <- gBuffer(spgeom = sp_shoppings, 
                            width = 1500, byid = T)

# Novamente tivemos um erro, devido o objeto sp_shoppings está com a CRS com 
# proj longlat (forma geodésica), e a função gBuffer exigir que esteja em proj 
# planar (distancias euclidianas)
shoppings_UTM <- spTransform(x = sp_shoppings, CRSobj = CRS('+init=epsg:22523'))

# Visualizando o resultado da transformação
tm_shape(shp = shoppings_UTM) + 
  tm_dots(size = 0.2, col = 'aquamarine4') # é possível notar uma leve alteração
# na posição relativa e absoluta dos pontos devido a troca do tipo de projeção
# utilizada na CRS (Datum)

# Aplicando o  gBuffer() novamente
buffer_shoppings <- gBuffer(spgeom = shoppings_UTM, 
                            byid = TRUE, width = 1500)

tm_shape(shp = buffer_shoppings) +
  tm_borders()

tmap_mode('view')

tm_shape(shp = buffer_shoppings) +
  tm_borders()

# Combinando os objetos shp_saopaulo, sf_shoppings e buffer_shoppings
tm_shape(shp = shp_saopaulo) +
  tm_borders(alpha= 0.4) +
  tm_shape(shp = sf_shoppings) + 
  tm_dots(size = 0.02, col = 'red') +
  tm_shape(shp = buffer_shoppings) +
  tm_borders(alpha = 0.8, col = 'black')


# 4 - Buffer Union --------------------------------------------------------

# A técnica de buffer union combina aqueles outputs da técnica de buffering que
# se encontrem
buffer_union <- gUnaryUnion(spgeom = buffer_shoppings)

class(buffer_union)

tm_shape(shp = shp_saopaulo) +
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sf_shoppings) +
  tm_dots(size = 0.02, col = 'regiao') +
  tm_shape(shp = buffer_union) +
  tm_borders(col = 'black', alpha = 0.8) +
  tm_fill(col = 'gray', alpha = 0.5)


# 5 - Kernel density ------------------------------------------------------

# A técnica de kernel densities calcula a densidade da presença de pontos de 
# interesse em determinada área geográfica

# O primeiro passo será criar um objeto sp com a base de dados atrelada a ele, 
# utilizando a função SpatialPointsDataFrame comentada acima
shoppings_sp_df <- SpatialPointsDataFrame(data = shoppings, 
                                          coords = coordenadas_shoppings, 
                                          proj4string = CRS('+proj=longlat'))

class(shoppings_sp_df)
shoppings_sp_df@data # data frame associado ao objeto do tipo sp

# Para o cálculo das kernel densities, podemos utilizar a função kernelUD()
shoppings_dens <- kernelUD(xy = shoppings_sp_df, h = 'href', grid = 1000, 
                           boundary = NULL)

class(shoppings_dens)
plot(shoppings_dens)

# Para estabelecer as zonas com maiores densidades
zona1 <- getverticeshr(x = shoppings_dens, percent = 20)
zona2 <- getverticeshr(x = shoppings_dens, percent = 40)
zona3 <- getverticeshr(x = shoppings_dens, percent = 60)
zona4 <- getverticeshr(x = shoppings_dens, percent = 80)

class(zona1)
plot(zona1)

tmap_options(check.and.fix = TRUE)

tm_shape(shp = shp_saopaulo) +
  tm_fill(col = 'gray90') +
  tm_borders(alpha = 0.2) +
  tm_shape(shp = shoppings_sp_df) +
  tm_dots(size = 0.25, col = 'regiao') +
  tm_shape(shp = zona1) +
  tm_borders(col = 'firebrick4', lwd = 2.5) + 
  tm_fill(col = 'firebrick4', alpha = 0.5) +
  tm_shape(shp = zona2) +
  tm_borders(col = 'firebrick3', lwd = 2.5) + 
  tm_fill(col = 'firebrick3', alpha = 0.5) +
  tm_shape(shp = zona3) +
  tm_borders(col = 'firebrick2', lwd = 2.5) + 
  tm_fill(col = 'firebrick2', alpha = 0.5) +
  tm_shape(shp = zona4) +
  tm_borders(col = 'firebrick1', lwd = 2.5) + 
  tm_fill(col = 'firebrick1', alpha = 0.5)

?'st_is_valid'

tmap_mode('plot')  
