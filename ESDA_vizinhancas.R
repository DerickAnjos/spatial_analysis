# Instalação e carregamento dos pacotes para o script de ESDA e Vizinhanças ----

pacotes <- c("tidyverse","rgdal","spdep","knitr","kableExtra","tmap","gtools")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Estabelecendo Vizinhanças -----------------------------------------------

# 01 - Vizinhanças por Contiguidade

# Carregando o shapefile
shp_sp <- readOGR(dsn = 'shapefile_sp', layer = 'estado_sp',
                  encoding = 'UTF-8') 

class(shp_sp)

# Visualizando o shapefile
plot(shp_sp)

tm_shape(shp_sp) +
  tm_borders()

# Estabelecendo Vizinhanças pelo método Queen
vizinhos_queen <- poly2nb(pl = shp_sp, queen = TRUE, 
                          row.names = shp_sp@data$NM_MUNICIP)
class(vizinhos_queen)

# Visualizando a vizinhança estabelecida
plot(shp_sp, border = 'lightgray')
plot(vizinhos_queen, coordinates(shp_sp), 
     add = TRUE, col = '#33638DFF')

# Informações relevantes sobre a vizinhança queen estabelecida
summary(vizinhos_queen)

# Matriz W
matrizW_queen <- nb2mat(neighbours = vizinhos_queen, style = 'B') # B - Binário

# O erro reportado pelo R refere-se a existência de polígonos, que pelo método
# de vizinhança utilizado (contiguidade), não possui nenhum vizinho por não 
# possuir fronteira física com nenhuma outra cidade. No caso do shapefile (SP) 
# em estudo, o erro se refere a cidade de 'Ilha Bela'
plot(shp_sp, border = 'lightgray')
plot(vizinhos_queen, coordinates(shp_sp), 
     add = TRUE, col = 'darkorchid')

# Para contornar a situação da presença de 'ilhas' no shapefile, temos que 
# indicar para a função 'nb2mat', através do parâmetro 'zero.policy' que mesmo
# com ilhas, nós queremos que ele faça a matriz W
matrizW_queen <- nb2mat(neighbours = vizinhos_queen, style = 'B', 
                        zero.policy = TRUE)

class(matrizW_queen)

colnames(matrizW_queen) <- shp_sp@data$NM_MUNICIP
view(matrizW_queen)

# Como estabelecer o critério de Contiguidade para outras ordens
vizinhos_queen_ordens <- nblag(neighbours = vizinhos_queen, maxlag = 5)
class(vizinhos_queen_ordens) # lista, e não 'nb' 

vizinhos_queen_ordens[[1]] # contiguidade de ordem 1
vizinhos_queen_ordens[[2]] # contiguidade de ordem 2
vizinhos_queen_ordens[[3]] # contiguidade de ordem 3
vizinhos_queen_ordens[[4]] # contiguidade de ordem 4
vizinhos_queen_ordens[[5]] # contiguidade de ordem 5

# Plotando vizinhanças de ordens maiores
plot(shp_sp, border = 'lightgray')
plot(vizinhos_queen_ordens[[2]], coordinates(shp_sp),
    add = TRUE, col = 'darkorchid') # exemplo com ordem 2 (acima disso o gráfico
# fica muito poluído)

plot.new() # limpando a área de plotagem

# Estabelecendo vizinhança por Contiguidade utilizando o critério Rook (torre)
vizinhos_rook <- poly2nb(pl = shp_sp, row.names = shp_sp@data$NM_MUNICIP, 
                         queen = FALSE)
class(vizinhos_rook)

plot.new()
plot(shp_sp, border = 'lightgray')
plot(vizinhos_rook, coordinates(shp_sp), 
     add = TRUE, col = 'darkorchid')

# Informações relevantes sobre a vizinhança 'rook' estabelecida
summary(vizinhos_rook)

# Extraindo a matriz W
matriz_rook <- nb2mat(neighbours = vizinhos_rook, style = 'B', 
                      zero.policy = TRUE)
class(matriz_rook)

# Melhorando a apresentação da Matriz
colnames(matriz_rook) <- shp_sp@data$NM_MUNICIP
view(matriz_rook)


# 02 - Vizinhanças por Distância Geográfica -------------------------------

# Carregando um shapefile
shp_ba <- readOGR(dsn = 'shapefile_ba', layer = 'ba_state', 
                  encoding = 'UTF-8', use_iconv = TRUE)
class(shp_ba)
?'readOGR'
# Estabelecendo vizinhanças por distância geográfica
vizinhos_distancia <- dnearneigh(coordinates(shp_ba), 
                                 d1 = 0, 
                                 d2 = 90, 
                                 longlat = TRUE)
class(vizinhos_distancia)

# Ao argumentar 'longlat = TRUE', as distâncias serão calculadas em 'km'. 
# Caso o objeto indicado no primeiro parâmetro (x) seja da classe 'sp, 
# as unidades de medida usada serão aquelas do CRS do objeto

# Informações relevantes sobre a vizinhança 'rook' estabelecida
summary(vizinhos_distancia)

# Visualizando as vizinhanças estabelecidas
plot.new()
plot(shp_ba, border = 'lightgray')
plot(vizinhos_distancia, coordinates(shp_ba),
     add = TRUE, col = 'darkorchid')

# Matriz W
matrizW_distancia <- nb2mat(neighbours = vizinhos_distancia, 
                            style = 'B', zero.policy = TRUE )
class(matrizW_distancia)

# Melhorando a apresentação da Matriz
colnames(matrizW_distancia) <- shp_ba@data$MUNICIPIO
rownames(matrizW_distancia) <- shp_ba@data$MUNICIPIO

view(matrizW_distancia)


# 03 - Vizinhanças ponderadas por k-Nearest Neighbors -------------------------

# Carregando um shapefile
shp_sc <- readOGR(dsn = 'shapefile_sc', layer = 'sc_state',
                  encoding = 'UTF-8', use_iconv = TRUE)

class(shp_sc)

?'spdep'
