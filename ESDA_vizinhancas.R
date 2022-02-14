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

# O primeiro passo é a criação de um objeto tipo 'lista' que contenha os outputs
# da função 'knearneigh'
lista_knear <- knearneigh(coordinates(shp_sc), k = 3, longlat = TRUE)
class(lista_knear)

# Calculando as vizinhanças
vizinhos_knear <- knn2nb(knn = lista_knear)
class(vizinhos_knear)

# Informações relevantes sobre a vizinhança estabelecida
vizinhos_knear

# Visualizando a vizinhança estabelecida
plot.new()
plot(shp_sc, border = 'lightgray')
plot(vizinhos_knear, coordinates(shp_sc), add = TRUE, 
     col = 'darkorchid4')

# Extraindo a matriz W
matrizw_knear <- nb2mat(neighbours = vizinhos_knear, style = 'B', 
                        zero.policy = TRUE)
class(matrizw_knear)

colnames(matrizw_knear) <- shp_sc@data$NM_MUNICIP
rownames(matrizw_knear) <- shp_sc@data$NM_MUNICIP

view(matrizw_knear)


# Vizinhanças Ponderadas por distâncias sociais --------------------------------

# Carregando a base de dados
load('dados_sp.RData')
summary(dados_sp)

# Juntando a base de dados ao shapefile de SP
shp_sp@data %>%
  rename(codigo = CD_GEOCMU) %>% 
  mutate(codigo = as.numeric(codigo)) %>% 
  left_join(dados_sp, by = 'codigo') -> shp_sp@data

shp_sp@data %>% 
  kable() %>% 
  kable_styling(bootstrap_options = 'striped', 
                full_width = T, 
                font_size = 12)

# Para que possamos traçar as distâncias sociais, assumindo como métrica de 
# distância social o pib das cidades de SP, devemos estipular uma distância de 
# corte di(k). Para o caso, por conveniência, utilizamos como cut-off a variação
# de 0.01 desvio padrão 

# Apenas para facilitar o cálculo, iremos padronizar a variável pib pelo 
# procedimento zscores

shp_sp@data['zpib'] <- scale(shp_sp@data$pib)

summary(shp_sp@data)
sd(shp_sp@data$zpib)

# A matriz de distâncias sociais pode ser obtida da seguinte maneira:
matrizw_distsocial <- distancia_social(x = shp_sp@data$zpib, 
                                       dmin = 0,
                                       dmax = 0.01, 
                                       style = 'B')

class(matrizw_distsocial)

colnames(matrizw_distsocial) <- shp_sp@data$NM_MUNICIP
rownames(matrizw_distsocial) <- shp_sp@data$NM_MUNICIP

view(matrizw_distsocial)

# Calculando as vizinhanças
vizinhos_distsocial <- mat2listw(x = matrizw_distsocial)
class(vizinhos_distsocial)

plot.new()
plot(shp_sp, border = 'lightgray')
plot(vizinhos_distsocial, coordinates(shp_sp), add = TRUE,
     col = 'darkorchid4') # difícil visualização, como já era de se esperar


# Padronização de Matrizes Eapaciais --------------------------------------

# 1 - Padronização em linha da Matriz W

# Utilizaremos para esse exemplo o objeto vizinhos_queen
vizinhos_queen

# A matriz binária W de pesos espaciais do objeto vizinhos_queen está no objeto
# matrizw_queen

# Definindo a matriz padronizada em linha:
matrizw_queen_linha <- nb2mat(neighbours = vizinhos_queen, style = 'W', 
                              zero.policy = TRUE)
class(matrizw_queen_linha)

colnames(matrizw_queen_linha) <- rownames(matrizw_queen_linha)
view(matrizw_queen_linha)


# 2 - Dupla padronização da matriz W 

matrizw_queen_dupla_padr <- nb2mat(neighbours = vizinhos_queen, style = 'U', 
                                   zero.policy = TRUE)
class(matrizw_queen_dupla_padr)

colnames(matrizw_queen_dupla_padr) <- rownames(matrizw_queen_dupla_padr)
view(matrizw_queen_dupla_padr)


# 3 - Padronização da matriz W pela estabilização da Variância

# Esse tipo de padronização é mais indicado quando avançamos até métodos 
# preditivos, que para Análise Espacial normalmente são expansões da OLs, que 
# possui um pressuposto importante que é a homocedasticidade dos termos de erro
# do modelo. Quando os termos de erro se mostram Heterocedásticos em Análise
# Espacial, esse tipo de padronização provavelmente irá ajudar


matrizw_queen_est_var <- nb2mat(neighbours = vizinhos_queen, style = 'S', 
                                zero.policy = TRUE)
class(matrizw_queen_est_var)

colnames(matrizw_queen_est_var) <- rownames(matrizw_queen_est_var)
view(matrizw_queen_est_var)


# Técnicas para verificação de Autocorrelação Espacial -------------------------

# Antes de seguir com as autocorrelações espaciais sobre a variável IDH,
# vamos observar alguns comportamentos:
tm_shape(shp = shp_sp) +
  tm_fill(col = 'idh', style = 'quantile', n = 5, palette = '-magma') +
  tm_borders()

# 1 - Autocorrelação Global: a estatística I de Moran --------------------------

# Para o cálculo da Estatítica I de Moran, o algoritmo espera como declaração
# um objeto da classe 'listw'. Como exemplo, utilizaremos o objeto 
# 'matriw_queen'
listw_queen <- mat2listw(matrizW_queen)
class(listw_queen)

# Após essa etapa estamos aptos a utilizar a função 'moran.test', do pacote
# 'spdep'
moran.test(x = shp_sp@data$idh, 
          listw = listw_queen,
          zero.policy = TRUE)

# 2 - Diagrama da Estatística I de Moran ---------------------------------------
moran.plot(x = shp_sp@data$idh, 
           listw = listw_queen, 
           zero.policy = TRUE, xlab = 'IDH', 
           ylab = "IDH Espacialmente Defasado", pch = 19)
?'moran.plot'
# O eixo Y (IDH Espacialmente Defasado) é obtido pela multiplicação matricial
# da matriz de ponderação espacial W pelo vetor da variável original ('idh')

# 3 - Autocorrelação Local: a Estatística Moran Local --------------------------

# Seguindo o pressuposto de Arselin (1995), devemos padronizar em linha nossa
# matriz de pesos espaciais
matrizw_queen_linha <- nb2mat(neighbours = vizinhos_queen, style = 'W', 
                              zero.policy = TRUE)

listw_queen <- mat2listw(x = matrizw_queen_linha)

# Aplicando a função 'localmoran', do pacote 'spdep' no objeto tipo listw 
# 'listw_queen'
moran_local <- localmoran(x = shp_sp@data$idh, listw = listw_queen, 
                          zero.policy = TRUE)
class(moran_local)

# Parêntese: calculando a I de Moran local na mão:
rowSums(sweep(x = matrizw_queen_linha, MARGIN = 2, 
              STATS = scale(shp_sp@data$idh), FUN = '*')) *
  scale(shp_sp@data$idh)

# Unindo os resultados da Estatística Moran Local no dataset do objeto shp_sp:
moran_local_mapa <- cbind(shp_sp, moran_local) # utiliza o objeto 'shp_sp' e não
# 'shp_sp@data' porque a intenção é gerar ainda um objeto do tipo Spatial Points
# e a função 'cbind' já faz essa união em 'data'
class(moran_local_mapa)

# Plotando:
tm_shape(shp = moran_local_mapa) +
  tm_fill(col = 'Ii', style = 'quantile', n = 5, palette = '-magma') +
  tm_borders()

# Corrigindo o erro da paleta de cores
quantile(moran_local_mapa@data$Ii, type = 5, probs = c(0,0.2,0.4,0.6,0.8))

moran_local_mapa@data <- moran_local_mapa@data %>% 
  mutate(faixa_quantis = factor(quantcut(x = Ii, q = 5)))

summary(moran_local_mapa@data$faixa_quantis)

tm_shape(shp = moran_local_mapa) +
  tm_fill(col = 'faixa_quantis', palette = '-magma') +
  tm_borders()

# Um atributo interessante do objeto 'moran_local' é o 'quadr', que nos ajuda
# a fazer um plot do diagrama
attr(x = moran_local, which = 'quadr')

moran_local_mapa <- cbind(moran_local_mapa, 
                          attr(x = moran_local, which = 'quadr')[1])

tm_shape(shp = moran_local_mapa) +
  tm_fill(col = 'mean', palette = '-magma') +
  tm_borders(col = 'lightgray')

# 4 - Clusterização LISA -------------------------------------------------------

# O primeiro passo é estabelecer um objeto que reservará espaços para conter  
# os quadrantes HH, LL, HL, LH
quadrantes <- vector(mode = 'numeric', length = nrow(moran_local))

# Criando um vetor que contenha o centro das observações da variável idh ao 
# redor de sua média
centro_idh <- shp_sp@data$idh - mean(shp_sp@data$idh)
centro_idh

# Criando um vetor que contenha o centro dos valors da Estatística Moran Local
# em torno de sua média
centro_moran_local <- moran_local[,1] - mean(moran_local[,1])
centro_moran_local

# Utilizando uma significancia estatística de 0.05
sig <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes
quadrantes[centro_idh > 0 & centro_moran_local > 0] <- 'HH'
quadrantes[centro_idh > 0 & centro_moran_local < 0] <- 'HL'
quadrantes[centro_idh < 0 & centro_moran_local > 0] <- 'LH'
quadrantes[centro_idh < 0 & centro_moran_local < 0] <- 'LL'

quadrantes

# Aplicando a significancia estatística
quadrantes[moran_local[,5] > sig] <- 'Estatisticamente_nao_significante'

quadrantes

# Unindo 'quadrantes' à 'moraa_local_mapa'
moran_local_mapa@data['quadrantes'] <- factor(quadrantes)

tm_shape(moran_local_mapa) +
  tm_fill(col = 'quadrantes', 
          palette = c(HH = 'darkred', 
                      HL = 'red', 
                      LH = 'lightblue', 
                      LL = 'darkblue', 
                      Estatisticamente_nao_significante = 'white'))+
  tm_borders(col  = 'lightgray')

# Versão do gráfico anterior para daltônicos:
tm_shape(shp = moran_local_mapa) +
  tm_polygons(col = "quadrantes", 
              pal = c(HH = "#FDE725FF",
                      HL = "#7AD151FF", 
                      LH = "#2A788EFF", 
                      LL = "#440154FF",
                      Estatisticamente_não_significante = "white")) +
  tm_borders()


# 5 - Autocorrelação Local: A estatística G de Getis e Ord ---------------------

# A estatística G de Getis e Ord, por definição espera uma matriz espacial W
# de distâncias geográficas (ex.: 'matrizw_distancias', do estado da Bahia)

# Importando os dados de IDH das cidades do estado
load('dados_ba.RData')

# Adicionando ao shapefile
shp_ba@data %>%
  left_join(dados_ba, by = 'Codigo') -> shp_ba@data

# Transformando o objeto 'matrizw_distancia' em um objeto da classe 'listw'
listw_dist <- mat2listw(matrizW_distancia)

# Calculando a Estatística G de Getis e Ord com a função 'localG', do pacote
# 'spdep'
g_local <- localG(x = shp_ba@data$idh, listw = listw_dist, zero.policy = TRUE)
class(g_local)

# Unindo os dados de 'g_local' com os dados do nosso shapefile
mapa_G <- cbind(shp_ba, as.matrix(g_local))

head(mapa_G@data) # nome da var ficou estranho

mapa_G@data %>% 
  rename(estatistica_g = 4) -> mapa_G@data

head(mapa_G@data)

# Plotando a Estatística G de forma espacial
tm_shape(shp = mapa_G) +
  tm_fill(col = 'estatistica_g', palette = '-viridis') +
  tm_borders(col = 'lightgray') # paleta de cores ruins. Vamos acertar como no 
# exemplo anterior, fazendo os quantiles separadamente
mapa_G@data %>% 
  mutate(faixa_quantis = factor(quantcut(x = estatistica_g, q = 8))) -> 
  mapa_G@data

# Plotando novamente
tm_shape(shp = mapa_G) +
  tm_fill(col = 'faixa_quantis', palette = '-viridis') +
  tm_borders(col = 'lightgray')

tm_shape(shp = mapa_G) +
  tm_fill(col = 'faixa_quantis', palette = '-RdBu') +
  tm_borders(col = 'lightgray') # paleta mais comum de se encontrar em 
# trabalhos

# FIM
