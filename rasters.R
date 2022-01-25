# Instalação e carregamento dos pacotes para o script de Rasters ---------------

pacotes <- c("tidyverse","raster","tmap","rgdal","rayshader","profvis")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# 1 - Introdução aos objetos do tipo Rasters -----------------------------------

# Carregando o objeto raster
relevo_sp <- raster('raster_sp/relevo_sp.tif')

class(relevo_sp)
relevo_sp

# Plotando o objeto
plot.new() # limpando a área de 'plot'
plot(relevo_sp)

# Estabelecendo um histograma a respeito das altitudes do objeto relevo_sp
hist(relevo_sp, 
     main = '',
     xlab = 'Altitudes das formações geográficas',
     ylab = 'Frequência', 
     col = 'darkblue', 
     maxpixels = 2160000)

relevo_sp@data # não traz a informação

# Onde está os dados de altitude no objeto relevo_sp? Como plotá-lo utilizando
# o ggplot2?

# A base de dados do objeto relevo_sp pode ser extraída com a 
# função as.data.frame()
relevo_sp_df <- as.data.frame(relevo_sp, xy = TRUE)

class(relevo_sp_df) 

head(relevo_sp_df)
summary(relevo_sp_df)
min(relevo_sp_df$relevo_sp)
max(relevo_sp_df$relevo_sp)

# Gerando o histograma novamente, agora utilizando o ggplot2
relevo_sp_df %>% 
  ggplot() +
  geom_histogram(aes(x = relevo_sp), fill = 'aquamarine4', color = 'white') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = 'Altitudes das formações geográficas', 
       y = 'Frequência', title = 'Histograma - ggplot2') +
  theme_bw()

# Apesar da função plot() ter cumprir o esperado, a função image() possui maior 
# capacidade visual para trabalharmos com imagens
image(relevo_sp, 
      xlab = 'Longitude', ylab = 'Latitude', main = 'Relevo de parte do litoral
      paulista')

# A função image() também permite a especificação de outras cores em seus 
# resultados quando combinadas com a função terrain.colors()
image(relevo_sp, 
      xlab = 'Longitude', ylab = 'Latitude', main = 'Relevo de parte do litoral
      paulista', col = terrain.colors(10), 
      zlim = c(0,800))

# Plotando em 3D, utilizando o pacote rayshader

# Primeiro passo é criar uma matriz a partir do objeto raster
relevo_matriz <- raster_to_matrix(raster = relevo_sp, verbose = interactive())

class(relevo_matriz)

# Após
relevo_matriz %>% 
  sphere_shade(texture = 'imhof1') %>% 
  plot_3d(relevo_matriz, zscale = 50, theta = -45, phi = 45, water = TRUE, 
         windowsize = c(1000,800), zoom = 0.75, waterlinecolor = 'white', 
         waterdepth = 100)

# Para capturar um snapshot
render_snapshot()

# A plotagem espacial dos objetos raster, são compatíveis com o pacote tmap
tm_shape(shp = relevo_sp) + 
  tm_raster(style = 'quantile', 
            n = 5, palette = 'viridis') +
  tm_layout(legend.position = c('left', 'bottom'), 
            legend.outside = TRUE)


# 2 - Combinando objetos Raster com Shapefile -----------------------------

# Carregando o shapefile do Estado de SP
shp_sp <- readOGR(dsn = 'shapefile_sp', layer = 'estado_sp', 
                  encoding = 'UTF8')

# Plotando o shapefile
plot(shp_sp)

tm_shape(shp = shp_sp) + 
  tm_borders()

# Plotando a combinação do objeto Raster com Shapefile
tm_shape(shp = relevo_sp) +
  tm_raster(style = 'quantile', 
            n = 5, palette = 'viridis') +
  tm_shape(shp = shp_sp) +
  tm_borders() +
  tm_layout(legend.position = c('left', 'bottom'), 
            legend.outside = T)


# 3 - Carregando o objeto por completo para a memória RAM ----------------------

# Para verificar se o objeto relevo_sp está aberto na memória RAM do computador
# utilizamos a função inMemory()
inMemory(relevo_sp)

mem_relevo_sp <- readAll(relevo_sp) # objeto totalmente na memória RAM
inMemory(mem_relevo_sp)

# Ganho computacional com essa mudança
info_relevo_sp <- profvis(
  {
    
    for(i in 10){
      summary(relevo_sp[])
      print(i)
    }
  }
)

info_mem_relevo_sp <- profvis(
  {
    
    for(i in 10){
      summary(relevo_sp[])
      print(i)
    }
  }
)

print(info_relevo_sp)
print(info_mem_relevo_sp)


# 4 - Recortando objetos Raster ------------------------------------------------

plot(mem_relevo_sp)

# Recortando com o mouse (não tão usual) - recorte retangular. Se atentar pois 
# após executar a função drawextent(), o R apresentará o sinal de 'Stop' no 
# console e irá ficar aguardando que seja indicado com o mouse na área de 
# plotagem o vértice de início e o vértice fim
recorte_mouse_1 <- drawExtent()

class(recorte_mouse_1)
plot(recorte_mouse_1) # apenas o retangulo, sem o mapa. A seguir, veremos que 
# essa informação será fornecida no campo de determinação dos vetores com os 
# limites da área desejada

# Salvando a tarefa executada
relevo_recortado_1 <- crop(x = mem_relevo_sp, y = recorte_mouse_1)
plot(relevo_recortado_1)

plot(mem_relevo_sp)

# Outra forma de realizar o corte com o mouse, é utilizando um polígono como 
# área de corte, ao invés do retângulo. Para sair do modo 'selecione os vértices'
# deve-se apertar 'Esc'
recorte_mouse_2 <- raster::select(mem_relevo_sp, use = 'pol')
plot(recorte_mouse_2)

# Recortando os objetos utilizando vetores
# Primeiro passo: salvar as coordenadas limites do recorte
recorte_coordenadas <- c(-45.51042, -45.18708, -23.95708, -23.74125)

relevo_recortado_3 <- crop(mem_relevo_sp, recorte_coordenadas)
plot(relevo_recortado_3)
