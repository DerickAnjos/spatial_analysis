# Instalação e carregamento dos pacotes para o script de Modos de visualização -

pacotes <- c("rgdal","plotly","tidyverse","knitr","kableExtra","gridExtra",
             "png","grid","magick","rgl","devtools","GISTools",
             "tmap","broom", 'dplyr')

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


# Nesse script utilizaremos alguns outros algoritmos para a Visualização de 
# objetos espaciais

# Carregando um shapefile da região Centro-oeste do BR
shp_centro_oeste <- readOGR(dsn = 'shapefile_centrooeste', 
                            layer = 'centrooeste_shapefile', encoding = 'UTF8')
class(shp_centro_oeste)

shp_centro_oeste@data %>% 
  kable() %>% 
  kable_styling(bootstrap_options = 'striped', 
                full_width = T, font_size = 12)

tm_shape(shp = shp_centro_oeste) +
  tm_borders()

tmap_mode('view')
tmap_mode('plot')

# Identificando o nome das cidades da região
tm_shape(shp = shp_centro_oeste) + 
  tm_borders() +
  tm_text(text = 'NM_MUNICIP', size = 0.4)

# Como há vários polígonos (cidades), fica impraticável colocar os nomes dessa 
# forma. Uma outra alternativa é utilizar o ggplot2 e aplicar o ggplotly para 
# deixar o mapa interativo


# Passo 1: transformando o shapefile num data.frame
shp_centro_oeste_df <- tidy(shp_centro_oeste, region = 'CD_GEOCODM') %>% 
  rename(CD_GEOCODM = id) %>% 
  mutate(state = substr(x = group, start = 1, stop = 2), 
         state = factor(state, 
                        levels = c('50', '51', '52', '53'),
                        labels = c('MS', 'MT', 'GO', 'DF')),
         city_cod = substr(x = group, start = 1, stop = 7)) %>% 
  left_join(shp_centro_oeste@data, 
            by = 'CD_GEOCODM') %>% 
  rename(city = NM_MUNICIP)

# Passo 2: Fazendo a plotagem de forma espacial e interativa
ggplotly(
  shp_centro_oeste_df %>% 
    ggplot() + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = state, 
                     label = city), color = 'black') +
    labs(x = 'Longitude', y = 'Latitude', title = 'Centro Oeste', 
         fill = 'State') +
    scale_fill_viridis_d(option = 'viridis') +
    theme_bw()
)

# Destacando as capitais de cada UF

# Passo 1: Criando uma variável que identifica as capitais no objeto

shp_centro_oeste_df %>% 
  mutate(capital = ifelse(city %in% c('CAMPO GRANDE', 'CUIABÁ', 
                                      'GOIÂNIA', 'BRASÍLIA'),
                          yes = TRUE, no = FALSE)) -> shp_centro_oeste_df

# Passo 2: Fazendo a plotagem de forma espacial e interativa
ggplotly(
  shp_centro_oeste_df %>% 
    ggplot() +
    geom_polygon(aes(x = long, y = lat, group = group, fill = capital, 
                     label = city), color = 'gray90') +
    labs(x = 'Longitude', y = 'Latitude', title = 'Centro Oeste', 
         fill = 'Capital?') + 
    scale_fill_viridis_d(option = 'viridis') +
    facet_wrap(~state) +
    theme_bw()
)

# Adicionado informações de pobreza sobre o mapa
load('dados_centro_oeste.RData')

shp_centro_oeste_dados <- merge(x = shp_centro_oeste, 
                                y = dados_centro_oeste, 
                                by.x = 'CD_GEOCODM', 
                                by.y = 'CD_GEOCODM')

# Plotando espacialmente a variável de interesse:
tm_shape(shp = shp_centro_oeste_dados) +
  tm_fill(col = 'poverty', style = 'quantile', n = 4, palette = 'viridis', 
          legend.hist = TRUE) +
  tm_borders(alpha = 0.7) +
  tm_compass() +
  tm_layout(legend.outside = TRUE)

# Para fazer um mapa semelhante no ggplot2
shp_centro_oeste_df <- tidy(shp_centro_oeste_dados, region = 'CD_GEOCODM') %>% 
  rename(CD_GEOCODM = id) %>% 
  left_join(shp_centro_oeste_dados@data, by = 'CD_GEOCODM')

# Vamos reconstruir o último mapa, explorando algumas palestas de cores do
# pacote viridis:

shp_centro_oeste_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = poverty), 
               color = 'gray80') +
  labs(x = 'Longitude', y = 'Latitude', title = 'Centro Oeste') +
  scale_fill_viridis_c(option = 'viridis') +
  theme_bw()

# Para replicar o último mapa feito utilizando o pacote tmap, mas agora com o 
# ggplot2, temos que replicar identicamente as faixas estabelecidas no tmap
shp_centro_oeste_df <- shp_centro_oeste_df %>% 
  mutate(poverty_bands = ifelse(poverty < 25.4, yes = '7.84 to 25.40',
                                no = ifelse(poverty < 31.8, 
                                            yes = '25.40 to 31.80',
                                            no = ifelse(poverty < 39.88,
                                                        yes = '31.80 to 39.88',
                                                        no = '39.88 to 72.04'))),
         poverty_bands = factor(poverty_bands,
                                levels = c('7.84 to 25.40', '25.40 to 31.80',
                                           '31.80 to 39.88', '39.88 to 72.04')))
  
# Plotando...

shp_centro_oeste_df %>% 
  ggplot() +
  geom_polygon(aes(x = long,y = lat, fill = poverty_bands, group = group), 
             color = 'gray85') +
  labs(x = 'Longitude', y = "Latitude", title = 'Centro Oeste', 
       fill = 'poverty') +
  scale_fill_viridis_d(option = 'viridis') +
  theme_bw()
  
# Utilizando outras paletas
shp_centro_oeste_df %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = poverty_bands, group = group), 
               color = 'gray75') +
  scale_fill_viridis_d(option = 'cividis') +
  labs(x = 'Longitude', y = 'Latitude', title = 'Centro Oeste', 
       fill = 'Poverty') +
  theme_bw()

shp_centro_oeste_df %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = poverty_bands, group = group), 
               color = 'gray75') +
  scale_fill_viridis_d(option = 'inferno') +
  labs(x = 'Longitude', y = 'Latitude', title = 'Centro Oeste', 
       fill = 'Poverty') +
  theme_bw()

shp_centro_oeste_df %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = poverty_bands, group = group), 
               color = 'gray75') +
  scale_fill_viridis_d(option = 'magma') +
  labs(x = 'Longitude', y = 'Latitude', title = 'Centro Oeste', 
       fill = 'Poverty') +
  theme_bw()

shp_centro_oeste_df %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = poverty_bands, group = group), 
               color = 'gray75') +
  scale_fill_viridis_d(option = 'plasma') +
  labs(x = 'Longitude', y = 'Latitude', title = 'Centro Oeste', 
       fill = 'Poverty') +
  theme_bw()

shp_centro_oeste_df %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = poverty_bands, group = group), 
               color = 'gray75') +
  scale_fill_viridis_d(option = 'turbo') +
  labs(x = 'Longitude', y = 'Latitude', title = 'Centro Oeste', 
       fill = 'Poverty') +
  theme_bw()

shp_centro_oeste_df %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = poverty_bands, group = group), 
               color = 'gray75') +
  scale_fill_viridis_d(option = 'rocket') +
  labs(x = 'Longitude', y = 'Latitude', title = 'Centro Oeste', 
       fill = 'Poverty') +
  theme_bw()

shp_centro_oeste_df %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = poverty_bands, group = group), 
               color = 'gray75') +
  scale_fill_viridis_d(option = 'mako') +
  labs(x = 'Longitude', y = 'Latitude', title = 'Centro Oeste', 
       fill = 'Poverty') +
  theme_bw()

# Plotando o histograma da variável 'poverty'
shp_centro_oeste_df %>% 
  ggplot() +
  geom_histogram(aes(x = poverty, fill = ..count..), color = 'white') +
  # scale_fill_gradient('Poverty', low = "#440154FF", high = "#FDE725FF") +
  scale_fill_viridis_c() +
  labs(x = 'Poverty', y = 'Frequência', title = 'Poverty - Centro Oeste', 
       fill = 'Poverty') + 
  theme_bw()

# Próximo passo é unir esses dois plots em apenas 1, como no tmap
# Passo 1: salvar os plots em objetos distintos
mapa <- shp_centro_oeste_df %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = poverty_bands, group = group), 
               color = 'gray75') +
  scale_fill_viridis_d(option = 'mako') +
  labs(x = 'Longitude', y = 'Latitude', title = 'Centro Oeste', 
       fill = 'Poverty') +
  theme_bw()

histograma <- shp_centro_oeste_df %>% 
  ggplot() +
  geom_histogram(aes(x = poverty, fill = ..count..), color = 'white') +
  # scale_fill_gradient('Poverty', low = "#440154FF", high = "#FDE725FF") +
  scale_fill_viridis_c() +
  labs(x = 'Poverty', y = 'Frequência', title = 'Poverty - Centro Oeste', 
       fill = 'Poverty') + 
  theme_bw()

# Passo 2: utilizar a função grid.arrange(), do pacote gridExtra, para combinar
# os objetos que contem os plots

# Versão em linha
grid.arrange(mapa, histograma, nrow = 1)

# Versão em coluna
grid.arrange(mapa, histograma, ncol = 1)

# Versão livre
grid.arrange(mapa, histograma, widths = c(50, 25), heights = c(50, 25))


# Plotagem da variável de interesse, de forma espacial e 3D

# Carregando o shapefile do estado de SC
shp_sc <- readOGR(dsn = 'shapefile_sc', layer = 'sc_state')

tmap_mode('view')

tm_shape(shp = shp_sc) +
  tm_borders()

tmap_mode('plot')

# Carregando os dados de pobreza de SC
load('dados_sc.RData')

dados_sc %>% 
  kable() %>% 
  kable_styling(bootstrap_options = 'striped', 
                full_width = T, 
                font_size = 12)

summary(dados_sc) # há missing values

# Passo 1: Transformando o objeto shp_sc em um data frame
shp_sc_df <- tidy(shp_sc, region = 'CD_GEOCMU') %>% 
  rename(CD_GEOCMU = id)

# Passo 2: Juntando as informação da base de dados 'dados_sc' no objeto 
# shp_sc_df
shp_sc_df <- shp_sc_df %>% 
  left_join(dados_sc, by = 'CD_GEOCMU')

# Gerando o mapa utilizando o pacote ggplot2
shp_sc_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, fill = poverty, group = group), 
               color = 'black') +
  labs(x = 'Longitude', y = 'Latitude', title = 'Santa Catarina', 
       fill = 'Poverty') +
  scale_color_viridis_c(option = 'viridis') +
  theme_bw()

# Ou

shp_sc_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, fill = poverty, group = group), 
               color = 'black') +
  scale_fill_gradient(limits = range(shp_sc_df$poverty), 
                      low = "#FFF3B0", 
                      high="blue") +
  layer(geom = 'path', stat = 'identity', position = 'identity', 
        mapping = aes(x = long, y = lat, group = group, color = I('#FFFFFF'))) +
  theme(legend.position = 'none', 
        axis.line = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())

# Passo 4: salvando mapa em um objeto
mapa_sc <- shp_sc_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, fill = poverty, group = group), 
               color = 'black') +
  scale_fill_gradient(limits = range(shp_sc_df$poverty), 
                      low = "#FFF3B0", 
                      high="#E09F3E") +
  layer(geom = 'path', stat = 'identity', position = 'identity', 
        mapping = aes(x = long, y = lat, group = group, color = I('#FFFFFF'))) +
  theme(legend.position = 'none', 
        axis.line = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())
class(mapa_sc)

# Passo 5: salvando o arquivo mapa_sc com um arquivo de extensão .png, com boa
# resolução (tipo imagem)
xlim <- ggplot_build(plot = mapa_sc)$layout$panel_scales_x[[1]]$range$range
ylim <- ggplot_build(plot = mapa_sc)$layout$panel_scales_y[[1]]$range$range

ggsave(filename = 'mapa_co_dsa.png', width = diff(xlim) *4, 
       height = diff(ylim)*4, units = 'cm')

# Passo 6: carregando a imagem do passo anterior
background_mapa <- readPNG('mapa_co_dsa.png')
class(background_mapa)

# Passo 7: capturando as coordenadas dos centróides de cada município de Sc
# em um data frame
coordinates(shp_sc) %>% 
  data.frame() %>% 
  rename(longitude = 1, 
         latitude = 2) %>% 
  mutate(CD_GEOCMU = shp_sc@data$CD_GEOCMU) %>%
  dplyr::select(latitude, everything()) -> coords_sc

# Passo 8: adicionando as coordenadas dos municipios no mapa
shp_sc_df <- shp_sc_df %>% 
  left_join(coords_sc, by = 'CD_GEOCMU')

summary(shp_sc_df)

# Passo 9: georreferenciando a imagem e plotando os dados sobre pobreza sobre
# os centróides dos municipios de sc
shp_sc_df %>% 
  ggplot() +
  annotation_custom(
    rasterGrob(background_mapa, 
               width = unit(1, 'npc'),
               height = unit(1, 'npc')), -Inf, Inf, -Inf, Inf) +
  xlim(xlim[1], xlim[2]) +
  ylim(ylim[1], ylim[2]) +
  geom_point(aes(x = longitude, y = latitude, color = poverty), size = 1.5) +
  scale_color_gradient(name = 'Poverty', 
                       limits = range(shp_sc_df$poverty),
                       low = "#FCB9B2", 
                       high = "#B23A48") +
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())

# Passo 10: salvando o resultado anterior
mapa_pobreza <- shp_sc_df %>% 
  ggplot() +
  annotation_custom(
    rasterGrob(background_mapa, 
               width = unit(1, 'npc'),
               height = unit(1, 'npc')), -Inf, Inf, -Inf, Inf) +
  xlim(xlim[1], xlim[2]) +
  ylim(ylim[1], ylim[2]) +
  geom_point(aes(x = longitude, y = latitude, color = poverty), size = 1.5) +
  scale_color_gradient(name = 'Poverty', 
                       limits = range(shp_sc_df$poverty),
                       low = "#FCB9B2", 
                       high = "#B23A48") +
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())

# Passo 11: gerando o mapa 3D da pobreza em SC
plot_gg(ggobj = mapa_pobreza, 
        width = 11, 
        height = 6, scale = 300, 
        multicore = T, windowsize = c(1000, 800))
