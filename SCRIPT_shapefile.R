# Instalação e Carregamento dos Pacotes Necessários para a Aula -----------

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


# Carregando um shapefile -------------------------------------------------
shp_sp <- readOGR(dsn = "shapefile_sp", layer = "estado_sp")

shp_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# summarizando por municipio para o ano de 2021
df_sf <- df_temp %>% 
          filter(ano==2021) %>% 
          group_by(id_municipio) %>% 
          summarise(crimes = sum(crimes, na.rm = T))


# merge do shapefile com o dataframe
shp_dados_sp <- merge(x = shp_sp,
                      y = df_sf,
                      by.x = "CD_GEOCMU",
                      by.y = "id_municipio")

# visualizando dados
shp_dados_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)


# Salvando shapefile:
writeOGR(obj = shp_dados_sp, 
         layer = "novo_shapefile_seguranca_sp", 
         driver = "ESRI Shapefile", 
         dsn = "seguranca")


# Utilizando o ggplot2: ---------------------------------------------------

# Caso a intenção fosse a plotagem dos dados do dataset presente no objeto 
# shp_dados_sp, a lógica seria a mesma já aprendida no curso:
shp_dados_sp@data %>% 
  ggplot() +
  geom_histogram(aes(x = crimes),
                 fill = "deepskyblue4",
                 color = "white") +
  theme_bw()

# Passo 1: Transformar o shapefile num objeto do tipo data frame e, depois,
# importar os dados que já estavam no shapefile para o novo objeto data frame.
shp_dados_df <- tidy(shp_dados_sp, region = "CD_GEOCMU") %>% 
  rename(CD_GEOCMU = id) %>% 
  left_join(shp_dados_sp@data,
            by = "CD_GEOCMU")

#Passo 2: A plotagem.
shp_dados_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = crimes),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Crimes") +
  scale_fill_viridis_c() +
  theme_bw() ->g1


plotly::ggplotly(g1)

# Como saber quais paletas de cores podem ser utilizadas? -----------------
display.brewer.all()

paleta.bruno <- colorRampPalette(c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494"))
paleta.bruno

# Vamos reconstruir o último mapa, utilizando uma nova paleta de cor e propondo
# 4 variações de cores:
tm_shape(shp = shp_dados_sp) + 
  tm_fill(col = "crimes", 
          style = "quantile", 
          n = 6, 
          palette = paleta.bruno(6))+
  tm_borders(alpha = 0.8)


# Utilizando a tmap: ------------------------------------------------------
tm_shape(shp = shp_dados_sp) +
  tm_fill(col = "crimes", palette = "Blues")

tm_shape(shp = shp_dados_sp) +
  tm_dots(col = "crimes",size = 0.8)+
  tm_borders()
