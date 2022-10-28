# Pacotes -----------------------------------------------------------------

library(basedosdados)
library(tidyverse)
library(lubridate)
library(writexl)

# Importando dados --------------------------------------------------------

## Importando dados via csv
df <- read_csv("dados/ocorrencias_registradas.csv")
df

# Para carregar o dado direto do portal basedosdados
query <- bdplyr("br_sp_gov_ssp.ocorrencias_registradas")
df <- bd_collect(query)
df

## Importando lat e log dos municipios
municipios <- read.csv2("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/main/csv/municipios.csv", sep = ",",encoding = "UTF-8")
municipios

# Manipulacao -------------------------------------------------------------

## visualiza df e suas variaveis
df %>% glimpse

## remove todos os NA e substitui por 0
df %>% 
  mutate_all(replace_na,0) %>% 
  rowSums(na.rm = TRUE)

## cria coluna total_homicidio (comecando pelo dataframe = df)
df_temp <- df %>% 
  rownames_to_column('id') %>% 
  gather(key, val, contains("homicidio")) %>% 
  group_by(id) %>% 
  mutate(total_homicidio = sum(val, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(key, val) %>% 
  select(everything(),-id)

## cria coluna total_lesao (identando pelo dataframe = df_temp)
df_temp <- df_temp %>% 
  rownames_to_column('id') %>% 
  gather(key, val, contains("lesao")) %>% 
  group_by(id) %>% 
  mutate(total_lesao = sum(val, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(key, val)%>% 
  select(everything(),-id)

## cria coluna latrocinio (identando pelo dataframe = df_temp)
df_temp <- df_temp %>% 
  rownames_to_column('id') %>% 
  gather(key, val, contains("latrocinio")) %>% 
  group_by(id) %>% 
  mutate(total_latrocinio = sum(val, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(key, val)%>% 
  select(everything(),-id)

## cria coluna estupro (identando pelo dataframe = df_temp)
df_temp <- df_temp %>% 
  rownames_to_column('id') %>% 
  gather(key, val, contains("estupro")) %>% 
  group_by(id) %>% 
  mutate(total_estupro = sum(val, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(key, val)%>% 
  select(everything(),-id)
    
## cria coluna roubo (identando pelo dataframe = df_temp)
df_temp <- df_temp %>% 
  rownames_to_column('id') %>% 
  gather(key, val, contains("roubo")) %>% 
  group_by(id) %>% 
  mutate(total_roubo = sum(val, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(key, val)%>% 
  select(everything(),-id)

## cria coluna furto (identando pelo dataframe = df_temp)
df_temp <- df_temp %>% 
  rownames_to_column('id') %>% 
  gather(key, val, contains("furto")) %>% 
  group_by(id) %>% 
  mutate(total_furto = sum(val, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(key, val)%>% 
  select(everything(),-id)

## seleciona somente colunas de interessa
df_temp <- df_temp %>% 
              select(ano,
                     mes,
                     id_municipio,
                     regiao_ssp,
                     starts_with("total_"),
                     -total_de_estupro,
                     -total_de_roubo_outros)

## aplica pivotagem na base de dados partindo dos crimes
df_temp <- df_temp %>% 
            tidyr::pivot_longer(
              cols = tidyselect::starts_with("total_"),
              names_to = "tipo_ocorrencia",
              values_to = "crimes")
df_temp$tipo_ocorrencia <- stringr::str_replace(df_temp$tipo_ocorrencia,pattern = "total_",replacement = "")
df_temp %>% View


## juntando o df_temp com a base de municipios (lat e lon)
municipios <- municipios %>% 
                  mutate(codigo_ibge = as.character(codigo_ibge)) %>% 
                  rename(id_municipio = codigo_ibge)
df_temp <- df_temp %>% 
  left_join(municipios,by = "id_municipio") %>% 
  select(1:9)

------------    
    
## criando coluna datatime ("somente se necessario")
df_temp2 <- df_temp2 %>% 
  mutate(dia = 1) %>% 
  mutate(data = lubridate::make_date(year = ano,month = mes,day = dia))
df_temp2$data[1] %>% class


# Visualização ------------------------------------------------------------

## paleta de cores
paleta.bruno <- colorRampPalette(c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494"))
paleta.bruno

## curva total de crimes por dia (labels agrupados por ano)
df_temp %>% 
  group_by(ano) %>%
  summarise(crimes = sum(crimes,na.rm = T)) %>%
  ggplot2::ggplot(aes(x=ano, y=crimes))+
  ggplot2::geom_line()+
  ggplot2::geom_point()+
  ggplot2::scale_x_continuous(n.breaks = 18)+
  ggplot2::scale_y_continuous(limits =  c(0,1800000),
                              labels = scales::number_format(accuracy = 1,
                                                             big.mark = "."))+
  ggplot2::theme_classic()


## curva de crimes por tipo_ocorrencia
df_temp %>% 
  group_by(ano,tipo_ocorrencia) %>%
  summarise(crimes = sum(crimes,na.rm = T)) %>%
  ggplot2::ggplot(aes(x=ano, y=crimes,color=tipo_ocorrencia))+
  ggplot2::geom_line(size=1.2)+
  ggplot2::geom_point(size=3)+
  # scale_color_continuous(colours = paleta.bruno(6))+
  ggplot2::scale_x_continuous(n.breaks = 18)+
  ggplot2::scale_y_continuous(expand = expansion(add = c(0,100000)),
                     labels = scales::number_format(accuracy = 1,
                                                    big.mark = "."))+
  ggplot2::theme_classic()
  

## curva de crimes por tipo_ocorrencia --- TESTE 2

df_temp %>% 
  filter(tipo_ocorrencia=="furto") %>% 
  group_by(ano,tipo_ocorrencia) %>%
  summarise(crimes = sum(crimes,na.rm = T),.groups = "keep") %>%
  ggplot2::ggplot(aes(x=ano, y=crimes))+
  ggplot2::geom_line(color="#41B6C4")+
  theme_classic()



## total de crimes por ano e por tipo_ocorrencia
df_temp %>% 
  group_by(ano,tipo_ocorrencia) %>% 
  summarise(crimes = sum(crimes)) %>% 
  ggplot2::ggplot(aes(x=ano, y=crimes,fill=tipo_ocorrencia))+
  ggplot2::geom_col()+
  ggplot2::theme_classic()+
  ggplot2::scale_x_continuous(n.breaks = 18)+
  ggplot2::scale_y_continuous(expand = expansion(add = c(0,100000)),
                              labels = scales::number_format(accuracy = 1,
                                                             big.mark = "."))+
  ggplot2::theme_classic()


## total de crimes por ano e por tipo_ocorrencia
df_temp %>% 
  group_by(ano,regiao_ssp) %>% 
  summarise(crimes = sum(crimes)) %>% 
  ggplot2::ggplot(aes(x=ano, y=crimes,fill=regiao_ssp))+
  ggplot2::geom_col(position = "fill")+
  ggplot2::theme_classic()+
  ggplot2::scale_x_continuous(n.breaks = 18)+
  ggplot2::theme_classic()



# Exportar ----------------------------------------------------------------

writexl::write_xlsx(df_temp,"df_temp.xlsx")
