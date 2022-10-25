# Pacotes -----------------------------------------------------------------

library(basedosdados)
library(tidyverse)
library(lubridate)

# Importando dados --------------------------------------------------------

## Importando dados via csv
df <- read_csv("dados/ocorrencias_registradas.csv")
df

# Para carregar o dado direto do portal basedosdados
query <- bdplyr("br_sp_gov_ssp.ocorrencias_registradas")
df <- bd_collect(query)
df

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

------------    
    
## criando coluna datatime ("somente se necessario")
df_temp2 <- df_temp2 %>% 
  mutate(dia = 1) %>% 
  mutate(data = lubridate::make_date(year = ano,month = mes,day = dia))
df_temp2$data[1] %>% class


# Visualização ------------------------------------------------------------


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
  ggplot2::geom_line()+
  ggplot2::geom_point()+
  ggplot2::scale_x_continuous(n.breaks = 18)+
  ggplot2::scale_y_continuous(expand = expansion(add = c(0,100000)),
                     labels = scales::number_format(accuracy = 1,
                                                    big.mark = "."))+
  ggplot2::theme_classic()
  

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

