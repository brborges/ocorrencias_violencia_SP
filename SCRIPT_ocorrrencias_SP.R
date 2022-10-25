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

## DATAFRAME MODELO 1
## cria coluna com total de crimes

df_temp <- df %>% 
  # adiciona um "id" com os nomes das linhas
  rownames_to_column('id') %>% 
  # empilha as colunas A até F. ao invés de -id poderia ser A:F
  gather(key, val, 6:28) %>% 
  # convert val em numeric
  mutate(val = as.numeric(val)) %>% 
  # agrupa pelo id
  group_by(id) %>%
  # cria soma total e proporcao (desconsiderando NAs)
  mutate(total_crimes = sum(val, na.rm = TRUE)) %>% 
  # desagrupa
  ungroup() %>% 
  # joga A:F nas colunas novamente
  spread(key, val) %>% 
  # reordena colunas e retira o "id"
  select(ano,mes,id_municipio, regiao_ssp,total_crimes,everything(),-id)
df_temp %>% View()

## criando coluna datatime
df_temp <- df_temp %>% 
  mutate(dia = 1) %>% 
  mutate(data = lubridate::make_date(year = ano,month = mes,day = dia))
df_temp$data[1] %>% class

## teste de agrupamento
df_temp %>%
  group_by(data) %>%
  summarise(total_crimes = sum(total_crimes)) %>% 
  filter(data == "2005-01-01")

## DATAFRAME MODELO 2
## aplica pivotagem na base de dados partindo dos crimes
df_temp2 <- df %>% 
              tidyr::pivot_longer(
                cols = (tidyselect::contains("homicidio")|
                        tidyselect::contains("lesao")|
                        tidyselect::contains("latrocinio")|
                        tidyselect::contains("estupro")|
                        tidyselect::contains("roubo")|
                        tidyselect::contains("furto")),
                names_to = "tipo_ocorrencia",
                values_to = "crimes")
df_temp2 %>% View

## criando coluna datatime (DATAFRAME MODELO 2)
df_temp2 <- df_temp2 %>% 
  mutate(dia = 1) %>% 
  mutate(data = lubridate::make_date(year = ano,month = mes,day = dia))
df_temp2$data[1] %>% class

#confere
df_temp %>% group_by(data) %>% filter(data == "2005-01-01") %>% summarise(n=sum(total_crimes))
df_temp2 %>% filter(ano == 2005 & mes==1) %>% summarise(n=sum(crimes,na.rm = T))

# Visualização ------------------------------------------------------------


## curva total de crimes por dia (labels agrupados por ano)
df_temp %>% 
  group_by(data) %>% 
  summarise(total_crimes = sum(total_crimes)) %>% 
  ggplot2::ggplot(aes(x=data, y=total_crimes,group=1))+
  ggplot2::geom_line()+
  ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggplot2::theme_classic()

## curva de crimes por tipo_ocorrencia
df_temp2 %>% 
  group_by(data,tipo_ocorrencia) %>%
  summarise(crimes = sum(crimes,na.rm = T)) %>%
  ggplot2::ggplot(aes(x=data, y=crimes,color=tipo_ocorrencia))+
  ggplot2::geom_line(show.legend = F)+
  ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ggplot2::theme_classic()
  

## total de crimes por ano
df_temp %>% 
  group_by(ano) %>% 
  summarise(total_crimes = sum(total_crimes)) %>% 
  ggplot2::ggplot(aes(x=ano, y=total_crimes))+
  ggplot2::geom_col()+
  ggplot2::theme_classic()
