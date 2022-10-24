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

df_temp %>% glimpse


## grafico teste
df_temp %>% 
  group_by(ano,mes) %>% 
  summarise(total_crimes = sum(total_crimes)) %>% 
  mutate(ano_mes = paste0(ano,sep="/",mes)) %>% 
  ggplot2::ggplot(aes(x=ano_mes, y=total_crimes, group=1))+
  ggplot2::geom_line()
  