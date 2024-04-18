library(tidyverse)
library (dplyr)
options(scipen = 999)

dados <- readxl::read_excel("data/Base de dados.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(tema = str_to_title(tema),
         situacao_rec = str_to_lower(situacao) %>% 
                      str_extract("projeto|pec|proposta de emenda . constituição|proposta de emenda à constituição") %>% 
           str_remove_all(fixed("`")),
         situacao_rec = ifelse(!situacao_rec %in% c("projeto",
                                              "pec",
                                              "proposta de emenda a constituição",
                                              "proposta de emenda à constituição"), situacao, situacao_rec) %>% 
           str_to_title(),
         n_palavras = str_count(ementa),
         data_proposicao_dia_mes_ano = janitor::excel_numeric_to_date(dados$data_proposicao_dia_mes_ano %>% as.numeric())
         
         )
###correção pós-banca de defesa###
df_treated = dados  %>%  
  select (-c(data_nascimento, escolaridade)) %>%  #excluir colunas escolaridade e data de nascimento
  distinct(id,ementa, .keep_all = TRUE) %>% #excluir observação repetida (1)
  rename(gênero = sexo) %>% #renomear de sexo para gênero
  mutate(gênero = ifelse(gênero %in% c("Masculino", "MasculinoNão"), "Masculino", gênero)) #padronizar a resposta para o gênero "masculino"
##################################

aprovada <- c(".provada", "Projeto", "Proposta", "Pec")
rejeitada <- c(".rquivada", "Inconstitucional", "Prejudicialidade",
               "Rejeitada")
analise <- "An.lise"

expressoes <-tibble(categoria = c("aprovada", "rejeitada", "analise"),
       expressoes = c(
         str_flatten(str_glue("\\b{aprovada}\\b"), collapse = "|"),
         str_flatten(str_glue("\\b{rejeitada}\\b"), collapse = "|"),
         str_flatten(str_glue("\\b{analise}\\b"), collapse = "|")
                            )
                  )

#recategorizar as situação das emendas legislativas
df_treated2 <- df_treated %>% 
  rowwise() %>% 
  mutate(situacao_rec = expressoes %>% 
           select(1) %>% 
           filter(map_lgl(expressoes$expressoes,
                          ~str_detect(situacao_rec, .x))) %>% 
           unlist()) 


write_rds(df_treated2, "df_treated2.rds")


theme_set(hrbrthemes::theme_ipsum_es())
# Visualização dos temas

df_treated2 %>% 
  ggplot(aes(x =reorder(tema, -table(tema)[tema]))) +
  geom_bar()+
  geom_text(aes(label = ..count..), stat = "count",
            vjust = -.5)+
  hrbrthemes::theme_ipsum_es()+
  labs(x = "Tema da Proposição", y = NULL,
       title = "Tema da Proposições Legislativas")


#Temas por Região

df_treated2 %>% 
  drop_na(tema) %>% 
  ggplot(
    aes(
      x = reorder(regiao, -table(regiao)[regiao]),
        fill =tema  
         )
    ) +
  geom_bar(position = "dodge")+
  hrbrthemes::theme_ipsum_es()+
  labs(x = "Tema da Proposição", y = NULL,
       title = "Tema da Proposições Legislativas por Região")+
  scale_fill_viridis_d()+
  theme(legend.position = "bottom")

# Wordcloud das ementas legislativas
library(tm)

df_treated2 %>% 
  select(ementa) %>% 
  mutate(ementa = str_to_lower(ementa) %>% 
           abjutils::rm_accent() %>% 
           tm::removeNumbers() %>% 
           removePunctuation() %>% 
           removeWords(stopwords("portuguese"))) %>%
  separate_rows(ementa,sep = " ") %>% 
  filter(ementa != "") %>% 
  DocumentTermMatrix() %>% 
  as.matrix() %>% 
  colSums() %>% 
  sort(decreasing = TRUE) %>% 
  tibble(palavras = names(.),
         frequencia = .) %>% 
  wordcloud2::wordcloud2()

# Analise de sentimento ementas legislativas

df_treated2 %>% 
  mutate(ementa = str_to_lower(ementa) %>% 
           abjutils::rm_accent() %>% 
           tm::removeNumbers() %>% 
           removePunctuation() %>% 
           removeWords(stopwords("portuguese"))) %>%
  separate_rows(ementa,sep = " ") %>% 
  filter(ementa != "") %>% 
  select(id, ementa) %>% 
  #count(ementa, sort = TRUE) %>% 
  inner_join(lexiconPT::oplexicon_v3.0 %>% 
               select(term, polarity),
             by = c("ementa" = "term")) %>% 
  mutate(polarity = recode(polarity,
                           "-1" = "negativo",
                           "0" = "neutro",
                           "1" = "positivo")) %>% 
  ggplot(
    aes(
      x = polarity
    )
  )+
  geom_bar(aes(y = ((..count..)/sum(..count..))*100 ))+
  labs( x= "Sentimento", y = "%",
        title = "Sentimento das Palavras nas Ementas Legislativas")

# Situação dos Temas
df_treated2 %>% 
  ggplot(
    aes( x = reorder(situacao_rec, -table(situacao_rec)[situacao_rec]))
  )+
  geom_bar()+
  geom_text(aes(label = ..count..), stat = "count",
            vjust = -.5)+
  labs(x = "Situação da Proposta Legislativa",
       y= NULL,
       title = "Situação das Propostas Legislativas")

# Wortcloud Rejeição
df_treated2 %>% 
  select(motivo_rejeicao) %>% 
  mutate(ementa = str_to_lower(motivo_rejeicao) %>% 
           abjutils::rm_accent() %>% 
           tm::removeNumbers() %>% 
           removePunctuation() %>% 
           removeWords(stopwords("portuguese"))) %>%
  separate_rows(motivo_rejeicao,sep = " ") %>% 
  filter(motivo_rejeicao != "") %>% 
  DocumentTermMatrix() %>% 
  as.matrix() %>% 
  colSums() %>% 
  sort(decreasing = TRUE) %>% 
  tibble(palavras = names(.),
         frequencia = .) %>% 
  wordcloud2::wordcloud2()

# Sentimento da Rejeição dos Tema
df_treated2 %>% 
  mutate(ementa = str_to_lower(motivo_rejeicao) %>% 
           abjutils::rm_accent() %>% 
           tm::removeNumbers() %>% 
           removePunctuation() %>% 
           removeWords(stopwords("portuguese"))) %>%
  separate_rows(motivo_rejeicao,sep = " ") %>% 
  filter(motivo_rejeicao != "") %>% 
  select(id, motivo_rejeicao) %>% 
  #count(ementa, sort = TRUE) %>% 
  inner_join(lexiconPT::oplexicon_v3.0 %>% 
               select(term, polarity),
             by = c("motivo_rejeicao" = "term")) %>% 
  mutate(polarity = recode(polarity,
                           "-1" = "negativo",
                           "0" = "neutro",
                           "1" = "positivo")) %>% 
  ggplot(
    aes(
      x = polarity
    )
  )+
  geom_bar(aes(y = ((..count..)/sum(..count..))*100 ))+
  labs( x= "Sentimento", y = "%",
        title = "Sentimento das Palavras nas Rejeições das Ementas")

#Tema da ementa por rejeicao
df_treated2 %>% 
  filter(situacao_rec == "rejeitada") %>% 
  ggplot(
    aes( x= reorder(tema, -table(tema)[tema]))
  )+
  geom_bar()+
  geom_text(
    aes(label = ..count..),
    stat = "count",
    vjust = -.5
  )+
  labs(
    x = "Tema",
    y = NULL,
    title = "Temas Rejeitados"
  )

# Número de Palavras x Aceite/rejeição
df_treated2 %>% 
  group_by(situacao_rec) %>% 
  summarise(
    media = mean(n_palavras, na.rm = TRUE),
    dp = sd(n_palavras, na.rm = TRUE)
  ) %>% 
  mutate(dp = replace_na(dp, 0)) %>% 
  ggplot(
    aes(x = situacao_rec, y= media)
  )+
  geom_point()+
  geom_linerange(
    aes(ymin = media-dp,
        ymax = media+dp
    )
  )+
  labs(
    x = "Situação de Emenda",
    title = "Média de Palavras da Ementa por Situação"
  )

#N de palavras por data
df_treated2 %>% 
  mutate(
    ano_mes = zoo::as.yearmon(data_proposicao_dia_mes_ano)
  ) %>% 
  group_by(ano_mes) %>% 
  summarise(n_palavras = mean(n_palavras)) %>% 
  ggplot(aes( x = ano_mes,
              y = n_palavras))+
  geom_point()+
  geom_line()+
  zoo::scale_x_yearmon(n = 20)

# Palavras por tema

df_treated2 %>% 
  group_by(tema) %>% 
  summarise(n_palavras = mean(n_palavras, na.rm = TRUE)) %>% 
  drop_na() %>% 
  ggplot(
    aes(
      x = reorder(tema, -n_palavras),
      y = n_palavras
    )
  )+
  geom_col()+
  labs(x= NULL,
       title = "Número de Palavras por Tema")

#Apoios por Tema

df_treated2 %>%
  group_by(tema) %>% 
  summarise(n_apoios = mean(n_apoios, na.rm = TRUE)) %>% 
  drop_na() %>% 
  ggplot(
    aes(
      x = reorder(tema, -n_apoios),
      y = n_apoios
    )
  )+
  geom_col()+
  labs(x= NULL,
       title = "Número de Apoios por Tema")

## Ideias legislativas por gênero

df_filtered <- df_treated2 %>%
  filter(gênero %in% c("Masculino", "Feminino")) ##filtrando apenas respostas que se encaixam nas duas categorias/respostas válidas
df_filtered %>%
  ggplot(
    aes(
      x="",
      y="ementa", 
      fill=gênero,
    )
  )+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)+
theme_void()

##conflitos com direitos existentes

df_filtered2 <- df_treated2 %>%
  filter(ideia_conflitiva_com_outros_direitos %in% c("Sim", "Não"))

frequency_table <- table(df_treated2$ideia_conflitiva_com_outros_direitos)
print(frequency_table)

df_filtered2 %>%
  ggplot(
    aes(
      x="",
      y="ementa", 
      fill=ideia_conflitiva_com_outros_direitos,
    )
  )+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)+
  theme_void()

