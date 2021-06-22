
#### Preparando o filtro da macrorregiao de saude ########

macro_saude_filtro <- reactive({
  macro_saude_filtro2 <- banco_macro_saude %>%
    filter(macrorregiao %in% input$filtro_geral)
  macro_saude_filtro2$IBGE
})


########## Aba CID ##########

datasetInputcid <- reactive({
  banco_aux <- banco_cid %>%
    filter(cid_num %in% input$checkbox_cid) %>%
    group_by(NUMERODN) %>%
    summarise(ANO_NASC = unique(ANO_NASC),CODMUNRES = unique(CODMUNRES))
  
  banco_aux2 <- banco_aux %>%
    group_by(ANO_NASC,CODMUNRES) %>%
    summarise(nascidos_vivos_anomalia = n())
  
  
  banco_aux3 <- banco_nascimentos %>%
    left_join(banco_aux2,by  = c("CODMUNRES","ANO_NASC" = "ANO_NASC")) %>%
    mutate(nascidos_vivos_anomalia = replace_na(nascidos_vivos_anomalia, 0),prevalencia = nascidos_vivos_anomalia/numero_nascidos_vivos*10^4) %>%
    mutate(prevalencia = ifelse(is.nan(prevalencia),0,prevalencia)) 
  banco_aux3 %>%
    filter(CODMUNRES %in% macro_saude_filtro())
})


datasetInputcid_ano <- reactive({
  datasetInputcid() %>%
    filter(ANO_NASC == input$ano_grafico_cid) 
})



### usado para criar a tabela 1
datasetInputcid_min <- reactive({
  banco_aux <- banco_cid %>%
    filter(CODMUNRES %in% macro_saude_filtro()) %>%
    group_by(ANO_NASC,cid) %>%
    summarise(nascidos_vivos_anomalia = n()) %>%
    ungroup()
  
  banco_aux$ANO_NASC = as.numeric(banco_aux$ANO_NASC)
  banco_aux2 <- spread(banco_aux, key = ANO_NASC, value = nascidos_vivos_anomalia)
  
  banco_aux3 <- banco_anomalias_analise %>%
    filter(CODMUNRES %in% macro_saude_filtro()) %>%
    group_by(ANO_NASC) %>%
    summarise(total_nascidos_vivos = sum(numero_nascidos_vivos))
  
  
  banco_aux2[,2:11] <- banco_aux2[,2:11]/matrix(rep(banco_aux3$total_nascidos_vivos,each= 9),ncol = 10,byrow = F)*10^4
  banco_aux2
})


datasetInputcid_macro_saude <- reactive({
  banco_aux2 <- datasetInputcid_ano() %>% 
    #filter(ANO_NASC  == input$ano_grafico_cid) %>%
    merge(.,banco_macro_saude,by.x=c("CODMUNRES"),by.y = c("IBGE"))
  
  banco_aux2 <- banco_aux2 %>%
    group_by(macro_cod,macrorregiao) %>%
    summarise(numero_nascidos_vivos = sum(numero_nascidos_vivos), nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia),
              prevalencia = nascidos_vivos_anomalia/numero_nascidos_vivos*10^4) %>%
    ungroup() 
  banco_aux2
})



########## Aba prevalencia ##########

dataset_anomalia_analise_ano <- reactive({
  banco_anomalias_analise %>% 
    filter(ANO_NASC == input$ano_grafico)
})

dataset_anomalia_analise_ano_filtro <- reactive({
  dataset_anomalia_analise_ano() %>%
    filter(CODMUNRES %in% macro_saude_filtro())
})


dataset_tabela_prevalencia <- reactive({
  banco_aux <- banco_anomalias_analise %>%
    filter(CODMUNRES %in% macro_saude_filtro()) %>%
    select(CODMUNRES,NOMEMUN,ANO_NASC,numero_nascidos_vivos,nascidos_vivos_anomalia,prevalencia) %>%
    rename(ano_de_nascimento=ANO_NASC, nascidos_vivos_anomalia=nascidos_vivos_anomalia)
  
  banco_aux
})



########## Aba casos ##########

dataset_anomalia_analise_casos_ano <- reactive({
  banco_anomalias_analise %>% 
    filter(ANO_NASC == input$ano_grafico_casos)
})

dataset_anomalia_analise_casos_ano_filtro <- reactive({
  dataset_anomalia_analise_casos_ano() %>%
    filter(CODMUNRES %in% macro_saude_filtro())
})



########## Aba pop ##########

dataset_anomalia_analise_pop_ano_filtro <- reactive({
  banco_anomalias_analise %>% 
    filter(ANO_NASC == input$ano_grafico_pop, CODMUNRES %in% macro_saude_filtro())
})







