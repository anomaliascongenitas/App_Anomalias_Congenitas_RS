


dataset_anomalia_analise_ano <- function(ano){
  banco_anomalias_analise %>%
    filter(ANO_NASC == ano)
}


datasetInputcid <- function(cid2){
  
  banco_aux <- banco_cid %>%
    filter(as.numeric(cid) %in% cid2) %>%
    group_by(ANO_NASC,CODMUNRES) %>%
    summarise(nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia),prevalencia = sum(nascidos_vivos_anomalia/unique(numero_nascidos_vivos))*10^4,NOMEMUN = unique(NOMEMUN))
  
  banco_aux2 <- banco_nascimentos %>%
    left_join(banco_aux,by=c("CODMUNRES","ANO_NASC")) 
  
  
  banco_aux2 <- banco_aux2 %>%
    replace_na(list(nascidos_vivos_anomalia = 0, prevalencia = 0)) %>%
    select(1,4,NOMEMUN = NOMEMUN.x,5,nascidos_vivos_anomalia = nascidos_vivos_anomalia, 7)
  banco_aux2
}



datasetInputcid_ano <- function(ano,cid){
  datasetInputcid(cid) %>%
    filter(ANO_NASC == ano)
}




datasetInputcid_min <- function(x){
  banco_aux <- banco_cid %>%
    group_by(ANO_NASC,cid) %>%
    summarise(nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia)) %>%
    ungroup()
  
  banco_aux$ANO_NASC = as.numeric(banco_aux$ANO_NASC)
  banco_aux2 <- spread(banco_aux, key = ANO_NASC, value = nascidos_vivos_anomalia)
  
  banco_aux2[,2:10] <- banco_aux2[,2:10]/matrix(rep(tabela_box$total_nascidos_vivos,each= 8),ncol = 9,byrow = F)*10^4
  banco_aux2
  
}

datasetInputcid_meso <- function(ano,cid){
  banco_aux2 <- datasetInputcid(cid) %>% 
    filter(ANO_NASC  == ano) %>%
    merge(.,base_mesoregiao,by.x=c("CODMUNRES"),by.y = c("IBGE"))
  
  banco_aux2 <- banco_aux2 %>%
    group_by(IBGE_meso,Nome_Mesoregiao) %>%
    summarise(numero_nascidos_vivos = sum(numero_nascidos_vivos), nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia),
              prevalencia = nascidos_vivos_anomalia/numero_nascidos_vivos*10^4) %>%
    ungroup() 
  banco_aux2
}


dataset_tipo_regiao_selecionada_ano <- function(regiao,ano){
  if(regiao == 1){
    aux <- dataset_anomalia_analise_ano(ano) %>%
      select(1,4,5,6,7,8)
    names(aux)[4] = "Nome"
    names(aux)[6] = "variavel"
    names(aux)[3] = "nascidos_vivos_anomalia"
  } else if(regiao == 2) {
    aux <- banco_meso_analise %>%
      filter(ANO_NASC == ano)
    names(aux)[2] = "Nome"
    names(aux)[6] = "variavel"
  }
  aux
}



teste <- dataset_anomalia_analise_ano(2018)


teste <- teste %>%
  filter(CODMUNRES %in% macro_saude_filtro) %>%
  select(NOMEMUN,prevalencia)

teste2 <- teste %>%
  slice_max(prevalencia,n = 20) %>%
  select(NOMEMUN)



mtcars %>% slice_min(mpg, n = 5)


library(devtools)
library(githubinstall)
install_github("brunoalanosilva/Projeto_Anomalias_Congenitas")
gh_update_package_list()
gh_suggest("AnomalyDetection")
gh_suggest("brunoalanosilva/Projeto_Anomalias_Congenitas")
gh_suggest_username("brunoalanosilva")







banco <- datasetInputcid(7) 



banco$ano <- as.factor(banco$ANO_NASC)

library(ggbeeswarm)



grafico2 <- ggplot(banco, aes(y = prevalencia, x = ano,fill = ano)) +
  geom_violin(position = position_dodge(width = 0.9)) +
  geom_point(position = position_jitterdodge(seed = 1, dodge.width = 0.9))

grafico <- ggplot(banco, aes(y = prevalencia, x = ano,fill = ano)) +
  geom_violin(position = position_dodge(width = 0.9)) +
  geom_quasirandom(dodge.width = 0.9, varwidth = TRUE) +
  scale_fill_manual(values = rep("darkmagenta",9))


grafico <- ggplot(banco, aes(y = prevalencia, x = ano,fill = ano)) +
  geom_violin(position = position_dodge(width = 0.9)) +
  geom_quasirandom(mapping = aes(
                                 text = sprintf(" %s <br>Prevalências ao nascimento: %s <br>Ano: %s",NOMEMUN,round(prevalencia,3),ano)
                                 ),
                   dodge.width = 0.3, varwidth = TRUE,width = 0.2) +
  #ylim( input$limite_dots_cid) +
  scale_fill_manual(values = rep("darkmagenta",9))


ggplotly(grafico,tooltip = "text")





ggplotly(grafico)
ggplotly(grafico2)



output$input_quadradinhos_html_cid <- renderUI({ 
  
  teste <- dataset_anomalia_analise_ano_filtro()  %>%
    select(NOMEMUN,numero_nascidos_vivos)
  
  teste2 <- teste %>%
    slice_max(numero_nascidos_vivos,n = 20) %>%
    select(NOMEMUN)
  
  
  selectizeInput("input_quadradinhos_cid",
                 label = "Escolha o(s) município(s)",
                 choices = unique(teste$NOMEMUN),
                 multiple = T,
                 options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
                 selected = teste2$NOMEMUN)
})


gerar_input_select <- function(x){
  teste <- dataset_anomalia_analise_ano_filtro()  %>%
    select(NOMEMUN,numero_nascidos_vivos)
  
  teste2 <- teste %>%
    slice_max(numero_nascidos_vivos,n = 20) %>%
    select(NOMEMUN)
  
  
  return(selectizeInput(x,
                 label = "Escolha o(s) município(s)",
                 choices = unique(teste$NOMEMUN),
                 multiple = T,
                 options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
                 selected = teste2$NOMEMUN))
}

