

output$input_quadradinhos_html_cidade2 <- renderUI({ 
  
  teste <- dataset_anomalia_analise_ano_filtro()  %>%
    select(NOMEMUN,numero_nascidos_vivos)
  
  teste2 <- teste %>%
    slice_max(numero_nascidos_vivos,n = 1) %>%
    select(NOMEMUN)
  
  
  selectizeInput("cidade2",
                 label = "Escolha o(s) município(s)",
                 choices = unique(teste$NOMEMUN),
                 multiple = T,
                 options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
                 selected = teste2$NOMEMUN)
})


output$grafico_serie_casos <- renderPlotly({
  serie <- banco_anomalias_analise %>%
    filter(NOMEMUN %in% input$cidade2) 
  
  ggplotly(
    ggplot(serie, aes(x = ANO_NASC , y = nascidos_vivos_anomalia, colour = NOMEMUN)) +
      geom_line() +
      geom_point(size = 2,aes(text=sprintf("Ano nascimento: %s<br>Cidade: %s<br>Número de nascidos vivos com anomalia: %s", ANO_NASC, NOMEMUN,round(nascidos_vivos_anomalia,2))))+
      ylim(input$limite2[1],input$limite2[2])+
      labs(x = "Ano", y = "Nascidos vivos com anomalias congenitas") +labs(color = "Municípios") +
      scale_x_continuous(breaks=2010:2019,labels=2010:2019),
    tooltip="text"
  )
})
