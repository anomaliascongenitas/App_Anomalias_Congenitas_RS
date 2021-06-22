output$input_quadradinhos_html_cidade1 <- renderUI({ 
  
  teste <- dataset_anomalia_analise_ano_filtro()  %>%
    select(NOMEMUN,numero_nascidos_vivos)
  
  teste2 <- teste %>%
    slice_max(numero_nascidos_vivos,n = 1) %>%
    select(NOMEMUN)
  
  
  selectizeInput("cidade1",
                 label = "Escolha o(s) município(s)",
                 choices = unique(teste$NOMEMUN),
                 multiple = T,
                 options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
                 selected = teste2$NOMEMUN)
})

output$grafico_serie_prevalencia <- renderPlotly({
  serie2 <- banco_anomalias_analise %>%
    filter(NOMEMUN %in% input$cidade1) 
  
  ggplotly(
    ggplot(serie2, aes(x = ANO_NASC , y = prevalencia, colour = NOMEMUN)) +
      geom_line() +
      geom_point(size = 2,aes(text=sprintf("Ano nascimento: %s<br>Cidade: %s<br>Prevalência ao nascimento: %s", ANO_NASC, NOMEMUN,round(prevalencia,2))))+
      ylim(input$limite1[1],input$limite1[2])+
      labs(x = "Ano", y = "Prevalência ao nascimento de anomalias congenitas")+labs(color = "Municípios")+
      scale_x_continuous(breaks=2010:2019,labels=2010:2019),
    tooltip="text"
  )
})
