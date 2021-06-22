output$grafico_mapa_proporcao_cid <- renderLeaflet({
  if (length(input$checkbox_cid) != 0 ) {
    if (input$group_regiao_cid == 1) {
      dataset <- datasetInputcid_ano()
      
      dataset$municipio  <- str_to_lower(dataset$NOMEMUN)
      
      posicao <- which(names(dataset) == "prevalencia")
      
      names(dataset)[posicao] = c("variavel")
      
      ponto_central <- round(sum(dataset$nascidos_vivos_anomalia) / sum(dataset$numero_nascidos_vivos)*10^4,3)
      var_aux <- dataset$variavel[dataset$variavel > 0]
      primeira_parte_escala = seq(0 + 10 ^ -4, ponto_central, length.out = 4)
      segunda_parte_escala = ponto_central + (ponto_central - primeira_parte_escala[3]) * c(1, 3, 6, 10)
      terceira_parte_escala = max(var_aux)
      escala <- c(0, unique(
          c(primeira_parte_escala,
            segunda_parte_escala,
            terceira_parte_escala
          )
        ))
      pal <- colorBin("plasma", domain = dataset$variavel, bins = escala) ## usar na.color
      #"YlOrRd"
      pal2 <- function(x) {
        ifelse(x == 0, "#808080", pal(x))
      }
      tidy <- dataset %>%
        left_join(mapa)
      tidy = st_as_sf(tidy)
      tidy <- st_transform(tidy, "+init=epsg:4326") ##leaflet
      leaflet(tidy) %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
        addPolygons(
          fillColor = ~ pal2(variavel),
          weight = 1.5,
          opacity = 0.7,
          fillOpacity = 0.7,
          color = "gray",
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = sprintf(
            "<strong>%s</strong><br/>Prevalência ao nascimento:
                                  %s</strong><br/>Número nascidos vivos: %s<br/>Número nascidos vivos com anomalia: %s",
            tidy$NOMEMUN,
            round(tidy$variavel, 3),
            tidy$numero_nascidos_vivos,
            tidy$nascidos_vivos_anomalia
          ) %>%
            lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "6px 11px"),
            textsize = "13px",
            opacity = 0.75,
            direction = "bottom"
          )
        ) %>%
        leaflet::addLegend(
          pal = pal,
          values = ~ tidy$variavel,
          opacity = 0.7,
          title = "Prevalência ao nascimento",
          labFormat = labelFormat(digits = 3, big.mark = " "),
          position = "bottomright"
        )  %>%
        addScaleBar(position = 'bottomleft')
    } else{
      aux <- datasetInputcid_macro_saude()
      names(aux)[2] = "Nome"
      names(aux)[5] = "variavel"
      names(aux)[1] = "cod"
      dataset <- aux
      limites <-
        c(round(min(dataset$variavel), 0) - 1, round(max(dataset$variavel), 0) +
            1)
      
      pal <- colorBin("plasma",domain = dataset$variavel, bins = seq(limites[1], limites[2], length.out = 6))
      pal2 <- function(x) {
        ifelse(x == 0, "#808080", pal(x))
      }
      tidy <- dataset %>%
        left_join(macro_saude_shape,
                  by = c("cod" = "macroregiao_num"))
      tidy = st_as_sf(tidy)
      
      tidy <- tidy %>%  st_transform()
      
      leaflet(tidy) %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
        addPolygons(
          fillColor = ~pal(variavel),
          weight = 1.5,
          opacity = 0.7,
          fillOpacity = 0.7,
          color = "gray",
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            fillOpacity = 0.7,
            opacity = 0.7,
            bringToFront = TRUE
          ),
          label = sprintf(
            "<strong>%s</strong><br/>Prevalência ao nascimento:
                                  %s</strong><br/>Número nascidos vivos: %s<br/>Número nascidos vivos com anomalia: %s",
            tidy$Nome,
            round(tidy$variavel, 3),
            tidy$numero_nascidos_vivos,
            tidy$nascidos_vivos_anomalia
          ) %>%
            lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "6px 11px"),
            textsize = "13px",
            opacity = 0.7,
            direction = "bottom"
          )
        ) %>%
        leaflet::addLegend(
          pal = pal,
          values = ~ tidy$variavel,
          opacity = 0.7,
          title = "Prevalência ao nascimento",
          labFormat = labelFormat(digits = 3, big.mark = " "),
          position = "bottomright"
        ) %>%
        addScaleBar(position = 'bottomleft')
    }
  }
})

#######################
## Grafico de Barras 
## (colocar a explicacao)
#########################

output$grafico_barras_cid <- renderPlotly({
  if(length(input$checkbox_cid) != 0){
    aux <- datasetInputcid_ano() %>% ## aqui a macrorregiao e o ano é filtrada 
      group_by(CODMUNRES) %>%
      summarise(prevalencia = sum(nascidos_vivos_anomalia)/unique(numero_nascidos_vivos)*10^4, NOMEMUN = unique(NOMEMUN)) %>% 
      arrange(prevalencia) %>% 
      filter(prevalencia > 0) %>%
      top_n(20, prevalencia)
    
    ordem <- aux$NOMEMUN
    names(aux)[3] = c("municipio")
    
    plot_barras <- ggplot(aux, aes(x = municipio, y = prevalencia)) +
      geom_col(fill = "darkmagenta", alpha = 1) +
      labs(x = "Município", y = "Prevalência ao nascimento") +
      #labs(x = "Municipio", y = "Gráfico das 20 cidades com maiores valores de Prevalencia por 10000") +
      scale_x_discrete(limits = ordem) +
      #      axis.text.x = element_text(angle=90,size=7, vjust = 0.5)
      coord_flip()
    
    ggplotly(plot_barras)
  }
})

output$grafico_serie_cid <- renderPlotly({
  if(length(input$checkbox_cid) != 0){
    
    aux <- datasetInputcid() %>%
      group_by(ANO_NASC) %>%
      summarise(prevalencia = sum(nascidos_vivos_anomalia)/sum(numero_nascidos_vivos)*10^4) %>%
      arrange(ANO_NASC)
    
    aux$ANO_NASC = as.character(aux$ANO_NASC)
    aux=data.frame(aux)
    names(aux)[1] = "ano"
    
    plotar = ggplot(aux) +
      geom_point(aes(x = ano, y = prevalencia), color = "darkmagenta", alpha = 1) +
      geom_line(aes(x = ano, y = prevalencia, group = 1), color = "darkmagenta", alpha = 1) +
      labs(x = "Ano", y = "Prevalência ao nascimento") +
      ylim(min(aux$prevalencia)-5, max(aux$prevalencia)+5)+
      theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
    ggplotly(plotar)
    
  }
})

output$plot_dots_cid <-  renderPlotly({
  banco <- datasetInputcid()  %>%
    filter(CODMUNRES %in% macro_saude_filtro())
  
  banco$ano <- as.factor(banco$ANO_NASC)
  
  grafico <- ggplot(banco, aes(y = prevalencia, x = ano,fill = ano)) +
    geom_violin(position = position_dodge(width = 0.9),
                #,
                #text=sprintf(" %s <br>Prevalências ao nascimento: %s <br>Ano: %s", banco$NOMEMUN,round(banco$prevalencia,3),banco$ano)
                alpha = 1) +
    geom_quasirandom(dodge.width = 0.2, varwidth = TRUE,mapping = 
                       aes(text=sprintf(" %s <br>Prevalências ao nascimento: %s <br>Ano: %s", NOMEMUN,round(prevalencia,3),ano)), alpha = 1) +
    ylim( input$limite_dots_cid) +
    scale_fill_manual(values = rep("darkmagenta",10))+
    theme(legend.position = "none")  
  
  ggplotly(grafico,tooltip = "text")
  
})

output$plot_quadradinhos_cid <- renderPlotly({
  if(length(input$checkbox_cid) != 0){
    
    cidades_banco_quadradinhos = banco_nascimentos %>%
      filter(ANO_NASC == 2019,NOMEMUN %in% input$input_quadradinhos_cid) %>%
      #arrange(numero_nascidos_vivos) %>%
      select(CODMUNRES)
    
    
    banco_agrupado <- datasetInputcid() %>%
      filter(CODMUNRES %in% cidades_banco_quadradinhos[[1]]) %>%
      group_by(ANO_NASC,CODMUNRES) %>%
      summarise(prevalencia = sum(nascidos_vivos_anomalia)/unique(numero_nascidos_vivos)*10^4,nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia))
    
    banco_aux <- banco_nascimentos %>%
      filter(CODMUNRES %in% cidades_banco_quadradinhos[[1]]) %>%
      left_join(banco_agrupado, by = c("ANO_NASC","CODMUNRES")) 
    
    banco_aux$prevalencia[is.na(banco_aux$prevalencia)] = 0
    banco_aux$nascidos_vivos_anomalia[is.na(banco_aux$nascidos_vivos_anomalia)] = 0
    
    quadradinho=ggplot(banco_aux,aes(x=ANO_NASC, y=reorder(NOMEMUN, prevalencia, FUN = sum), fill=prevalencia))+
      geom_raster(aes(text=sprintf("Ano nascimento: %s<br>Cidade: %s<br>Prevalência ao nascimento: %s <br>Nº de nascidos vivos com anomalia: %s <br>Número de nascidos vivos: %s", ANO_NASC, NOMEMUN,round(prevalencia,2),nascidos_vivos_anomalia,numero_nascidos_vivos))) +
      scale_fill_viridis_c(option = "A", breaks =  round(bins_defalt$brks, 2), name= "Prevalência ao nascimento",alpha = 1)+
      labs(x="ano nascimento",y="cidade") +
      theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))+
      scale_x_continuous(breaks=2010:2019,labels=2010:2019)
    
    ggplotly(quadradinho,tooltip="text")
    
  }
  
})

output$plot_area_chart_cid <- renderPlotly({
  if(length(input$checkbox_cid) != 0){
    banco <- banco_cid %>%
      filter(cid_num %in% input$checkbox_cid, CODMUNRES %in% macro_saude_filtro()) %>%
      group_by(cid,ANO_NASC) %>%
      summarise(nascidos_vivos_anomalia = n())
    
    myLevels <- banco %>%
      group_by(cid) %>%
      summarise(nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia)) %>%
      arrange(nascidos_vivos_anomalia)
    
    banco$cid <- factor(banco$cid, levels=myLevels$cid )
    
    banco$ANO_NASC <- as.numeric(banco$ANO_NASC)
    
    
    banco <- banco %>%
      select(CID = cid,ano_nascimento = ANO_NASC,nascidos_vivos_anomalia = nascidos_vivos_anomalia)

    p <- ggplot(banco) + 
      geom_area(aes(x=ano_nascimento, y=nascidos_vivos_anomalia, fill=CID),alpha=1 , size=.5,colour="white") +
      scale_fill_viridis(discrete = T,direction = -1) +
      theme_ipsum() +
      labs(x="Ano nascimento",y="Número de Nascidos vivos com anomalia") +
      scale_x_continuous(breaks = 2010:2019,labels = 2010:2019)
    
    ggplotly(p)
  }
})


#######################
## Download da tabela
## Gerar arquivo para download da aba CID
#########################

output$downloadData_cid_1 <- downloadHandler(
  filename = function() {
    paste("banco_anomalias_congenitas_rs_cid_", Sys.Date(),".csv", sep="")
  },
  content = function(file) {
    write.csv( datasetInputcid_min() ,file)
  }
  
)
output$tabela_cid_1 <- renderDataTable({
  aux <- datasetInputcid_min()
  aux[,2:11] <- round(aux[,2:11],3)
  
  aux2 <- aux %>%
    datatable(
      rownames = F,
      # filter = "top",
      options = list(
        scrollX = TRUE,
        rowCallback = JS(rowCallback)
        #ordering = T
        #scrollY = "460px",
        #paging = FALSE
      )
    )%>%
    formatCurrency(2:11,' ', digits = 3, interval = 3, mark = "", dec.mark = ",")
  
    return(aux2)
})


output$downloadData_cid_2 <- downloadHandler(
  filename = function() {
    paste("banco_anomalias_congenitas_rs_grupo_cid_", Sys.Date(),".csv", sep="")
  },
  content = function(file) {
    write.csv( datasetInputcid() ,file)
  }
)

output$tabela_cid_2 <- renderDataTable({
  
  datasetInputcid() %>%
    datatable(
      rownames = F,
      # filter = "top",
      options = list(
        scrollX = TRUE,
        rowCallback = JS(rowCallback)
        #ordering = T
        #scrollY = "460px",
        #paging = FALSE
      )
    )%>%
    formatCurrency(6,' ', digits = 3, interval = 3, mark = "", dec.mark = ",")
})
