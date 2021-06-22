output$grafico_mapa_proporcao <- renderLeaflet({
  if(input$group_regiao == 1){
    dataset <- dataset_anomalia_analise_ano_filtro()  
    
    posicao <- c(which(names(dataset) == "NOMEMUN"),which(names(dataset) == "prevalencia"),which(names(dataset) == "nascidos_vivos_anomalia"))
    
    names(dataset)[posicao[1]] = "Nome"
    names(dataset)[posicao[2]] = "variavel"
    names(dataset)[posicao[3]] = "nascidos_vivos_anomalia"
    
    pal <- colorBin("plasma", domain = dataset$variavel, bins = bins_defalt$brks)
    pal2 <- function(x){
      ifelse(x==0,"#808080",pal(x))
    }
    
    tidy <- dataset %>%
      left_join(mapa) 
    tidy = st_as_sf(tidy)
    tidy <- st_transform(tidy, "+init=epsg:4326")
    
    leaflet(tidy) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addPolygons(fillColor = ~pal2(variavel), 
                  weight = 1.5,
                  opacity = 0.7,
                  fillOpacity = 0.7,
                  color = "gray",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = sprintf("<strong>%s</strong><br/>Prevalência ao nascimento:
                                    %s</strong><br/>Número nascidos vivos: %s<br/>Casos anomalias: %s",
                                  tidy$Nome, round(tidy$variavel,3), tidy$numero_nascidos_vivos, 
                                  tidy$nascidos_vivos_anomalia) %>%
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "6px 11px"),
                    textsize = "13px",
                    opacity = 0.75,
                    direction = "bottom")) %>%
      leaflet::addLegend(pal = pal, values = ~tidy$variavel, opacity = 0.7, title = "Prevalência ao nascimento",
                         labFormat = labelFormat(digits = 3,big.mark = " "),
                         position = "bottomright") %>%
      addScaleBar(position = 'bottomleft')
    
  } else {
    banco_aux <- banco_anomalias_analise %>%
      filter(CODMUNRES %in% macro_saude_filtro())
    
    banco_aux2 <- banco_aux %>% 
      filter(ANO_NASC  == input$ano_grafico) %>%
      merge(.,banco_macro_saude,by.x=c("CODMUNRES"),by.y = c("IBGE"))
    
    dataset <- banco_aux2 %>%
      group_by(macro_cod,macrorregiao) %>%
      summarise(numero_nascidos_vivos = sum(numero_nascidos_vivos), nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia),
                prevalencia = nascidos_vivos_anomalia/numero_nascidos_vivos*10^4) %>%
      ungroup() 
    
    aux <- dataset
    names(aux)[2] = "Nome"
    names(aux)[4] = "variavel"
    names(aux)[1] = "cod"
    dataset <- aux
    limites <-
      c(round(min(dataset$variavel), 0) - 1, round(max(dataset$variavel), 0) +
          1)
    
    pal <- colorBin("plasma",domain = dataset$variavel, bins = seq(limites[1], limites[2], length.out = 6),alpha = 0.5)
    pal2 <- function(x) {
      ifelse(x == 0, "#808080", pal(x))
    }
    tidy <- dataset %>%
      left_join(macro_saude_shape,
                by = c("cod" = "macroregiao_num"))
    tidy = st_as_sf(tidy)
    
    tidy <- tidy %>%  st_transform("+init=epsg:4326")
    
    
    
    
    leaflet(tidy) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addPolygons(fillColor = ~pal2(variavel), 
                  weight = 1.5,
                  opacity = 0.7,
                  fillOpacity = 0.7,
                  color = "gray",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = sprintf("<strong>%s</strong><br/>Prevalência ao nascimento:
                                  %s</strong><br/>Número nascidos vivos: %s<br/>Número nascidos vivos com anomalia: %s",
                                  tidy$Nome, round(tidy$prevalencia,3), tidy$numero_nascidos_vivos, 
                                  tidy$variavel) %>%
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "6px 11px"),
                    textsize = "13px",
                    opacity = 0.75,
                    direction = "bottom")) %>%
      leaflet::addLegend(pal = pal, values = ~tidy$variavel, opacity = 0.7, title = "Prevalência ao nascimento",
                         labFormat = labelFormat(digits = 3,big.mark = " "),
                         position = "bottomright") %>%
      addScaleBar(position = 'bottomleft')
    
    
  }
  
})

###########################################################################################  
### MAPA LISA

output$mapa_lisa_prevalencia <- renderLeaflet({
  dataset <- dataset_anomalia_analise_ano() %>%
    select(NOMEMUN,municipio , prevalencia)  
  names(dataset)[3]=c("variavel")
  
  tidy <- dataset %>%
    right_join(mapa) 
  tidy = st_as_sf(tidy)
  tidy <- st_transform(tidy, "+init=epsg:4326") 
  
  ### LISA
  locm <- localmoran(tidy$variavel, matriz_w)
  tidy$Sgini <- scale(tidy$variavel)
  tidy$lag <- lag.listw(matriz_w, tidy$Sgini)
  tidy$pval <- locm[,5]
  
  tidy$quad_sig <-
    ifelse(
      tidy$Sgini >= 0 & tidy$lag >= 0 & tidy$pval <= 0.05,
      1,
      ifelse(
        tidy$Sgini <= 0 & tidy$lag <= 0 & tidy$pval <= 0.05,
        2,
        ifelse(
          tidy$Sgini >= 0 & tidy$lag <= 0 & tidy$pval <= 0.05,
          3,
          ifelse(tidy$Sgini >= 0 &
                   tidy$lag <= 0 & tidy$pval <= 0.05, 4, 5)
        )
      )
    )

  breaks <- seq(1, 5, 1)
  labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
  np <- findInterval(tidy$quad_sig, breaks)
  colors <- c("red", "blue", "lightpink", "skyblue2", "white")
  
  tidy$quad_sig <- ifelse(tidy$Sgini >= 0 & tidy$lag >= 0 & tidy$pval <= 0.05, 1, 
                          ifelse(tidy$Sgini <= 0 & tidy$lag <= 0 & tidy$pval <= 0.05, 2, 
                                 ifelse(tidy$Sgini >= 0 & tidy$lag <= 0 & tidy$pval <= 0.05, 3, 
                                        ifelse(tidy$Sgini >= 0 & tidy$lag <= 0 & tidy$pval <= 0.05, 4, 5))))
  
  tidy$quad_sig <- factor(
    tidy$quad_sig,
    levels = c(1, 2, 3, 4, 5),
    labels = c(
      "Alto-Alto",
      "Baixo-Baixo",
      "Alto-Baixo",
      "Baixo-Alto",
      "Nao Significativo"
    )
  )
  
  pal <-
    colorFactor(
      palette = c("red", "blue", "purple", "orange", "gray"),
      domain = tidy$quad_sig
    )
  y = tidy$quad_sig
  
  leaflet(tidy) %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
    addPolygons(
      fillColor = ~ pal(tidy$quad_sig),
      weight = 1.5,
      opacity = 1,
      fillOpacity = 0.7,
      color = "gray",
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = sprintf("<strong>%s</strong><br/> %s</strong><br/>
                        p-valor: %s",
                      tidy$NOMEMUN, tidy$quad_sig,  round(tidy$pval, 3)) %>%
        lapply(htmltools::HTML),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "6px 11px"),
        textsize = "13px",
        opacity = 0.75,
        direction = "bottom")) %>%
    leaflet::addLegend(
      pal = pal,
      values = ~ tidy$quad_sig,
      opacity = 0.7,
      title = NULL,
      labFormat = labelFormat(digits = 5,big.mark = " "),
      position = "bottomright"
    ) %>%
    addScaleBar(position = 'bottomleft')
})

output$prevalencia_barras <- renderPlotly({
  aux <- dataset_anomalia_analise_ano_filtro() %>%
    select(NOMEMUN, prevalencia) %>%
    arrange(prevalencia) %>%
    top_n(20, prevalencia)
  
  ordem <- aux$NOMEMUN
  names(aux)[1] = c("município")
  
  plot_barras <- ggplot(aux, aes(x = município, y = prevalencia)) +
    geom_col(fill = "darkmagenta", alpha = 1) +
    labs(x = "Município", y = "Prevalência ao nascimento") +
    scale_x_discrete(limits = ordem) +
    coord_flip()
  
  ggplotly(plot_barras)
})

output$prevalencia_serie <- renderPlotly({
  aux <- banco_anomalias_analise %>%
    filter(CODMUNRES %in% macro_saude_filtro()) %>%
    select(ANO_NASC, nascidos_vivos_anomalia, numero_nascidos_vivos) %>%
    group_by(ANO_NASC) %>%
    summarise(prevalencia = sum(nascidos_vivos_anomalia)/sum(numero_nascidos_vivos)*10^4)%>%
    arrange(ANO_NASC)

  aux$ANO_NASC = as.character(aux$ANO_NASC)
  aux=data.frame(aux)
  names(aux)[1] = "ano"
  
  plotar = ggplot(aux) +
    geom_point(aes(x = ano, y = prevalencia), color = "darkmagenta", alpha = 1) +
    geom_line(aes(x = ano, y = prevalencia, group = 1), color = "darkmagenta", alpha = 1) +
    labs(x = "Ano", y = "Média Prevalência ao nascimento") +
    ylim(min(aux$prevalencia)-5, max(aux$prevalencia)+5)+
    theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
  
  ggplotly(plotar)
})


output$boxplot_prevalencia <- renderPlotly({
  banco_aux <- banco_anomalias_analise %>%
    filter(CODMUNRES %in% macro_saude_filtro())
  banco_aux$ano <- as.character(banco_aux$ANO_NASC)
  
  banco_aux %>%
    plot_ly(
      x = ~ ano,
      y = ~ prevalencia,
      type = "box",
      marker = list(color = 'rgba(139,0,139,0.75)'),
      color = I("rgba(139,0,139,1)"),
      text = sprintf(
        "Cidade: %s <br>Prevalências ao nascimento: %s <br>Ano: %s",
        banco_aux$NOMEMUN,
        round(banco_aux$prevalencia, 2),
        banco_aux$ano
      )
    ) %>%
    layout(yaxis = list(range = input$limite_boxplot1))
})

output$prevalencia_hist <- renderPlotly({
  dataset_anomalia_analise_ano_filtro() %>%
    plot_ly(
      x = ~ prevalencia,
      type = "histogram",
      hovertemplate = "Intervalo (%{x}) <br>Frequencia: %{y}",
      nbinsx = 75,
      marker = list(color = 'rgba(139,0,139,0.6)')
    ) %>%
    layout(xaxis = list(range =   input$limite_hist_prevalencia),
           yaxis = list(c(0, 500)))
})



output$tabela_teste_moran = function(){
  
  tempor = round(teste_moran, 3) %>%
    filter(ano_teste ==input$ano_grafico) %>%
    select(1,2) 
  
  colnames(tempor) <- c('Valor da estatística de Teste', 'p-valor')
  rownames(tempor) <- c(input$ano_grafico)
  
  tempor %>%
    knitr::kable("html") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "responsive", 
                                        background = "withe", color = "black"),
                  font_size = 15)
}





output$tabela <- renderDataTable({
  
  dataset_tabela_prevalencia() %>%
    datatable(
      rownames = F,
      options = list(
        scrollX = TRUE,
        rowCallback = JS(rowCallback)
      )
    )%>%
    formatCurrency(6,' ', digits = 3, interval = 3, mark = ".", dec.mark = ",")
})


output$downloadData <- downloadHandler(
  contentType = "csv",
  filename = function() {
    paste("banco_anomalias_congenitas_rs_", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    banco_aux <- dataset_tabela_prevalencia()
    write.csv(banco_aux, file)
  }
)


output$plot_quadradinhos <- renderPlotly({
  cidades_banco_quadradinhos = banco_anomalias_analise %>%
    filter(ANO_NASC == 2019) %>%
    arrange(numero_nascidos_vivos) %>%
    filter(NOMEMUN %in% input$input_quadradinhos)  %>%
    select(CODMUNRES)
  
  banco_quadradinhos = banco_anomalias_analise %>%
    filter(CODMUNRES %in% cidades_banco_quadradinhos[[1]])
  
  quadradinho = ggplot(banco_quadradinhos,
                       aes(x = ANO_NASC, y = reorder(NOMEMUN, prevalencia, FUN = sum), fill = prevalencia)) +
    geom_raster(aes(
      text = sprintf(
        "Ano nascimento: %s<br>Cidade: %s<br>Prevalência ao nascimento: %s",
        ANO_NASC,
        NOMEMUN,
        round(prevalencia, 2)
      )
    )) +
    scale_fill_viridis_c(
      option = "A",
      breaks =  round(bins_defalt$brks, 2),
      name = "Prevalência"
    ) +
    labs(x = "ano nascimento", y = "cidade") +
    theme(axis.text.x = element_text(
      angle = 45,
      size = 9,
      vjust = 0.5
    )) +
    scale_x_continuous(breaks=2010:2019,labels=2010:2019)
  
  ggplotly(quadradinho, tooltip = "text")
})
