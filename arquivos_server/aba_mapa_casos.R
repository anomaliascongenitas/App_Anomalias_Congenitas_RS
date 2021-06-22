output$mapa_n_casos <- renderLeaflet({
  dataset <- dataset_anomalia_analise_casos_ano_filtro() %>%
    select(CODMUNRES,NOMEMUN,numero_nascidos_vivos,nascidos_vivos_anomalia,prevalencia,municipio)  
  names(dataset)[4]=c("variavel")
  
  pal <- colorBin("YlOrRd", domain = dataset$variavel, bins = bins_defalt_nascidos_vivos_anomalia$brks)

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
                                tidy$NOMEMUN, round(tidy$prevalencia,3), tidy$numero_nascidos_vivos, 
                                tidy$variavel) %>%
                  lapply(htmltools::HTML),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "6px 11px"),
                  textsize = "13px",
                  opacity = 0.75,
                  direction = "bottom")) %>%
    leaflet::addLegend(pal = pal, values = ~tidy$variavel, opacity = 0.7, 
                       title = "N nascidos vivos c/ anomalias",
                       labFormat = labelFormat(digits = 3,big.mark = " "),
                       position = "bottomright") %>%
    addScaleBar(position = 'bottomleft')
  
})


output$n_casos_barras <- renderPlotly({
  aux <- dataset_anomalia_analise_casos_ano_filtro() %>%
    filter(nascidos_vivos_anomalia > 0 ) %>%
    select(NOMEMUN, nascidos_vivos_anomalia) %>%
    arrange(nascidos_vivos_anomalia) %>%
    top_n(20, nascidos_vivos_anomalia)
  
  ordem <- aux$NOMEMUN
  names(aux)[1] = c("município")
  
  plot_barras <- ggplot(aux, aes(x = município, y = nascidos_vivos_anomalia)) +
    geom_col(fill = "red2", alpha = 1) +
    labs(x = "Município", y = "Prevalência ao nascimento") +
    scale_x_discrete(limits = ordem) +
    coord_flip()
  
  ggplotly(plot_barras)
  
})

output$n_casos_serie <- renderPlotly({
  aux <- banco_anomalias_analise %>%
    filter(CODMUNRES %in% macro_saude_filtro()) %>%
    select(ANO_NASC, nascidos_vivos_anomalia) %>%
    group_by(ANO_NASC) %>%
    summarise(total = sum(nascidos_vivos_anomalia))%>%
    arrange(ANO_NASC)
  
  aux$ANO_NASC = as.character(aux$ANO_NASC)
  aux=data.frame(aux)
  names(aux)[1] = "ano"
  
  plotar = ggplot(aux) +
    geom_point(aes(x = ano, y = total), color = "red2", alpha = 1) +
    geom_line(aes(x = ano, y = total, group = 1), color = "red2", alpha = 1) +
    labs(x = "Ano", y = "Número de nascidos vivos com anomalias congênitas") +
    ylim(min(aux$total)-5, max(aux$total)+5)+
    theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
  
  ggplotly(plotar)
  
})


output$plot_quadradinhos_casos <- renderPlotly({
  cidades_banco_quadradinhos = banco_anomalias_analise %>%
    filter(ANO_NASC == 2019) %>%
    arrange(numero_nascidos_vivos) %>%
    filter(NOMEMUN %in% input$input_quadradinhos_casos) %>%
    select(CODMUNRES)
  
  banco_quadradinhos = banco_anomalias_analise %>%
    filter(CODMUNRES %in% cidades_banco_quadradinhos[[1]])
  
  
  quadradinho=ggplot(banco_quadradinhos,aes(x=ANO_NASC, y=reorder(NOMEMUN, nascidos_vivos_anomalia, FUN = sum), fill=nascidos_vivos_anomalia))+
    geom_raster(aes(text=sprintf("Ano nascimento: %s<br>Cidade: %s<br>Número de casos: %s", ANO_NASC, NOMEMUN,nascidos_vivos_anomalia))) +
    scale_fill_viridis_c(option = "C", name= "N nasc c/ anomalia")+
    labs(x="",y="") +
    theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))+
    scale_x_continuous(breaks=2010:2019,labels=2010:2019)
  
  ggplotly(quadradinho,tooltip="text")
  
  
})
