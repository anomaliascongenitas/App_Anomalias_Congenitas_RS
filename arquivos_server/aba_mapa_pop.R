output$mapa_pop <- renderLeaflet({
  
  dataset <- dataset_anomalia_analise_pop_ano_filtro() %>%
    select(CODMUNRES,NOMEMUN,numero_nascidos_vivos,nascidos_vivos_anomalia,prevalencia,municipio)  
  names(dataset)[3]=c("variavel")
  
  max(banco_anomalias_analise$numero_nascidos_vivos)
  
  
  pal <- colorBin("plasma", domain = dataset$variavel, bins = (exp(seq(0,10,2))-1))
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
                                tidy$NOMEMUN, round(tidy$prevalencia,3), tidy$variavel, 
                                tidy$nascidos_vivos_anomalia) %>%
                  lapply(htmltools::HTML),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "6px 11px"),
                  textsize = "13px",
                  direction = "bottom")) %>%
    leaflet::addLegend(pal = pal, values = ~tidy$variavel, opacity = 0.7, title = "Número de nascimentos",
                       labFormat = labelFormat(digits = 3,big.mark = " "),
                       position = "bottomright") %>%
    addScaleBar(position = 'bottomleft')
  
  
})

output$pop_barras <- renderPlotly({
  aux <- dataset_anomalia_analise_pop_ano_filtro() %>%
    select(NOMEMUN, numero_nascidos_vivos) %>%
    arrange(numero_nascidos_vivos) %>%
    top_n(20, numero_nascidos_vivos)
  
  ordem <- aux$NOMEMUN
  names(aux)[1] = c("município")
  
  plot_barras <- ggplot(aux, aes(x = município, y = numero_nascidos_vivos)) +
    geom_col(fill = "red2", alpha = 1) +
    labs(x = "Município", y = "numero_nascidos_vivos") +
    scale_x_discrete(limits = ordem) +
    coord_flip()
  
  ggplotly(plot_barras)
  
})


output$pop_serie <- renderPlotly({
  aux <- banco_anomalias_analise %>%
    filter(CODMUNRES %in% macro_saude_filtro()) %>%
    select(ANO_NASC, numero_nascidos_vivos) %>%
    group_by(ANO_NASC) %>%
    summarise(total = sum(numero_nascidos_vivos))%>%
    arrange(ANO_NASC)
  
  aux$ANO_NASC = as.character(aux$ANO_NASC)
  aux=data.frame(aux)
  names(aux)[1] = "ano"
  
  plotar = ggplot(aux) +
    geom_point(aes(x = ano, y = total), color = "red2", alpha = 1) +
    geom_line(aes(x = ano, y = total, group = 1), color = "red2", alpha = 1) +
    labs(x = "Ano", y = "Número de nascidos vivos") +
    ylim(min(aux$total)-5, max(aux$total)+5)+
    theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
  
  ggplotly(plotar)
})

output$plot_quadradinhos_pop <- renderPlotly({
  cidades_banco_quadradinhos = banco_anomalias_analise %>%
    filter(ANO_NASC == 2019) %>%
    filter(NOMEMUN %in% input$input_quadradinhos_pop) %>%
    select(CODMUNRES)
  
  banco_quadradinhos = banco_anomalias_analise %>%
    filter(CODMUNRES %in% cidades_banco_quadradinhos[[1]])
  
  quadradinho=ggplot(banco_quadradinhos,aes(x=ANO_NASC, y=reorder(NOMEMUN, numero_nascidos_vivos, FUN = sum), fill=numero_nascidos_vivos))+
    geom_raster(aes(text=sprintf("Ano nascimento: %s<br>Cidade: %s<br>Número nascidos vivos: %s", ANO_NASC, NOMEMUN,numero_nascidos_vivos))) +
    scale_fill_viridis_c(option = "C", name= "Número de nascimentos")+
    labs(x="",y="") +
    theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))+
    scale_x_continuous(breaks=2010:2019,labels=2010:2019)
  
  ggplotly(quadradinho,tooltip="text")
})
