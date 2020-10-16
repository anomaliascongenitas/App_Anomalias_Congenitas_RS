
server <- function(input, output,session) {
    
     
    
  #callModule(profvis_server, "profiler")
  
  ###### Base de dados reativas ############################
  
  
  
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
  
  
  output$input_quadradinhos_html <- renderUI({ 
    
    teste <- dataset_anomalia_analise_ano_filtro()  %>%
      select(NOMEMUN,numero_nascidos_vivos)
    
    teste2 <- teste %>%
      slice_max(numero_nascidos_vivos,n = 20) %>%
      select(NOMEMUN)
    
    
    selectizeInput("input_quadradinhos",
                   label = "Escolha o(s) município(s)",
                   choices = unique(teste$NOMEMUN),
                   multiple = T,
                   options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
                   selected = teste2$NOMEMUN)
  })

  
  output$input_quadradinhos_html_casos <- renderUI({ 
    
    teste <- dataset_anomalia_analise_casos_ano_filtro()  %>%
      select(NOMEMUN,numero_nascidos_vivos)
    
    teste2 <- teste %>%
      slice_max(numero_nascidos_vivos,n = 20) %>%
      select(NOMEMUN)
    
    
    selectizeInput("input_quadradinhos_casos",
                   label = "Escolha o(s) município(s)",
                   choices = unique(teste$NOMEMUN),
                   multiple = T,
                   options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
                   selected = teste2$NOMEMUN)
  })
  
  
  output$input_quadradinhos_html_pop <- renderUI({ 
    
    teste <- dataset_anomalia_analise_pop_ano_filtro()  %>%
      select(NOMEMUN,numero_nascidos_vivos)
    
    teste2 <- teste %>%
      slice_max(numero_nascidos_vivos,n = 20) %>%
      select(NOMEMUN)
    
    
    selectizeInput("input_quadradinhos_pop",
                   label = "Escolha o(s) município(s)",
                   choices = unique(teste$NOMEMUN),
                   multiple = T,
                   options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
                   selected = teste2$NOMEMUN)
  })
  
  output$input_quadradinhos_html_cid <- renderUI({ 
    
    teste <- datasetInputcid_ano()  %>%
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
  
  
  
  
  
  
  
  
  dataset_anomalia_analise_ano <- reactive({
    banco_anomalias_analise %>% 
      filter(ANO_NASC == input$ano_grafico)
  })
  
  macro_saude_filtro <- reactive({
    macro_saude_filtro2 <- banco_macro_saude %>%
      filter(macrorregiao %in% input$filtro_geral)
    macro_saude_filtro2$IBGE
  })
  
  
  
  dataset_anomalia_analise_ano_filtro <- reactive({
    dataset_anomalia_analise_ano() %>%
      filter(CODMUNRES %in% macro_saude_filtro())
  })
  
  dataset_anomalia_analise_casos_ano <- reactive({
    banco_anomalias_analise %>% 
      filter(ANO_NASC == input$ano_grafico_casos)
  })
  
  dataset_anomalia_analise_casos_ano_filtro <- reactive({
    dataset_anomalia_analise_casos_ano() %>%
      filter(CODMUNRES %in% macro_saude_filtro())
  })

  dataset_anomalia_analise_pop_ano_filtro <- reactive({
    banco_anomalias_analise %>% 
      filter(ANO_NASC == input$ano_grafico_pop, CODMUNRES %in% macro_saude_filtro())
  })
  
  
  

  datasetInputcid <- reactive({
    
    banco_aux <- banco_cid %>%
      filter(as.numeric(cid) %in% input$checkbox_cid) %>%
      group_by(ANO_NASC,CODMUNRES) %>%
      summarise(nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia),prevalencia = sum(nascidos_vivos_anomalia/unique(numero_nascidos_vivos))*10^4,NOMEMUN = unique(NOMEMUN))
    
    banco_aux2 <- banco_nascimentos %>%
      left_join(banco_aux,by=c("CODMUNRES","ANO_NASC")) 
    
    
    banco_aux2 <- banco_aux2 %>%
      replace_na(list(nascidos_vivos_anomalia = 0, prevalencia = 0)) %>%
      select(1,4,NOMEMUN = NOMEMUN.x,5,nascidos_vivos_anomalia = nascidos_vivos_anomalia, 7) %>%
      filter(CODMUNRES %in% macro_saude_filtro())
    
  })
  

  
  
  datasetInputcid_ano <- reactive({
    datasetInputcid() %>%
      filter(ANO_NASC == input$ano_grafico_cid) 
  })
  
  
  
  
  datasetInputcid_min <- reactive({
    banco_aux <- banco_cid %>%
      filter(CODMUNRES %in% macro_saude_filtro()) %>%
      group_by(ANO_NASC,cid) %>%
      summarise(nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia)) %>%
      ungroup()
    
    banco_aux$ANO_NASC = as.numeric(banco_aux$ANO_NASC)
    banco_aux2 <- spread(banco_aux, key = ANO_NASC, value = nascidos_vivos_anomalia)
    
    banco_aux3 <- banco_anomalias_analise %>%
      filter(CODMUNRES %in% macro_saude_filtro()) %>%
      group_by(ANO_NASC) %>%
      summarise(total_nascidos_vivos = sum(numero_nascidos_vivos))
    
    
    banco_aux2[,2:10] <- banco_aux2[,2:10]/matrix(rep(banco_aux3$total_nascidos_vivos,each= 8),ncol = 9,byrow = F)*10^4
    banco_aux2
    
    
  })
  
  datasetInputcid_meso <- reactive({
    banco_aux2 <- datasetInputcid() %>% 
      filter(ANO_NASC  == input$ano_grafico_cid) %>%
      merge(.,base_mesoregiao,by.x=c("CODMUNRES"),by.y = c("IBGE"))
    
    banco_aux2 <- banco_aux2 %>%
      group_by(IBGE_meso,Nome_Mesoregiao) %>%
      summarise(numero_nascidos_vivos = sum(numero_nascidos_vivos), nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia),
                prevalencia = nascidos_vivos_anomalia/numero_nascidos_vivos*10^4) %>%
      ungroup() 
    banco_aux2
  })
  
  
  
  datasetInputcid_macro_saude <- reactive({
    banco_aux2 <- datasetInputcid() %>% 
      filter(ANO_NASC  == input$ano_grafico_cid) %>%
      merge(.,banco_macro_saude,by.x=c("CODMUNRES"),by.y = c("IBGE"))
    
    banco_aux2 <- banco_aux2 %>%
      group_by(macrorregiao) %>%
      summarise(numero_nascidos_vivos = sum(numero_nascidos_vivos), nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia),
                prevalencia = nascidos_vivos_anomalia/numero_nascidos_vivos*10^4) %>%
      ungroup() 
    banco_aux2
  })
  
  
  dataset_tipo_regiao_selecionada_ano <- reactive({
    if(input$group_regiao == 1){
      aux <- dataset_anomalia_analise_ano_filtro()  %>%
        select(1,4,5,6,7,8)
      names(aux)[4] = "Nome"
      names(aux)[6] = "variavel"
      names(aux)[3] = "nascidos_vivos_anomalia"
    } else if(input$group_regiao == 2) {
      aux <- banco_meso_analise %>%
        filter(ANO_NASC == input$ano_grafico,Nome_Mesoregiao %in% input$filtro_geral)
      names(aux)[2] = "Nome"
      names(aux)[6] = "variavel"
    }
    aux
  })
  

  dataset_box_filtro <- reactive({
    vetor = c()
    vetor[1] = sum(dataset_anomalia_analise_ano_filtro()$numero_nascidos_vivos)
    vetor[2] = sum(dataset_anomalia_analise_ano_filtro()$nascidos_vivos_anomalia)
    vetor[3] = ifelse(vetor[1] != 0 ,round((vetor[2]/vetor[1])*10^4,3),0)
    vetor
  })
  
  
  
  ################ Box ###########################
  
  
    output$box_populacao <- renderValueBox({
        valueBox(
          dataset_box_filtro()[1],
            "Total nascidos vivos",
            icon = icon("baby",lib = "font-awesome"),
            color = "blue"
        )
    })
    
    
    output$box_numero_casos <- renderValueBox({
        valueBox(
          dataset_box_filtro()[2],
            "Total nascidos vivos com anomalias congenitas",
            icon = icon("notes-medical"),
            color = "red"
        )
    })
    
    
    output$box_prevalencia <- renderValueBox({
      valueBox(
            dataset_box_filtro()[3],
            "Prevalência ao nascimento no RS por 10000",
            icon = icon("notes-medical"),
            color = "purple"
        )
    })
    
    
    
    
    
    
    ############### CAIXAS APLICATIVO aba nascidos vivos com anomalia ###################################  
    
    
    output$box_populacao_casos <- renderValueBox({
        valueBox(
          dataset_box_filtro()[1],
            "Total nascidos vivos",
            icon = icon("baby",lib = "font-awesome"),
            color = "blue"
        )
    })
    
    output$box_numero_casos_casos <- renderValueBox({
        valueBox(
          dataset_box_filtro()[2],
            "Total nascidos vivos com anomalias congenitas",
            icon = icon("notes-medical"),
            color = "red"
        )
    })
    
    output$box_prevalencia_casos <- renderValueBox({
        valueBox(
          dataset_box_filtro()[3],
            "Prevalência ao nascimento no RS por 10000",
            icon = icon("notes-medical"),
            color = "purple"
        )
    })
    
    
    ############### CAIXAS APLICATIVO aba nascidos vivos  ###################################  
    
    
    output$box_populacao_pop <- renderValueBox({
        valueBox(
          dataset_box_filtro()[1],
            "Total nascidos vivos",
          icon = icon("baby",lib = "font-awesome"),
            color = "blue"
        )
    })
    
    output$box_numero_casos_pop <- renderValueBox({
        valueBox(
          dataset_box_filtro()[2],
            "Total nascidos vivos com anomalias congenitas",
            icon = icon("notes-medical"),
            color = "red"
        )
    })
    
    output$box_prevalencia_pop <- renderValueBox({
        valueBox(
          dataset_box_filtro()[3],
            "Prevalência ao nascimento no RS por 10000",
            icon = icon("notes-medical"),
            color = "purple"
        )
    })
    
    
    
    

    
    
    ###################### Box aba cid ####################
    
    output$box_populacao_cid <- renderValueBox({
      banco_aux <- dataset_anomalia_analise_ano_filtro()
      valueBox(
        sum(banco_aux$numero_nascidos_vivos),
        "Total nascidos vivos",
        icon = icon("baby",lib = "font-awesome"),
        color = "blue"
      )
    })
    
    
    output$box_numero_casos_cid <- renderValueBox({
      # banco_aux <- banco_cid %>% 
      #  filter(as.numeric(cid) %in% input$checkbox_cid, ANO_NASC == input$ano_grafico_cid,CODMUNRES %in% macro_saude_filtro())
      
      valueBox(
        sum(datasetInputcid_ano()$nascidos_vivos_anomalia),
        "Total nascidos vivos com anomalias congenitas",
        icon = icon("notes-medical",lib = "font-awesome"),
        color = "red"
      )
    })
    
    output$box_prevalencia_cid <- renderValueBox({
      banco_aux <- dataset_anomalia_analise_ano_filtro()
      
      valueBox(
        round(sum(datasetInputcid_ano()$nascidos_vivos_anomalia)/sum(banco_aux$numero_nascidos_vivos)*10^4,3),
        "Prevalência ao nascimento no RS por 10000",
        icon = icon("notes-medical"),
        color = "purple"
      )
    })
    
    
    ########## MAPA APLICATIVOS  
    
    ###########################################################################################  
    
    
    ####### Mapas e graficos com o CID ########################
    
    output$grafico_mapa_proporcao_cid <- renderLeaflet({
      
      if(length(input$checkbox_cid) != 0){
        
        if(input$group_regiao_cid == 1){
          dataset <- datasetInputcid_ano() 
          
          dataset$municipio  <- str_to_lower(dataset$NOMEMUN)
          
          
          names(dataset)[6]=c("variavel")
          
          pal <- colorBin("plasma", domain = dataset$variavel, bins = bins_defalt$brks)
          
          pal2 <- function(x){
            ifelse(x==0,"#808080",pal(x))
          }
          #########################################################################################
          #### MAPA  
          #########################################################################################
          tidy <- dataset %>%
            left_join(mapa_rs) 
          tidy = st_as_sf(tidy)
          tidy <- st_transform(tidy, "+init=epsg:4326") ##leaflet
          
          
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
                                        tidy$NOMEMUN, round(tidy$variavel,3), tidy$numero_nascidos_vivos, 
                                        tidy$nascidos_vivos_anomalia) %>%
                          lapply(htmltools::HTML),
                        labelOptions = labelOptions(
                          style = list("font-weight" = "normal", padding = "6px 11px"),
                          textsize = "13px",
                          direction = "bottom")) %>%
            leaflet::addLegend(pal = pal, values = ~tidy$variavel, opacity = 0.7, title = "Prevalência ao nascimento",
                               labFormat = labelFormat(digits = 3),
                               position = "bottomright")
        } else if(input$group_regiao_cid == 2){
          
          dataset <- datasetInputcid_meso()
          
          
          names(dataset)[5] = "variavel"
          
          
          
          
          limites <- c(round(min(dataset$variavel),0)-1,round(max(dataset$variavel),0)+1)
          
          pal <- colorBin("plasma", domain = dataset$variavel, bins = seq(limites[1],limites[2],length.out = 6))
          
          pal2 <- function(x){
            ifelse(x==0,"#808080",pal(x))
          }
          #########################################################################################
          #### MAPA  
          #########################################################################################
          tidy <- dataset %>%
            merge(mapa_rs_meso,by.x = c("Nome_Mesoregiao"), by.y = c("NM_MESO"))  
          tidy = st_as_sf(tidy)
          tidy <- st_transform(tidy, "+init=epsg:4326") ##leaflet
          
          
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
                                        tidy$Nome_Mesoregiao, round(tidy$variavel,3), tidy$numero_nascidos_vivos, 
                                        tidy$nascidos_vivos_anomalia) %>%
                          lapply(htmltools::HTML),
                        labelOptions = labelOptions(
                          style = list("font-weight" = "normal", padding = "6px 11px"),
                          textsize = "13px",
                          direction = "bottom")) %>%
            leaflet::addLegend(pal = pal, values = ~tidy$variavel, opacity = 0.7, title = "Prevalência ao nascimento",
                               labFormat = labelFormat(digits = 3),
                               position = "bottomright")
          
        } else{
          aux <- datasetInputcid_macro_saude() 
          names(aux)[1] = "Nome"
          names(aux)[4] = "variavel"
          dataset <- aux
          limites <- c(round(min(dataset$variavel),0)-1,round(max(dataset$variavel),0)+1)
          
          pal <- colorBin("plasma", domain = dataset$variavel, bins = seq(limites[1],limites[2],length.out = 6))
          
          pal2 <- function(x){
            ifelse(x==0,"#808080",pal(x))
          }
          #########################################################################################
          #### MAPA  
          #########################################################################################
          tidy <- dataset %>%
            merge(macro_saude_shape,by.x = c("Nome"), by.y = c("macroregiao"))  
          tidy = st_as_sf(tidy)
          tidy <- st_transform(tidy, "+init=epsg:4326") ##leaflet
          
          
          leaflet(tidy) %>%
            addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
            addPolygons(fillColor = ~pal(variavel), 
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
                                        tidy$Nome, round(tidy$variavel,3), tidy$numero_nascidos_vivos, 
                                        tidy$nascidos_vivos_anomalia) %>%
                          lapply(htmltools::HTML),
                        labelOptions = labelOptions(
                          style = list("font-weight" = "normal", padding = "6px 11px"),
                          textsize = "13px",
                          direction = "bottom")) %>%
            leaflet::addLegend(pal = pal, values = ~tidy$variavel, opacity = 0.7, title = "Prevalência ao nascimento",
                               labFormat = labelFormat(digits = 3),
                               position = "bottomright")
          
        }  
        
        
        
      }
    })
    
    
    
    
    
    
    output$grafico_barras_cid <- renderPlotly({
      if(length(input$checkbox_cid) != 0){
        aux <- datasetInputcid_ano() %>%
          group_by(CODMUNRES) %>%
          summarise(prevalencia = sum(nascidos_vivos_anomalia)/unique(numero_nascidos_vivos)*10^4, NOMEMUN = unique(NOMEMUN)) %>%
          arrange(prevalencia) %>% 
          filter(prevalencia > 0) %>%
          top_n(20, prevalencia)
        
        
        
        ordem <- aux$NOMEMUN
        names(aux)[3] = c("municipio")
        
        plot_barras <- ggplot(aux, aes(x = municipio, y = prevalencia)) +
          geom_col(fill = "darkmagenta") +
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
          geom_point(aes(x = ano, y = prevalencia), color = "darkmagenta") +
          geom_line(aes(x = ano, y = prevalencia, group = 1), color = "darkmagenta") +
          #geom_text(aes(x = ano,y = media, label = round(media,3))) +
          #scale_x_discrete(limits = ordem) +
          labs(x = "Ano", y = "Média Prevalência ao nascimento") +
          ylim(min(aux$prevalencia)-5, max(aux$prevalencia)+5)+
          #scale_fill_manual(values = cores_uti_emergencia) +
          #scale_color_manual(values = cores_uti_emergencia) +
          theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
        
        ggplotly(plotar)
        
      }
    })
    
    
    # output$grafico_boxplot_cid <- renderPlotly({
    #   if(length(input$checkbox_cid) != 0){
    #     
    #     # banco_aux <- banco_cid %>%
    #     #   filter(as.numeric(cid) %in% input$checkbox_cid) %>%
    #     #   group_by(ANO_NASC,CODMUNRES) %>%
    #     #   summarise(prevalencia = sum(nascidos_vivos_anomalia/unique(numero_nascidos_vivos))*10^4,NOMEMUN = unique(NOMEMUN),nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia))
    #     # 
    #     # 
    #     # 
    #     # 
    #     # banco_aux2 <- banco_nascimentos %>%
    #     #   left_join(banco_aux,by=c("CODMUNRES","ANO_NASC"))
    #     # 
    #     # 
    #     
    #     banco_aux2 <- datasetInputcid()
    #     
    #     banco_aux2$prevalencia[is.na(banco_aux2$prevalencia)] = 0
    #     
    #     banco_aux2$ano <- as.character(banco_aux2$ANO_NASC)
    #     
    #     
    #     
    #     banco_aux2 %>%
    #       plot_ly(x = ~ano,y = ~prevalencia, type = "box",marker = list(color = 'rgba(139,0,139,0.75)'),color = I("rgba(139,0,139,0.75)"),
    #               text=sprintf("Cidade: %s <br>Prevalências ao nascimento: %s <br>Ano: %s", banco_aux2$NOMEMUN,round(banco_aux2$prevalencia,2),banco_aux2$ano))# %>%    
    #     # layout(yaxis = list(range = input$limite_boxplot_cid))
    #     
    #     
    #     
    #   }
    # })
    
    # 
    # output$grafico_hist_cid <- renderPlotly({
    #   datasetInputcid_ano() %>%
    #     plot_ly(x = ~prevalencia, type = "histogram", hovertemplate = "Intervalo (%{x}) <br>Frequencia: %{y}",nbinsx = 75,marker = list(color = 'rgba(139,0,139,0.75)')) %>%    
    #     layout(
    #       xaxis = list(range=   input$limite_hist_prevalencia_cid),yaxis = list(c(0,500)))
    #   
    # })
    # 
    
    
    
    output$plot_dots_cid <-  renderPlotly({
      banco <- datasetInputcid()  %>%
        filter(CODMUNRES %in% macro_saude_filtro())
      
      
      
      banco$ano <- as.factor(banco$ANO_NASC)
      

      grafico <- ggplot(banco, aes(y = prevalencia, x = ano,fill = ano)) +
        geom_violin(position = position_dodge(width = 0.9)
                    #,
                    #text=sprintf(" %s <br>Prevalências ao nascimento: %s <br>Ano: %s", banco$NOMEMUN,round(banco$prevalencia,3),banco$ano)
                    ) +
        geom_quasirandom(dodge.width = 0.2, varwidth = TRUE,mapping = aes(text=sprintf(" %s <br>Prevalências ao nascimento: %s <br>Ano: %s", NOMEMUN,round(prevalencia,3),ano))) +
        ylim( input$limite_dots_cid) +
        scale_fill_manual(values = rep("darkmagenta",9))
      
      
      ggplotly(grafico,tooltip = "text")
      
    })
    
    
    
    output$plot_quadradinhos_cid <- renderPlotly({
      if(length(input$checkbox_cid) != 0){
        
        cidades_banco_quadradinhos = banco_anomalias_analise %>%
          filter(ANO_NASC == 2018) %>%
          arrange(Total) %>%
          filter(NOMEMUN %in% input$input_quadradinhos_cid)  %>%
          select(CODMUNRES)
        
        banco_agrupado <- banco_cid %>%
          filter(as.numeric(cid) %in% input$checkbox_cid,CODMUNRES %in% cidades_banco_quadradinhos[[1]]) %>%
          group_by(ANO_NASC,CODMUNRES) %>%
          summarise(prevalencia = sum(nascidos_vivos_anomalia)/unique(numero_nascidos_vivos)*10^4,nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia))
        
        banco_aux <- banco_nascimentos %>%
          filter(CODMUNRES %in% cidades_banco_quadradinhos[[1]]) %>%
          left_join(banco_agrupado, by = c("ANO_NASC","CODMUNRES"))
        
        banco_aux$prevalencia[is.na(banco_aux$prevalencia)] = 0
        banco_aux$nascidos_vivos_anomalia[is.na(banco_aux$nascidos_vivos_anomalia)] = 0
        
        quadradinho=ggplot(banco_aux,aes(x=ANO_NASC, y=reorder(NOMEMUN, prevalencia, FUN = sum), fill=prevalencia))+
          #scale_y_discrete(expand=c(0,0))
          geom_raster(aes(text=sprintf("Ano nascimento: %s<br>Cidade: %s<br>Prevalência ao nascimento: %s <br>Nº de nascidos vivos com anomalia: %s <br>Número de nascidos vivos: %s", ANO_NASC, NOMEMUN,round(prevalencia,2),nascidos_vivos_anomalia,numero_nascidos_vivos))) +
          scale_fill_viridis_c(option = "A", breaks =  round(bins_defalt$brks, 2), name= "Prevalência ao nascimento",alpha = 0.9)+
          labs(x="ano nascimento",y="cidade") +
          theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
        
        ggplotly(quadradinho,tooltip="text")
        
      }
      
    })
    
    
    output$plot_area_chart_cid <- renderPlotly({
      if(length(input$checkbox_cid) != 0){
        banco <- banco_cid %>%
          filter(as.numeric(cid) %in% input$checkbox_cid, CODMUNRES %in% macro_saude_filtro()) %>%
          group_by(cid,ANO_NASC) %>%
          summarise(nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia))
        
        
        
        myLevels <- banco %>%
          filter(ANO_NASC==2018) %>%
          arrange(nascidos_vivos_anomalia)
        
        banco$cid <- factor(banco$cid , levels=myLevels$cid )
        
        banco$ANO_NASC <- as.numeric(banco$ANO_NASC)
        
        
        banco <- banco %>%
          select(CID = cid,ano_nascimento = ANO_NASC,nascidos_vivos_anomalia = nascidos_vivos_anomalia)
        
        
        
        
        
        p <- ggplot(banco, aes(x=ano_nascimento, y=nascidos_vivos_anomalia, fill=CID)) + 
          geom_area(alpha=0.6 , size=.5, colour="white") +
          scale_fill_viridis(discrete = T,direction = -1) +
          theme_ipsum() +
          labs(x="Ano nascimento",y="Número de Nascidos vivos com anomalia")
        
        ggplotly(p)
      }
    })
    
    
    
    
    
    
    
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
      aux[,2:10] <- round(aux[,2:10],3)
      
      aux %>%
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
        )
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
        # #filter(pergunta_3 %in% input$curso)
        # filter(curso %in% input$curso) %>% ## testando
        # mutate(acesso_internet = pergunta_4,
        #        reside_em_poa = pergunta_9,
        #        disponibilidade_acompanhamento_ensino_remoto = pergunta_10,
        #        outros_motivos_dificuldade_acompanhamento = pergunta_11,
        #        comentarios_ensino_remoto = pergunta_12) %>%
        # select(curso, acesso_internet, nome, cartao_8_digitos, telefone,email,  reside_em_poa,
        #        disponibilidade_acompanhamento_ensino_remoto,outros_motivos_dificuldade_acompanhamento,
        #        comentarios_ensino_remoto,ingresso,i1,afastado,diplomacao,
        #        quantidade_creditos_matriculados,endereco,bairro,cep_cidade_uf) %>%
      # arrange(acesso_internet) %>%
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
      )
    })
    
    
    ############### CAIXAS APLICATIVO aba prevalência  ########################################################### 
    
    
    
    
    output$grafico_mapa_proporcao <- renderLeaflet({
      
      if(input$group_regiao %in% 1:2 ){
        
      dataset <- dataset_tipo_regiao_selecionada_ano()
      }
      if(input$group_regiao == 1){
        aux <- dataset_anomalia_analise_ano_filtro()  %>%
          select(1,4,5,6,7,8)
        names(aux)[4] = "Nome"
        names(aux)[6] = "variavel"
        names(aux)[3] = "nascidos_vivos_anomalia"
        
        dataset <- aux
        
        pal <- colorBin("plasma", domain = dataset$variavel, bins = bins_defalt$brks)
        pal2 <- function(x){
          ifelse(x==0,"#808080",pal(x))
        }
        
        tidy <- dataset %>%
          left_join(mapa_rs) 
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
                        direction = "bottom")) %>%
          leaflet::addLegend(pal = pal, values = ~tidy$variavel, opacity = 0.7, title = "Prevalência ao nascimento",
                             labFormat = labelFormat(digits = 3),
                             position = "bottomright")
        
      }else if(input$group_regiao == 2) {
        
        aux <- banco_meso_analise %>%
          filter(ANO_NASC == input$ano_grafico,CODMUNRES %in% input$filtro_geral)
        names(aux)[2] = "Nome"
        names(aux)[6] = "variavel"
        
        limites <- c(round(min(dataset$variavel),0)-1,round(max(dataset$variavel),0)+1)
        
        pal <- colorBin("plasma", domain = dataset$variavel, bins = seq(limites[1],limites[2],length.out = 6))
        
        pal2 <- function(x){
          ifelse(x==0,"#808080",pal(x))
        }
        #########################################################################################
        #### MAPA  
        #########################################################################################
        tidy <- dataset %>%
          merge(mapa_rs_meso,by.x = c("Nome"), by.y = c("NM_MESO"))  
        tidy = st_as_sf(tidy)
        tidy <- st_transform(tidy, "+init=epsg:4326") ##leaflet
        
        
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
                                      tidy$Nome, round(tidy$variavel,3), tidy$numero_nascidos_vivos, 
                                      tidy$nascidos_vivos_anomalia) %>%
                        lapply(htmltools::HTML),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "6px 11px"),
                        textsize = "13px",
                        direction = "bottom")) %>%
          leaflet::addLegend(pal = pal, values = ~tidy$variavel, opacity = 0.7, title = "Prevalência ao nascimento",
                             labFormat = labelFormat(digits = 3),
                             position = "bottomright")
        
        
      } else {
        banco_aux <- banco_anomalias_analise %>%
          filter(CODMUNRES %in% macro_saude_filtro())
        
        banco_aux2 <- banco_aux %>% 
          filter(ANO_NASC  == input$ano_grafico) %>%
          merge(.,banco_macro_saude,by.x=c("CODMUNRES"),by.y = c("IBGE"))
        
        dataset <- banco_aux2 %>%
          group_by(macrorregiao) %>%
          summarise(numero_nascidos_vivos = sum(numero_nascidos_vivos), nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia),
                    prevalencia = nascidos_vivos_anomalia/numero_nascidos_vivos*10^4) %>%
          ungroup() 
        
        names(dataset)[1] = "Nome"
        names(dataset)[4] = "variavel"
        
        limites <- c(round(min(dataset$variavel),0)-1,round(max(dataset$variavel),0)+1)
        
        pal <- colorBin("plasma", domain = dataset$variavel, bins = seq(limites[1],limites[2],length.out = 6))
        
        pal2 <- function(x){
          ifelse(x==0,"#808080",pal(x))
        }
        #########################################################################################
        #### MAPA  
        #########################################################################################
        tidy <- dataset %>%
          merge(macro_saude_shape,by.x = c("Nome"), by.y = c("macroregiao"))  
        tidy = st_as_sf(tidy)
        tidy <- st_transform(tidy, "+init=epsg:4326") ##leaflet
        
        
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
                                      tidy$Nome, round(tidy$variavel,3), tidy$numero_nascidos_vivos, 
                                      tidy$nascidos_vivos_anomalia) %>%
                        lapply(htmltools::HTML),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "6px 11px"),
                        textsize = "13px",
                        direction = "bottom")) %>%
          leaflet::addLegend(pal = pal, values = ~tidy$variavel, opacity = 0.7, title = "Prevalência ao nascimento",
                             labFormat = labelFormat(digits = 3),
                             position = "bottomright")
        
        
      }
      
    })
    
    
    
    
    
    
    ###########################################################################################  
    ### MAPA LISA
    
    output$mapa_lisa_prevalencia <- renderLeaflet({
      
        ### BANCO DE DADOS  
        dataset <- dataset_anomalia_analise_ano() %>%
            #filter(ANO_NASC == "2016") %>%
            select(6 ,7 , 8)  
        names(dataset)[3]=c("variavel")
        
        tidy <- dataset %>%
            right_join(mapa_rs) 
        tidy = st_as_sf(tidy)
        tidy <- st_transform(tidy, "+init=epsg:4326") ##leaflet
        
        #### teste moran
        #moran= moran.mc(tidy$variavel, matriz_w, nsim=999)
        
        ### LISA
        locm <- localmoran(tidy$variavel, matriz_w)
        tidy$Sgini <- scale(tidy$variavel)
        tidy$lag <- lag.listw(matriz_w, tidy$Sgini)
        tidy$pval <- locm[,5]
        
        tidy$quad_sig <- ifelse(tidy$Sgini >= 0 & tidy$lag >= 0 & tidy$pval <= 0.05, 1, 
                                ifelse(tidy$Sgini <= 0 & tidy$lag <= 0 & tidy$pval <= 0.05, 2, 
                                       ifelse(tidy$Sgini >= 0 & tidy$lag <= 0 & tidy$pval <= 0.05, 3, 
                                              ifelse(tidy$Sgini >= 0 & tidy$lag <= 0 & tidy$pval <= 0.05, 4, 5))))
        
        
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
                    direction = "bottom")) %>%
            leaflet::addLegend(
                pal = pal,
                values = ~ tidy$quad_sig,
                opacity = 0.7,
                title = NULL,
                labFormat = labelFormat(digits = 5),
                position = "bottomright"
            ) %>%
            addScaleBar(position = 'bottomleft')
        
        
    })
    
    
    

    
    
    
    # ####################### GRAFICOS BARRAS E SERIE
    
    output$prevalencia_barras <- renderPlotly({
        
        aux <- dataset_anomalia_analise_ano_filtro() %>%
            #filter(ANO_NASC == 2016) %>%
            select(NOMEMUN, prevalencia) %>%
            #mutate(prevalencia = `prevalencia de anomalias congenitas`) %>%
            arrange(prevalencia) %>%
            top_n(20, prevalencia)
        
        ordem <- aux$NOMEMUN
        names(aux)[1] = c("município")
        
        plot_barras <- ggplot(aux, aes(x = município, y = prevalencia)) +
            geom_col(fill = "darkmagenta") +
            labs(x = "Município", y = "Prevalência ao nascimento") +
            #labs(x = "Municipio", y = "Gráfico das 20 cidades com maiores valores de Prevalencia por 10000") +
            scale_x_discrete(limits = ordem) +
            #      axis.text.x = element_text(angle=90,size=7, vjust = 0.5)
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
        
        
        #ordem <- as.character(aux$ano)
        aux$ANO_NASC = as.character(aux$ANO_NASC)
        aux=data.frame(aux)
        names(aux)[1] = "ano"
        
        plotar = ggplot(aux) +
            geom_point(aes(x = ano, y = prevalencia), color = "darkmagenta") +
            geom_line(aes(x = ano, y = prevalencia, group = 1), color = "darkmagenta") +
            #geom_text(aes(x = ano,y = media, label = round(media,3))) +
            #scale_x_discrete(limits = ordem) +
            labs(x = "Ano", y = "Média Prevalência ao nascimento") +
            ylim(min(aux$prevalencia)-5, max(aux$prevalencia)+5)+
            #scale_fill_manual(values = cores_uti_emergencia) +
            #scale_color_manual(values = cores_uti_emergencia) +
            theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
        
        ggplotly(plotar)
        
    })
    
    
    output$boxplot_prevalencia <- renderPlotly({
        
      banco_aux <- banco_anomalias_analise %>%
        filter(CODMUNRES %in% macro_saude_filtro())
      banco_aux$ano <- as.character(banco_aux$ANO_NASC)  
      
      
      banco_aux %>%
        plot_ly(x = ~ano,y = ~prevalencia, type = "box",marker = list(color = 'rgba(139,0,139,0.75)'),color = I("rgba(139,0,139,0.75)"),
                text=sprintf("Cidade: %s <br>Prevalências ao nascimento: %s <br>Ano: %s", banco_aux$NOMEMUN,round(banco_aux$prevalencia,2),banco_aux$ano)) %>%    
        layout(yaxis = list(range = input$limite_boxplot1))
      
        
    })
    
    output$prevalencia_hist <- renderPlotly({
      dataset_anomalia_analise_ano_filtro() %>%
        plot_ly(x = ~prevalencia, type = "histogram", hovertemplate = "Intervalo (%{x}) <br>Frequencia: %{y}",nbinsx = 75,marker = list(color = 'rgba(139,0,139,0.75)') ) %>%    
        layout(
          xaxis = list(range=   input$limite_hist_prevalencia),yaxis = list(c(0,500)))
      
    
    })
    
    
    
    
    #######################################################################
    output$tabela_teste_moran = function(){
        
        tempor = round(teste_moran, 3) %>%
            filter(ano_teste ==input$ano_grafico) %>%
            #filter(ano_teste == 2002) %>%
            select(1,2) 
        
        
        colnames(tempor) <- c('Valor da estatística de Teste', 'p-valor')
        rownames(tempor) <- c(input$ano_grafico)
        
        #knitr::kable(tempor, align = 'r', digits = 2, caption = 'Estatística Índice Geral de Moran')
        
        tempor %>%
            knitr::kable("html") %>%
            kable_styling(bootstrap_options = c("striped", "hover", "responsive", 
                                                background = "withe", color = "black"),
                          font_size = 15)#%>%
        #column_spec(c(1,2,3), background = "withe", color = "black")
        
    }
    
    
    ######################################################################################
    ##
    ## TABELA
    ##
    
    rowCallback <- c(
        "function(row, data){",
        "  for(var i=0; i<data.length; i++){",
        "    if(data[i] === null){",
        "      $('td:eq('+i+')', row).html('NA')",
        "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
        "    }",
        "  }",
        "}"  
    )
    
    dataset_tabela_prevalencia <- reactive({
      banco_aux <- banco_anomalias_analise %>%
        filter(CODMUNRES %in% macro_saude_filtro()) %>%
        select(c(1,6,3,4,5,8)) %>%
        rename(ano_de_nascimento=ANO_NASC, nascidos_vivos_anomalia=nascidos_vivos_anomalia)
      
      banco_aux
    })
    
    output$tabela <- renderDataTable({
        
      dataset_tabela_prevalencia() %>%
            # #filter(pergunta_3 %in% input$curso)
            # filter(curso %in% input$curso) %>% ## testando
            # mutate(acesso_internet = pergunta_4,
            #        reside_em_poa = pergunta_9,
            #        disponibilidade_acompanhamento_ensino_remoto = pergunta_10,
            #        outros_motivos_dificuldade_acompanhamento = pergunta_11,
            #        comentarios_ensino_remoto = pergunta_12) %>%
            # select(curso, acesso_internet, nome, cartao_8_digitos, telefone,email,  reside_em_poa,
            #        disponibilidade_acompanhamento_ensino_remoto,outros_motivos_dificuldade_acompanhamento,
            #        comentarios_ensino_remoto,ingresso,i1,afastado,diplomacao,
            #        quantidade_creditos_matriculados,endereco,bairro,cep_cidade_uf) %>%
        # arrange(acesso_internet) %>%
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
        )
    })
    
    #########################
    
    # datasetInput <- reactive({
    #     
    #     banco_anomalias_analise %>%
    #         select(-municipio, - Total) %>%
    #         rename(ano_de_nascimento = ANO_NASC ,  nascidos_vivos_anomalia = nascidos_vivos_anomalia)
    #     # banco_completo2 = banco_completo %>%
    #     #   #filter(pergunta_3 %in% input$curso)
    #     #   filter(curso %in% input$curso) %>% ## testando
    #     #   mutate(acesso_internet = pergunta_4,
    #     #          reside_em_poa = pergunta_9,
    #     #          disponibilidade_acompanhamento_ensino_remoto = pergunta_10,
    #     #          outros_motivos_dificuldade_acompanhamento = pergunta_11,
    #     #          comentarios_ensino_remoto = pergunta_12) %>%
    #     #   select(curso, acesso_internet, nome, cartao_8_digitos, telefone,email,  reside_em_poa,
    #     #          disponibilidade_acompanhamento_ensino_remoto,outros_motivos_dificuldade_acompanhamento,
    #     #          comentarios_ensino_remoto,ingresso,i1,afastado,diplomacao,
    #     #          quantidade_creditos_matriculados,endereco,bairro,cep_cidade_uf) %>%
    #     #   arrange(acesso_internet)
    # })
    
    
    

    
    
    
    
    output$downloadData <- downloadHandler(contentType = "csv",
        
        filename = function() {
            paste("banco_anomalias_congenitas_rs_", Sys.Date(),".csv", sep="")
        },
        content = function(file) {
          banco_aux <- dataset_tabela_prevalencia()
          
          write.csv( banco_aux,file)
        }
        
    )
    
    ######################################################################################
    ##
    ## QUADRADINHOS
    ##
    
    
    # 
    # # ui_filtro_quadradinhos
    # output$ui_filtro_quadradinhos <- renderUI({
    #   
    #   var <- rlang::sym(str_c(input$var_covid,input$tipo_covid))
    #   var2 <- rlang::sym(input$agrup_covid)
    #   pop_var <- rlang::sym(str_c("populacao_estimada_",input$agrup_covid))
    #   
    #   aux <- dados_covid_rs %>%
    #     filter(regiao_covid %in% input$filtro_covid) %>%
    #     mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
    #            acompanhamento = ifelse(evolucao == "EM ACOMPANHAMENTO", 1, 0),
    #            recuperados = ifelse(evolucao == "RECUPERADO", 1, 0)) %>% 
    #     group_by(!!var2) %>%
    #     summarise(confirmados = n(), confirmados_taxa = n()*100000/as.numeric(first(!!pop_var)),
    #               obitos = sum(obitos, na.rm = T), obitos_taxa = sum(obitos, na.rm = T)*100000/as.numeric(first(!!pop_var)), 
    #               acompanhamento = sum(acompanhamento, na.rm = T), acompanhamento_taxa = sum(acompanhamento, na.rm = T)*100000/as.numeric(first(!!pop_var)),
    #               recuperados = sum(recuperados, na.rm = T), recuperados_taxa = sum(recuperados, na.rm = T)*100000/as.numeric(first(!!pop_var)),
    #               populacao = as.numeric(first(!!pop_var))) %>%
    #     arrange(desc(!!var))
    #   
    #   aux <- as.data.frame(aux)
    #   
    #   box(
    #     width = 12,
    #     selectInput(
    #       "filtro_quadradinhos",
    #       label = "Selecione os municípios de interesse(por default estão os 15 de maior quantidade da variável escolhida)",
    #       choices = aux[,input$agrup_covid],
    #       selected = aux[1:15,input$agrup_covid],
    #       multiple = T
    #     ),
    #     plotlyOutput("plot_quadradinhos", height = 650L)
    #   )
    #   
    #   
    # })
    
    # plot_quadradinhos
    output$plot_quadradinhos <- renderPlotly({
        
        cidades_banco_quadradinhos = banco_anomalias_analise %>%
            filter(ANO_NASC == 2018) %>%
            arrange(Total) %>%
            filter(NOMEMUN %in% input$input_quadradinhos)  %>%
            select(CODMUNRES)
        
        banco_quadradinhos = banco_anomalias_analise %>%
            filter(CODMUNRES %in% cidades_banco_quadradinhos[[1]])
        
        
        
        
        
        
        quadradinho=ggplot(banco_quadradinhos,aes(x=ANO_NASC, y=reorder(NOMEMUN, prevalencia, FUN = sum), fill=prevalencia))+
            #scale_y_discrete(expand=c(0,0))
            geom_raster(aes(text=sprintf("Ano nascimento: %s<br>Cidade: %s<br>Prevalência ao nascimento: %s", ANO_NASC, NOMEMUN,round(prevalencia,2)))) +
            scale_fill_viridis_c(option = "A", breaks =  round(bins_defalt$brks, 2), name= "Prevalência")+
            labs(x="ano nascimento",y="cidade") +
            theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
        
        ggplotly(quadradinho,tooltip="text")
        
        
    })

    ############################################################################################
    output$mapa_n_casos <- renderLeaflet({
        
        
        ### BANCO DE DADOS  
        dataset <- dataset_anomalia_analise_casos_ano_filtro() %>%
            select(6, 4, 5, 7, 8)  
        names(dataset)[3]=c("variavel")
        
        pal <- colorBin("YlOrRd", domain = dataset$variavel, bins = bins_defalt_nascidos_vivos_anomalia$brks)
        #########################################################################################
        #### MAPA  
        #########################################################################################
        
        pal2 <- function(x){
          ifelse(x==0,"#808080",pal(x))
        }
        
        tidy <- dataset %>%
            left_join(mapa_rs) 
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
                            direction = "bottom")) %>%
            leaflet::addLegend(pal = pal, values = ~tidy$variavel, opacity = 0.7, 
                      title = "N nascidos vivos c/ anomalias",
                      labFormat = labelFormat(digits = 3),
                      position = "bottomright")
        
    })
    
    ###########################################################################################  
    
    
    
    ##################### GRAFICOS BARRAS E SERIE
    
    output$n_casos_barras <- renderPlotly({
        
        aux <- dataset_anomalia_analise_casos_ano_filtro() %>%
          filter(nascidos_vivos_anomalia > 0 ) %>%
            #filter(ANO_NASC == 2016) %>%
            select(NOMEMUN, nascidos_vivos_anomalia) %>%
            #mutate(prevalencia = `prevalencia de anomalias congenitas`) %>%
            arrange(nascidos_vivos_anomalia) %>%
            top_n(20, nascidos_vivos_anomalia)
        
        ordem <- aux$NOMEMUN
        names(aux)[1] = c("município")
        
        plot_barras <- ggplot(aux, aes(x = município, y = nascidos_vivos_anomalia)) +
            geom_col(fill = "red2") +
            labs(x = "Município", y = "Prevalência ao nascimento") +
            #labs(x = "Municipio", y = "Gráfico das 20 cidades com maiores valores de Prevalencia por 10000") +
            scale_x_discrete(limits = ordem) +
            #      axis.text.x = element_text(angle=90,size=7, vjust = 0.5)
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
        
        #ordem <- as.character(aux$ano)
        aux$ANO_NASC = as.character(aux$ANO_NASC)
        aux=data.frame(aux)
        names(aux)[1] = "ano"
        
        plotar = ggplot(aux) +
            geom_point(aes(x = ano, y = total), color = "red2") +
            geom_line(aes(x = ano, y = total, group = 1), color = "red2") +
            #geom_text(aes(x = ano,y = media, label = round(media,3))) +
            #scale_x_discrete(limits = ordem) +
            labs(x = "Ano", y = "Número de nascidos vivos com anomalias congênitas") +
            ylim(min(aux$total)-5, max(aux$total)+5)+
            #scale_fill_manual(values = cores_uti_emergencia) +
            #scale_color_manual(values = cores_uti_emergencia) +
            theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
        
        ggplotly(plotar)
        
    })
    
    
    output$plot_quadradinhos_casos <- renderPlotly({
        
        cidades_banco_quadradinhos = banco_anomalias_analise %>%
            filter(ANO_NASC == 2018) %>%
            arrange(Total) %>%
            filter(NOMEMUN %in% input$input_quadradinhos_casos) %>%
            select(CODMUNRES)
        
        banco_quadradinhos = banco_anomalias_analise %>%
            filter(CODMUNRES %in% cidades_banco_quadradinhos[[1]])
        
        
        quadradinho=ggplot(banco_quadradinhos,aes(x=ANO_NASC, y=reorder(NOMEMUN, nascidos_vivos_anomalia, FUN = sum), fill=nascidos_vivos_anomalia))+
            #scale_y_discrete(expand=c(0,0))
            geom_raster(aes(text=sprintf("Ano nascimento: %s<br>Cidade: %s<br>Número de casos: %s", ANO_NASC, NOMEMUN,nascidos_vivos_anomalia))) +
            scale_fill_viridis_c(option = "C", name= "N nasc c/ anomalia")+
            #scale_fill_gradient()+
            #, name= "Número de nascidos vivos com anomalias congênitas")+
            labs(x="",y="") +
            theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
        
        ggplotly(quadradinho,tooltip="text")
        
        
    })
    
    
    
    ###### Aba Mapa População   ##################################
    
    output$mapa_pop <- renderLeaflet({
        
      dataset <- dataset_anomalia_analise_pop_ano_filtro() %>%
            #filter(ANO_NASC == "2016") %>%
            select(6, 4, 5, 7, 8)  
        names(dataset)[2]=c("variavel")
        
        max(banco_anomalias_analise$numero_nascidos_vivos)
        
        
        pal <- colorBin("plasma", domain = dataset$variavel, bins = (exp(seq(0,10,2))-1))
        pal2 <- function(x){
          ifelse(x==0,"#808080",pal(x))
        }
        #########################################################################################
        #### MAPA  
        #########################################################################################
        tidy <- dataset %>%
            left_join(mapa_rs) 
        tidy = st_as_sf(tidy)
        tidy <- st_transform(tidy, "+init=epsg:4326") ##leaflet
        
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
                      labFormat = labelFormat(digits = 3),
                      position = "bottomright")
        
        
    })
    
    output$pop_barras <- renderPlotly({
        
        aux <- dataset_anomalia_analise_pop_ano_filtro() %>%
            select(NOMEMUN, numero_nascidos_vivos) %>%
            arrange(numero_nascidos_vivos) %>%
            top_n(20, numero_nascidos_vivos)
        
        ordem <- aux$NOMEMUN
        names(aux)[1] = c("município")
        
        plot_barras <- ggplot(aux, aes(x = município, y = numero_nascidos_vivos)) +
            geom_col(fill = "red2") +
            labs(x = "Município", y = "numero_nascidos_vivos") +
            #labs(x = "Municipio", y = "Gráfico das 20 cidades com maiores valores de Prevalencia por 10000") +
            scale_x_discrete(limits = ordem) +
            #      axis.text.x = element_text(angle=90,size=7, vjust = 0.5)
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
        
        #ordem <- as.character(aux$ano)
        aux$ANO_NASC = as.character(aux$ANO_NASC)
        aux=data.frame(aux)
        names(aux)[1] = "ano"
        
        plotar = ggplot(aux) +
            geom_point(aes(x = ano, y = total), color = "red2") +
            geom_line(aes(x = ano, y = total, group = 1), color = "red2") +
            #geom_text(aes(x = ano,y = media, label = round(media,3))) +
            #scale_x_discrete(limits = ordem) +
            labs(x = "Ano", y = "Número de nascidos vivos") +
            ylim(min(aux$total)-5, max(aux$total)+5)+
            #scale_fill_manual(values = cores_uti_emergencia) +
            #scale_color_manual(values = cores_uti_emergencia) +
            theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
        
        ggplotly(plotar)
        
    })
    
    
    
    output$plot_quadradinhos_pop <- renderPlotly({
        
        cidades_banco_quadradinhos = banco_anomalias_analise %>%
            filter(ANO_NASC == 2018) %>%
            arrange(Total) %>%
            filter(NOMEMUN %in% input$input_quadradinhos_pop) %>%
            select(CODMUNRES)
        
        banco_quadradinhos = banco_anomalias_analise %>%
            filter(CODMUNRES %in% cidades_banco_quadradinhos[[1]])
        
        
        quadradinho=ggplot(banco_quadradinhos,aes(x=ANO_NASC, y=reorder(NOMEMUN, numero_nascidos_vivos, FUN = sum), fill=numero_nascidos_vivos))+
            #scale_y_discrete(expand=c(0,0))
            geom_raster(aes(text=sprintf("Ano nascimento: %s<br>Cidade: %s<br>Número nascidos vivos: %s", ANO_NASC, NOMEMUN,numero_nascidos_vivos))) +
            scale_fill_viridis_c(option = "C", name= "Número de nascimentos")+
            #scale_fill_gradient()+
            #, name= "Número de nascidos vivos com anomalias congênitas")+
            labs(x="",y="") +
            theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
        
        ggplotly(quadradinho,tooltip="text")
        
        
    })
    
    
    
 
    
    output$grafico_serie_casos <- renderPlotly({
        
        serie <- banco_anomalias_analise %>%
            filter(NOMEMUN %in% input$cidade2) 
        
        ggplotly(
            ggplot(serie, aes(x = ANO_NASC , y = nascidos_vivos_anomalia, colour = NOMEMUN)) +
                geom_line() +
                geom_point(size = 2,aes(text=sprintf("Ano nascimento: %s<br>Cidade: %s<br>Número de nascidos vivos com anomalia: %s", ANO_NASC, NOMEMUN,round(nascidos_vivos_anomalia,2))))+
                ylim(input$limite2[1],input$limite2[2])+
                labs(x = "Ano", y = "Nascidos vivos com anomalias congenitas") +labs(color = "Municípios") ,
            tooltip="text"
        )
        
    })
    
    
    
    #############################################################################
    
    output$grafico_serie_proporcao <- renderPlotly({
        
        serie2 <- banco_anomalias_analise %>%
            filter(NOMEMUN %in% input$cidade1) 
        
        ggplotly(
            ggplot(serie2, aes(x = ANO_NASC , y = prevalencia, colour = NOMEMUN)) +
                geom_line() +
                geom_point(size = 2,aes(text=sprintf("Ano nascimento: %s<br>Cidade: %s<br>Prevalência ao nascimento: %s", ANO_NASC, NOMEMUN,round(prevalencia,2))))+
                ylim(input$limite1[1],input$limite1[2])+
                labs(x = "Ano", y = "Prevalência ao nascimento de anomalias congenitas")+labs(color = "Municípios"),
            tooltip="text"
            #theme(legend.position = "none")
        )
        
    })
    
    output$scan_result_texto_red <- renderUI({
      cid_selecionado <- as.numeric(input$cid_scan)
      
      
      resultado <- lista_completa[[cid_selecionado]][1]$MC_pvalue
      cluster <- lista_completa[[cid_selecionado]][2]$cluster$municipio
      
      
      
      
      
      n <- length(cluster)
      texto <- str_c("<br>Distribuição: ",resultado$distribution," <br/>",
                     "Monte Carlo p-valor: ", resultado$MC_pvalue, "<br>",
                     "Municípios que pertencem ao cluster: ",paste(cluster[1:(n-1)],collapse=", ")," e ",cluster[n],".")
      
      
      
      HTML(texto)
    })
    
    
    
    
    output$scan_result_texto <- renderUI({
      cid_selecionado <- as.numeric(input$cid_scan)
      
      
      resultado <- lista_completa[[cid_selecionado]][1]$MC_pvalue
      cluster <- lista_completa[[cid_selecionado]][2]$cluster$municipio
      
  
      
      
      
      n <- length(cluster)
      texto <- str_c("<br>Distribuição: ",resultado$distribution," <br/>",
                     "Tipo de estatística Scan: univariado"," <br/>",
                     "Número de Regiões consideradas: ",resultado$n_zones," <br>",
                     "Número de Replicações de Monte Carlo: ",resultado$n_mcsim," <br>",
                     "Monte Carlo p-valor: ", resultado$MC_pvalue, "<br>",
                     "Gumbel p-valor: ",round(resultado$Gumbel_pvalue,5)," <br>",
                     "Municípios que pertencem ao cluster: ",paste(cluster[1:(n-1)],collapse=", ")," e ",cluster[n],".")
      
      
      
      HTML(texto)
    })
    
    
    
    output$scan_cluster_texto <- renderText({
      cid_selecionado <- as.numeric(input$cid_scan)
      cluster <- lista_completa[[cid_selecionado]][2]$cluster$municipio
      n <- length(cluster)
      str_c("Os municípios que pertencem ao cluster são: ",paste(cluster[1:(n-1)],collapse=", ")," e ",cluster[n],".")
      
    })
    
    
    output$mapa_scan_cluster <- renderPlot({
      cid_selecionado <- as.numeric(input$cid_scan)
      
      resultado <- lista_completa[[cid_selecionado]][1]
      cluster <- lista_completa[[cid_selecionado]][2]
      banco <- lista_completa[[cid_selecionado]][3]
      banco_modelo <- lista_completa[[9]]
      
      
      banco$NOMEMUN <- unique(banco_modelo$NOMEMUN)
      banco$municipio  <- str_to_lower(banco$NOMEMUN)
      
      
      dataset <- merge(mapa_rs_modelo, banco, by = "municipio", all.y = TRUE)# %>%
      dataset$variavel <- dataset$municipio %in% cluster$cluster$municipio
      
      
      
      dataset$variavel <- as.factor(dataset$variavel)
      
      
      
      level <- levels(dataset$variavel) <- c("Não pertence","Pertence")
      
      factpal <- colorFactor("plasma", dataset$variavel)
      
      gRR <- ggplot(dataset) + geom_sf(aes(fill = variavel ),alpha = 0.8) +
        scale_fill_manual(values = c(factpal(level[1]),factpal(level[2])), name= "Cluster") +
        theme_bw()
      
      
      gRR
      
      
    })
    

    
    
    
    
    output$mapa_scan <- renderLeaflet({
      cid_selecionado <- as.numeric(input$cid_scan)
      banco <- lista_completa[[cid_selecionado]][3]
      banco_modelo <- lista_completa[[9]]
      
      
      
      banco$NOMEMUN <- unique(banco_modelo$NOMEMUN)
      banco$municipio  <- str_to_lower(banco$NOMEMUN)
      
      
      dataset <- merge(mapa_rs_modelo, banco, by = "municipio", all.y = TRUE)# %>%
      
      
      
      
      
      names(dataset)[7]=c("variavel")
      pal <- colorBin("plasma", domain =dataset$variavel, bins = seq(0,1,length.out = 7))
      
      tidy = st_as_sf(dataset)
      tidy <- st_transform(tidy, "+init=epsg:4326") ##leaflet
      
      
      leaflet(tidy) %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
        addPolygons(fillColor = ~pal(variavel), 
                    weight = 1.5,
                    opacity = 0.7,
                    fillOpacity = 0.7,
                    color = "gray",
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = sprintf("<strong>%s</strong><br/>Relative score:
                                  %s",
                                    tidy$municipio, round(tidy$variavel,3)) %>%
                      lapply(htmltools::HTML),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "6px 11px"),
                      textsize = "13px",
                      direction = "bottom")) %>%
        leaflet::addLegend(pal = pal, values = ~tidy$variavel, opacity = 0.7, title = "Relative score",
                           labFormat = labelFormat(digits = 3),
                           position = "bottomright")
      
      
    })
    
    
    
    
    
    
    
    
}
