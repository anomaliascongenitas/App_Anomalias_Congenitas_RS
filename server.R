options(OutDec= ",") #Muda de ponto para virgula nos decimais! 

server <- function(input, output,session) {
    
  #callModule(profvis_server, "profiler") ### função para monitorar o desempenho do aplicativo
  
   output$input_quadradinhos_html <- renderUI({ 
    
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
    

    source("arquivos_server/box.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
    source("arquivos_server/bancos_reativos.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)

    source("arquivos_server/aba_mapa_cid.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
    source("arquivos_server/aba_mapa_prevalencia.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
    source("arquivos_server/aba_mapa_casos.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
    source("arquivos_server/aba_mapa_pop.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
    source("arquivos_server/aba_scan.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
    source("arquivos_server/aba_serie_casos.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
    source("arquivos_server/aba_serie_prevalencia.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)

}
