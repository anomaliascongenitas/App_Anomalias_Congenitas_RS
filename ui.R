#library(profvis)




################################################
options(OutDec= ".") #Muda de ponto para virgula nos decimais! 

################################################################################
################
################  lendo e adaptando os bancos de dados
################
################################################################################



# banco_municipios <- banco_anomalias_analise %>%
#     filter(ANO_NASC == 2018) %>%
#     arrange(desc(numero_nascidos_vivos)) %>%
#     select(NOMEMUN,numero_nascidos_vivos) 
# 
# write.csv(banco_municipios,file = "banco_municipios.csv",row.names=FALSE)


anos <- 2010:2018
limites_contagem <- 253
limites_prevalencia <- 1300
cids_values <- c("Cardiopatias congênitas",                                              
                 "Defeitos de parede abdominal",                                         
                 "Defeitos de redução de membros/ pé torto/ artrogripose / polidactilia",
                 "Defeitos de Tubo Neural",                                              
                 "Fendas orais",                                                         
                 "hipospadia",                                                           
                 "Microcefalia",                                                         
                 "Sexo indefinido",                                                      
                 "Síndrome de Down")   




cids_values2 <- c("Cardiopatias congênitas – CID Q20, Q21, Q22, Q23, Q24, Q25, Q26, Q27, Q28",
                  "Defeitos de parede abdominal – CID Q79.2 Q79.3",
                  "Defeitos de redução de membros/ pé torto/ artrogripose / polidactilia – CID Q66, Q69, Q71, Q72, Q73 e Q74.3",
                  "Defeitos de Tubo Neural – CID Q00.0, Q00.1, Q00.2, Q01 e Q05",
                  "Fendas orais – CID Q35, Q36 e Q37",
                  "hipospadia - CID  Q54",
                  "Microcefalia – CID Q02",
                  "Sexo indefinido CID 56",
                  "Síndrome de Down – CID Q90")



#header <- dashboardHeader(title = "Análise de nasc. vivos com anomalias congênitas no RS",
    #                      titleWidth = 600)


header <- dashboardHeaderPlus(
  enable_rightsidebar = T,
  rightSidebarIcon = "gears",
  title = tagList(
    span(class = "logo-lg", "Análise de nasc. vivos com anomalias congênitas no RS"), 
    icon = icon("tachometer-alt")),
  titleWidth = 650
)





rightsidebar <- rightSidebar( icon = "desktop",
  width = 400,
  h3("Digite as Macrorregiões de saúde de interesse"),
  selectizeInput("filtro_geral",
                 label = NULL,
                 choices = macro_saude_shape$macroregiao,
                 selected = macro_saude_shape$macroregiao,
                 multiple = T,
                 width = "100%")
)





sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Mapa Estratificado pelo CID",tabName = "mapa_cid",icon = icon("globe-americas")),
    menuItem("Mapa da prevalência ao nascimento", tabName = "mapa_porporcao",icon = icon("globe-americas")),
    menuItem("Mapa nasc. vivos com anomalia", tabName = "mapa_n_casos",icon = icon("globe-americas")),
    menuItem("Mapa nasc. vivos", tabName = "mapa_populacao",icon = icon("globe-americas")),
    menuItem("Série Temp. prevalência ao nascimento", tabName = "serie_proporcao",icon = icon("chart-line")),
    menuItem("Série Temp. nasc. vivos com anomalia", tabName = "serie_casos",icon = icon("chart-line")),
    menuItem("Estatística Scan", tabName = "scan",icon = icon("search-location")),
    # menuItem("Série Temp. nasc. vivos", tabName = "serie_pop"),
    # menuItem("Tabela com dados das prevalências", tabName = "tabela"),
    menuItem("Sobre", tabName = "sobre",icon = icon("book"))
  ),
  width = 300
)

body <- dashboardBody(
  tags$head(tags$style(HTML(".small-box {height: 115px}"))),
  tabItems(
    
    
    tabItem("mapa_cid",
            fluidPage(
              
              titlePanel("Mapa da prevalência ao nascimento considerandos os grupos de CIDs, ano e macrorregiões de saúde selecionados"),
              fluidRow(column(
                6,
                selectizeInput(
                  "ano_grafico_cid",
                  label = "Escolha o ano a ser considerado",
                  choices = anos,
                  selected = "2018"
                ),h4("O ícone no canto superior direito seleciona quais as macrorregiões de saúde do RS serão apresentadas")),
                column(
                  width = 6,
                  tags$img(src="ufrgs_logo.png", height = 100, width = 127),
                  tags$img(src="logo_ime.png", height = 100, width = 270),
                  tags$img(src="ppg_genetica.png", height = 100, width = 124)
                )
                
              ),br(),
              
              fluidRow(
                box(title = "",
                    checkboxGroupInput("checkbox_cid", "Escolha o(s) grupo(s) CID(s):", selected = 1,
                                       choiceNames = cids_values2,
                                       choiceValues = 1:9),
                    background = "blue",width = 12)
              ),  
              
              fluidRow(
                valueBoxOutput("box_populacao_cid", width = 3),
                valueBoxOutput("box_numero_casos_cid", width = 3),
                valueBoxOutput("box_prevalencia_cid", width = 3)
              ),
              fluidRow(
                box(h5("Selecione o tipo de agrupamento"),
                    radioButtons("group_regiao_cid",
                                 label = NULL,
                                 choices = list("Municípios" = 1, 
                                                #"Mesorregiões" = 2,
                                                "Macrorregiões de saúde"=3),
                                 selected = 1,
                                 inline = T),
                    title="Mapa das prevalências ao nascimento de anômalias congênitas considerando os grupos de CIDs, ano e macrorregiões de saúde selecionados",
                    background = "blue",
                    width = 12,
                    leafletOutput("grafico_mapa_proporcao_cid", height = "700px"),collapsible = TRUE
                )
              ),
              fluidRow(
                box(title = "Gráfico de barras dos 20 municipios com maior prevalencia considerando os grupos de CIDs, ano e macrorregiões de saúde selecionados",
                    width = 6,
                    background = "blue",
                    plotlyOutput("grafico_barras_cid"),collapsible = TRUE
                    
                ),
                box(title = "Serie temporal da prevalência ao nascimento considerando os grupos de CIDs e macrorregiões de saúde selecionados",
                    width = 6,
                    background =  "blue",
                    plotlyOutput("grafico_serie_cid"),collapsible = TRUE
                )
              ), 
              # fluidRow(
              #   box(title = "Boxplot da prevalência ao nascimento separado pelo ano",
              #       width = 6,
              #       sliderInput("limite_boxplot_cid",
              #                   "Limites do eixo vertical",
              #                   min = 0,
              #                   max = limites_prevalencia,
              #                   value = c(0,500),
              #                   step = 1),
              #       background = "blue",
              #       plotlyOutput("grafico_boxplot_cid")
              #   ),
              #   box(title = "Histograma da prevalência ao nascimento considerando o(s) CID(s) e ano selecionados",
              #       width = 6,  
              #       background = "blue",
              #       sliderInput("limite_hist_prevalencia_cid",
              #                   "Limites do eixo horizontal",
              #                   min = 0,
              #                   max = limites_prevalencia,
              #                   value = c(0,500),
              #                   step = 1),
              #       plotlyOutput("grafico_hist_cid")
              #       
              #   )
              # ),
              
              
              fluidRow(
                
                  box(
                    title = "Gráfico de densidade da prevalência ao nascimento ao longo dos anos considerando os grupos de CIDs, ano e macrorregiões de saúde selecionados",
                    background = "blue",
                    sliderInput("limite_dots_cid",
                                "Limites do eixo vertical",
                                min = 0,
                                max = limites_prevalencia,
                                value = c(0,limites_prevalencia),
                                step = 1),
                    plotlyOutput("plot_dots_cid"),
                    width = 12,collapsible = TRUE
                  )
                ),
              
              
              fluidRow(
                  box(
                    title = "Gráfico da evolução da prevalência ao nascimento considerando os grupos de CIDs e macrorregiões de saúde selecionados",
                    # selectizeInput("input_quadradinhos_cid",
                    #                label = "Escolha o(s) município(s)",
                    #                choices = unique(banco_municipios2$NOMEMUN),
                    #                multiple = T,
                    #                options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
                    #                selected = banco_municipios2$NOMEMUN[1:20]),
                    background = "blue",
                    htmlOutput("input_quadradinhos_html_cid"),
                    plotlyOutput("plot_quadradinhos_cid"),
                    width = 12,collapsible = TRUE
                  )
                ),
              
              fluidRow(
                
                  
                  box(
                    title = "Gráfico de Área do Número de nascidos vivos com anomalia congênita por grupo de CID considerando os grupos de CIDs e macrorregiões de saúde selecionados",
                    background = "blue",
                    plotlyOutput("plot_area_chart_cid"),
                    width = 12,collapsible = TRUE
                  )
                ),
              
              
              
              
              
              fluidRow(
                column(h2("Tabela de prevalência ao nascimento agrupada por cada grupo de CID considerando as macrorregiões de saúde selecionadas"),
                       width = 12,dataTableOutput("tabela_cid_1", height = 500),
                       downloadButton("downloadData_cid_1", "Download Tabela de dados"),
                       HTML("<br><br><br>")
                )
              )
              
              # ,
              # fluidRow(
              #   column(
              #     width = 12,
              #     
              #   )
              # )
              
              # ,
              # fluidRow(
              #   column(
              #     width = 12,
              #     
              #   )
              # )
              
              ,
              fluidRow(
                column(h2("Tabela de prevalência ao nascimento com apenas os grupos de CID's e macrorregiões de saúde selecionados"),
                       width = 12,
                       dataTableOutput("tabela_cid_2", height = 500),
                       downloadButton("downloadData_cid_2", "Download Tabela de dados"),
                       HTML("<br><br><br>")
                )
              )
              
              
              
              
              
            )
            
    ),  
    
    
  tabItem(
    "mapa_porporcao",
    fluidPage(
      titlePanel("Prevalências ao nascimento de anomalias congenitas por 10.000"),
      #profvis_ui("profiler"),
      fluidRow(column(
        6,
        selectizeInput(
          "ano_grafico",
          label = "Escolha o ano a ser considerado",
          choices = anos,
          selected = "2018"
        ),h4("O ícone no canto superior direito seleciona quais as macrorregiões de saúde do RS serão apresentadas")),
        column(
          width = 6,
          tags$img(src="ufrgs_logo.png", height = 100, width = 127),
          tags$img(src="logo_ime.png", height = 100, width = 270),
          tags$img(src="ppg_genetica.png", height =100, width = 124)
        ),br(),
        
      ),br(),
      fluidRow(
        valueBoxOutput("box_populacao", width = 3),
        valueBoxOutput("box_numero_casos", width = 3),
        valueBoxOutput("box_prevalencia", width = 3)
      ),
      fluidRow(
        column(
          width = 12,
          box(h5("Selecione o tipo de agrupamento"),
              radioButtons("group_regiao",
                           label = NULL,
                           choices = list("Municípios" = 1, 
                                          #"Mesorregiões" = 2,
                                          "Macrorregiões de saúde"=3),
                           selected = 1,
                           inline = T),
            title ="Mapa das prevalências ao nascimento de anômalias congênitas considerando o ano e macrorregiões de saúde selecionados",
            #h1("Mapa das prevalencias de anomalias congenitas no ano selecionado"),
            #h4("Dados provenientes da Pre"),
            leafletOutput("grafico_mapa_proporcao", height = "700px"),
            #HTML("<br><br><br>"),
            width = 12,
            background = "blue"
          )
        )),
      fluidRow(
        column(
          width = 6,
          box(
            title = "Gráfico de barras dos 20 municípios com maior prevalência ao nascimento considerando o ano e macrorregiões de saúde selecionados",
            background = "blue",
            plotlyOutput("prevalencia_barras"),
            width = 12
          )
        ),
        column(
          width = 6,
          box(
            title = "Série Temporal Prevalência ao nascimento considerando as macrorregiões de saúde selecionadas",
            background = "blue",
            plotlyOutput("prevalencia_serie"),
            width = 12
          )
        )),
      
      
      
      
      
      fluidRow(
        column(
          width = 6,
          box(
            title = "Boxplot da prevalência ao nascimento separado por ano considerando as macrorregiões de saúde selecionadas",
            background = "blue",
            sliderInput("limite_boxplot1",
                        "Limites do eixo vertical",
                        min = 0,
                        max = limites_prevalencia,
                        value = c(0,limites_prevalencia),
                        step = 1),
            plotlyOutput("boxplot_prevalencia"),width = 12
          )
        ),
        
        
        column(
          width = 6,
          box(
            title = "Histograma da prevalência ao nascimento das cidades no ano selecionado",
            background = "blue",
            sliderInput("limite_hist_prevalencia",
                        "Limites do eixo horizontal",
                        min = 0,
                        max = limites_prevalencia,
                        value = c(0,500),
                        step = 1),
            plotlyOutput("prevalencia_hist"),
            width = 12
          )
        )
      ),
      
      
      
      
      
      
      fluidRow(
        column(
          width = 4,
          box(
            title = "Resultado do teste I de moran",
            status = "primary",
            solidHeader = TRUE,
            #background = "blue",
            h4("O mapa do Índice Local ao lado só deve ser interpretado se a 
              estatística de teste abaixo for significativa"),
            tableOutput("tabela_teste_moran"),
            width = 12
          )
        ),
        column(
          width = 8,
          box(
            title ="Mapa do Índice Local de Associação Espacial (LISA)",
            #h1("Mapa do Índice Local de Associação Espacial (LISA)"),
            #h4("Dados provenientes da Pre"),
            leafletOutput("mapa_lisa_prevalencia", height = "700px"),
            background = "blue",
            width = 12
          )
        )),
      fluidRow(
        column(
          width = 12,
          box(
            title = "Gráfico da evolução das prevalências ao nascimento por 10.000 
              dos 20 municipios com maior número de nascidos vivos no RS considerando apenas as macrorregiões de saúde selecionadas",
            background = "blue",
            # selectizeInput("input_quadradinhos",
            #                label = "Escolha o(s) município(s)",
            #                choices = unique(banco_municipios2$NOMEMUN),
            #                multiple = T,
            #                options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
            #                selected = banco_municipios2$NOMEMUN[1:20]),
            htmlOutput("input_quadradinhos_html"),
            plotlyOutput("plot_quadradinhos"),
            width = 12
          )
        )),
      
      fluidRow(
        column(
          width = 12,
          downloadButton("downloadData", "Download Tabela de dados"),
          HTML("<br><br><br>")
        )
      ),
      fluidRow(
        column(
          width = 12,
          dataTableOutput("tabela", height = 500)
        )
      )
    )), #fluidpaige # Tabitem
  # 
  
  
  
  
  
  
  
  tabItem(
    "mapa_n_casos",
    fluidPage(
      titlePanel("Mapa do número de nascidos vivos com anomalias congênitas no ano selecionado"),
      
      fluidRow(column(
        6,
        selectizeInput(
          "ano_grafico_casos",
          label = "Escolha o ano a ser considerado",
          choices = anos,
          selected = "2018"
        ),h4("O ícone no canto superior direito seleciona quais as macrorregiões de saúde do RS serão apresentadas")),
        column(
          width = 6,
          tags$img(src="ufrgs_logo.png", height = 100, width = 127),
          tags$img(src="logo_ime.png", height = 100, width = 270),
          tags$img(src="ppg_genetica.png", height = 100, width = 124)
        ),
        
      ),br(),
      fluidRow(
        valueBoxOutput("box_populacao_casos", width = 3),
        valueBoxOutput("box_numero_casos_casos", width = 3),
        valueBoxOutput("box_prevalencia_casos", width = 3)
      ),
      
      fluidRow(
        column(
          width = 12,
          box(
            title ="Mapa do número de nascidos vivos com anomalias congênitas considerando apenas o ano e as macrorregiões de saúde selecionados",
            #h1("Mapa das prevalencias de anomalias congenitas no ano selecionado"),
            #h4("Dados provenientes da Pre"),
            leafletOutput("mapa_n_casos", height = "700px"),
            #HTML("<br><br><br>"),
            width = 12,
            background = "blue"
          )
        )),
      fluidRow(
        column(
          width = 6,
          box(
            title = "Gráfico de barras dos 20 municípios com maior número de nascidos vivos com anomalias congenitas no ano selecionado considerando apenas as macrorregiões de saúde selecionadas",
            background = "blue",
            plotlyOutput("n_casos_barras"),
            width = 12
          )
        ),
        column(
          width = 6,
          box(
            title = "Série Temporal do número total de nascidos vivos com anomalias congênitas considerando apenas as macrorregiões de saúde selecionadas e todos os anos",
            background = "blue",
            plotlyOutput("n_casos_serie"),
            width = 12
          )
        )),
      
      
      fluidRow(
        column(
          width = 12,
          box(
            title = "Gráfico da evolução do número de nascidos com anomalias
              20 municipios com maior número de nascidos vivos no RS considerando apenas as macrorregiões de saúde selecionadas",
            # selectizeInput("input_quadradinhos_casos",
            #                label = "Escolha o(s) município(s)",
            #                choices = unique(banco_municipios2$NOMEMUN),
            #                multiple = T,
            #                options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
            #                selected = banco_municipios2$NOMEMUN[1:20]),
            
            background = "blue",
            htmlOutput("input_quadradinhos_html_casos"),
            plotlyOutput("plot_quadradinhos_casos"),
            width = 12
          )
        )),
    )), # #fluidpage Tabitem
  
  
  # # 
  tabItem(
    "mapa_populacao",
    fluidPage(
      titlePanel("Mapa dos número de nascidos vivos"),
      
      fluidRow(column(
        6,
        selectizeInput(
          "ano_grafico_pop",
          label = "Escolha o ano a ser considerado",
          choices = anos,
          selected = "2018"
        ),h4("O ícone no canto superior direito seleciona quais as macrorregiões de saúde do RS serão apresentadas")),
        column(
          width = 6,
          tags$img(src="ufrgs_logo.png", height = 100, width = 127),
          tags$img(src="logo_ime.png", height = 100, width = 270),
          tags$img(src="ppg_genetica.png", height = 100, width = 124)
        )
        
      ),br(),
      fluidRow(
        valueBoxOutput("box_populacao_pop", width = 3),
        valueBoxOutput("box_numero_casos_pop", width = 3),
        valueBoxOutput("box_prevalencia_pop", width = 3)
      ),
      
      fluidRow(
        column(
          width = 12,
          box(
            title ="Mapa dos número de nascidos vivos no ano selecionado considerando apenas o ano e as macrorregiões de saúde selecionados",
            #h1("Mapa das prevalencias de anomalias congenitas no ano selecionado"),
            #h4("Dados provenientes da Pre"),
            leafletOutput("mapa_pop", height = "700px"),
            #HTML("<br><br><br>"),
            width = 12,
            background = "blue"
          )
        )),
      
      fluidRow(
        column(
          width = 6,
          box(
            title = "Gráfico de barras dos 20 munucípios com maior número de nascidos vivos no ano selecionado considerando apenas as macrorregiões de saúde selecionadas",
            background = "blue",
            plotlyOutput("pop_barras"),
            width = 12
          )
        ),
        column(
          width = 6,
          box(
            title = "Série Temporal do número total de nascidos vivos considerando apenas as macrorregiões de saúde selecionadas e todos os anos",
            background = "blue",
            plotlyOutput("pop_serie"),
            width = 12
          )
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          box(
            title = "Gráfico da evolução do número de nascidos vivos considerando apenas as macrorregiões de saúde selecionadas",
            # selectizeInput("input_quadradinhos_pop",
            #                label = "Escolha o(s) município(s)",
            #                choices = unique(banco_municipios2$NOMEMUN),
            #                multiple = T,
            #                options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
            #                selected = banco_municipios2$NOMEMUN[1:20]),
            background = "blue",
            htmlOutput("input_quadradinhos_html_pop"),
            plotlyOutput("plot_quadradinhos_pop"),
            width = 12
          )
        ))
      
    ) ) , #Tabitem fluid paige
  
  # tabItem("serie_pop",
  #         fluidPage(
  #           titlePanel("Série temporal dos nascidos vivos por município"),
  # 
  #           fluidRow(
  #             column(
  #               width = 12,
  #               tags$img(src="ufrgs_logo.png", height = 100, width = 127),
  #               tags$img(src="logo_ime.png", height = 100, width = 270),
  #               tags$img(src="ppg_genetica.png", height = 80, width = 95),
  #               offset = 6
  #             )
  #           ),
  # 
  #           fluidRow(
  #             sidebarPanel(
  #               #h3("Escolha a Cidade"),
  #               selectizeInput("cidade3",
  #                              label = "Escolha o município",
  #                              choices = unique(banco_luzivan_novo$Cidade),
  #                              multiple = T,
  #                              options = list(maxItems = 300, placeholder = 'Escolha os municípios
  #                                           que irão compor o gráfico'),
  #                              selected = "são luís"),
  #               #selected = levels(factor(banco_luzivan_novo$Cidade))),
  #               sliderInput("limite3",
  #                           "Limites do eixo vertical",
  #                           min = 0,
  #                           max = max(banco_luzivan_novo$populacao)+1,
  #                           value = c(0,max(banco_luzivan_novo$populacao)+1),
  #                           step = 10),
  #               width = 12
  #             )
  #           ),
  #           mainPanel(
  #             plotlyOutput("grafico_serie_pop", height = "600px"),
  #             width = 12
  #           )
  #         )
  # ),
  
  
 
  
  
  
  ################################################################################################
  
  tabItem("serie_casos",
          fluidPage(
            titlePanel("Série Temporal dos nascidos vivos com anomalias congenitas por município"),
            
            fluidRow(
              column(h4("O ícone no canto superior direito seleciona quais as macrorregiões de saúde do RS serão apresentadas"),width = 6),
              column(
                width = 6,
                tags$img(src="ufrgs_logo.png", height = 100, width = 127),
                tags$img(src="logo_ime.png", height = 100, width = 270),
                tags$img(src="ppg_genetica.png", height = 100, width = 124),
                offset = 6
              )
            ),br(),
            
            fluidRow(
              sidebarPanel(
                #h3("Escolha a Cidade"),
                #h3("Cursos"),
                # selectizeInput("cidade2",
                #                label = "Escolha o município",
                #                choices = unique(banco_municipios2$NOMEMUN),
                #                multiple = T,
                #                options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
                #                selected = "Porto Alegre"),
                htmlOutput("input_quadradinhos_html_cidade2"),
                sliderInput("limite2",
                            "Limites do eixo vertical",
                            min = 0,
                            max = limites_contagem,
                            value = c(0,limites_contagem),
                            step = 1),
                width = 12
              )
            ),
            mainPanel(
              plotlyOutput("grafico_serie_casos", height = "600px"),
              width = 12
            )
          )
  ),
  # 
  tabItem("serie_proporcao",
          fluidPage(
            titlePanel("Série Temporal das prevalências ao nascimento de anomalias congenitas por 10.000
                       por município"),
            
            fluidRow(
              column(h4("O ícone no canto superior direito seleciona quais as macrorregiões de saúde do RS serão apresentadas"),width = 6),
              column(
                width = 12,
                tags$img(src="ufrgs_logo.png", height = 100, width = 127),
                tags$img(src="logo_ime.png", height = 100, width = 270),
                tags$img(src="ppg_genetica.png", height = 100, width = 124),
                offset = 6
              )
            ),br(),
            
            fluidRow(
              sidebarPanel(
                #h3("Escolha a Cidade"),
                #h3("Cursos"),
                # selectizeInput("cidade1",
                #                label = "Escolha o município",
                #                choices = unique(banco_municipios2$NOMEMUN),
                #                multiple = T,
                #                options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
                #                selected = "Porto Alegre"),
                htmlOutput("input_quadradinhos_html_cidade1"),
                sliderInput("limite1",
                            "Limites do eixo vertical",
                            min = (0),
                            max = limites_prevalencia+2,
                            value = c(0,limites_prevalencia+2),
                            step = 1,
                            round = TRUE),
                width = 12
              )
            ),
            mainPanel(
              plotlyOutput("grafico_serie_proporcao", height = "600px"),
              width = 12
            )
          )
  ),
  
  
  
  tabItem("scan",
          fluidPage(
            titlePanel("Estatística Scan"),
            
            fluidRow(
              column(
                width = 12,
                tags$img(src="ufrgs_logo.png", height = 100, width = 127),
                tags$img(src="logo_ime.png", height = 100, width = 270),
                tags$img(src="ppg_genetica.png", height = 100, width = 124),
                offset = 6
              )
            ),br(),
            
            
            tags$style(HTML("
            .tabbable > .nav > li > a                  {background-color: #0073b6;  color:white; border-color: white}
            .tabbable > .nav > li[class=active]    > a {background-color: white; color:black}
          ")),
                    
        
            
            
            
            fluidRow(
              box(title = "",
                  radioButtons("cid_scan", "Escolha o grupo de CID para a Estatística Scan", selected = 1,
                                     choiceNames  = cids_values2,
                                     choiceValues = 1:9),
                  background = "blue",width = 12)
            ),
            fluidRow(
              box(    mainPanel(width = 12,
                                
                                # Output: Tabset w/ plot, summary, and table ----
                                tabsetPanel(type = "tabs",
                                            tabPanel(title ="Versão Resumida", uiOutput("scan_result_texto_red"),br()),
                                            tabPanel(title = "Versão Completa", uiOutput("scan_result_texto"), br())
                                )
                                
              ),
              title = "Resultado da Estatística Scan",
              background = "blue",width = 12)
            ),
            
            
            fluidRow(
              column(width = 2),
              column(align="center",
                     width = 8,
                     box(
                       title ="Mapa com o Cluster detectado pela Estatística Scan",
                       #h1("Mapa das prevalencias de anomalias congenitas no ano selecionado"),
                       #h4("Dados provenientes da Pre"),
                       plotOutput("mapa_scan_cluster", height = "500px"),br(),
                       textOutput("scan_cluster_texto"),br(),
                       #HTML("<br><br><br>"),
                       width = 12,
                       background = "blue",collapsible = TRUE
                     )
              ),column(width = 2)
            ),
            
            fluidRow(
              column(
                width = 12,
                box(
                  title ="Mapa do Risco Relativo por munícipio",
                  #h1("Mapa das prevalencias de anomalias congenitas no ano selecionado"),
                  #h4("Dados provenientes da Pre"),
                  leafletOutput("mapa_scan", height = "700px"),
                  #HTML("<br><br><br>"),
                  width = 12,
                  background = "blue",collapsible = TRUE
                )
              ))
            
           
            
            
  )),
  
  
  tabItem("sobre",
          fluidPage(
            fluidRow(
              
              # setZoom(id = "covidMetrika",class = "small-box"),
              #  setZoom(id = "git_covidMetrika",class = "small-box"),
              
              column(
                width = 12,
                valueBoxOutput("covidMetrika",width = 12)
              ),
              column(
                width = 6,
                valueBoxOutput("git_covidMetrika", width = 12)
              ),
              
              widgetUserBox(title="Aplicativo desenvolvido pela professora Márcia Helena Barbian 
                              com colaboração de Lavínia Schuler-Faccini, Augusto Cardoso dos Santos, 
                              .........",
                            type=2, collapsible = TRUE,color = "primary", width = 12,
                            tags$div(
                              class="box box-widget widget-user-2", style="left: 407px;bottom: 207px;"
                            )),
              # 
              # widgetUserBox(
              #   title = tags$b("Márcia Helena Barbian"),
              #   subtitle = "Professora do Departamento de Estatística da UFRGS",
              #   type = 2,
              #   width = 12,
              #   src = 'marcia.png',
              #   color = "blue",
              #   "Contato: mhbarbian@ufrgs.br",
              #   footer_padding = F
              # ),
              # 
              # widgetUserBox(
              #   title = tags$b("Luzivan Costa Reis"),
              #   subtitle = "Aluno de Pós-graduação em Genética e Biologia molecular, UFRGS",
              #   type = 2,
              #   width = 4,
              #   src = 'luzivan.jpg',
              #   color = "red",
              #   "Contato: luzivanreis@gmail.com",
              #   footer_padding = F
              # ),
              # widgetUserBox(
              #   title = tags$b("Lavínia Schuler-Faccini"),
              #   subtitle = "Professora do programa de Pós-graduação em Genética e Biologia molecular, UFRGS",
              #   type = 2,
              #   width = 4,
              #   src = 'ppg_genetica.png',
              #   color = "red",
              #   "Contato: lavinia.faccini@ufrgs.br",
              #   footer_padding = F
              # ),
              # 
              # widgetUserBox(
              #   title = tags$b("Augusto Cardoso dos Santos"),
              #   #subtitle = "Pós-graduação em Genética e Biologia molecular, UFRGS",
              #   type = 2,
              #   width = 4,
              #   src = 'ppg_genetica.png',
              #   color = "red",
              #   "Contato: santosaccd@gmail.com",
              #   footer_padding = F
              # ),
              # widgetUserBox(
              #   title = tags$b("Elis Vanessa de Lima e Silva"),
              #   #subtitle = "Pós-graduação em Genética e Biologia molecular, UFRGS",
              #   type = 2,
              #   width = 4,
              #   src = 'ppg_genetica.png',
              #   color = "red",
              #   "Contato: evlsilva@hcpa.edu.br",
              #   footer_padding = F
              # ),
              # widgetUserBox(
              #   title = tags$b("Juliano Boquett"),
              #   #subtitle = "Pós-graduação em Genética e Biologia molecular, UFRGS",
              #   type = 2,
              #   width = 4,
              #   src = 'ppg_genetica.png',
              #   color = "red",
              #   "Contato: Juliano Boquett ",
              #   footer_padding = F
              # ),
              
              widgetUserBox(title="Fonte de dados: Datasus",
                            type=2, collapsible = TRUE,color = "", width = 12,
                            tags$div(
                              class="box box-widget widget-user-2", style="left: 407px;bottom: 207px;"
                            )),
              # ),
              # 
              # 
              # tags$img(src = "logos.png", 
              #          height = "150", width = "1000")
            )
          ) #fluidpaige
  ) #Tabitem
  
  
  
)) 





#shinyUI(dashboardPage(header, sidebar,rightsidebar, body))

shinyUI(dashboardPagePlus(enable_preloader = T, rightsidebar = rightsidebar, header = header, sidebar = sidebar, 
                          body = body))

