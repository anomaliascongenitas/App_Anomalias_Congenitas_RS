aba_mapa_prevalencia <- tabItem(
  "mapa_prevalencia",
  fluidPage(
    fluidRow(
      tags$img(src = "logo_projeto_anomalias_git.png", height = 107 * 0.75),
      tags$img(
        src = "logo_parceiros_projeto.png",
        height = 107 * 0.75,
        width = 1275 * .75
      )
    ),
    fluidRow(
      tags$img(src = "ufrgs_logo.png", height = 107 * 0.75),
      tags$img(src = "logos_hcpa_ibc.png", height = 107 * 0.75),
      tags$img(src = "logo_ime.png", height = 107 * 0.75)
    )
    ,
    
    titlePanel("Prevalências ao nascimento de anomalias congenitas por 10.000 no ano selecionado"),
    #profvis_ui("profiler"),
    fluidRow(column(
      12,
      selectizeInput(
        "ano_grafico",
        label = "Escolha o ano a ser considerado",
        choices = anos,
        selected = "2019"
      ),
      h4(
        str_c(
          "O ícone no canto superior direito seleciona quais as macrorregiões de saúde ",
          preposicao_sigla_uf,
          " ",
          sigla_uf,
          " serão apresentadas"
        )
      )
    ), br()),
    br(), 
    fluidRow(
      valueBoxOutput("box_populacao", width = 3),
      valueBoxOutput("box_numero_casos", width = 3),
      valueBoxOutput("box_prevalencia", width = 3)
    ), 
    fluidRow(column(
      width = 12,
      box(
        h5("Selecione o tipo de agrupamento"),
        radioButtons(
          "group_regiao",
          label = NULL,
          choices = list("Municípios" = 1,
                         #"Mesorregiões" = 2,
                         "Macrorregiões de saúde" = 3),
          selected = 1,
          inline = T
        ),
        title = "Mapa das prevalências ao nascimento de anômalias congênitas considerando o ano e macrorregiões de saúde selecionados",
        leafletOutput("grafico_mapa_proporcao", height = "700px"),
        width = 12,
        background = "blue"
      )
    )),
    fluidRow(column(
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
    fluidRow(column(
      width = 6,
      box(
        title = "Boxplot da prevalência ao nascimento separado por ano considerando as macrorregiões de saúde selecionadas",
        background = "blue",
        sliderInput(
          "limite_boxplot1",
          "Limites do eixo vertical",
          min = 0,
          max = limites_prevalencia,
          value = c(0, limites_prevalencia),
          step = 1
        ),
        plotlyOutput("boxplot_prevalencia"),
        width = 12
      )
    ),
    column(
      width = 6,
      box(
        title = "Histograma da prevalência ao nascimento das cidades no ano selecionado",
        background = "blue",
        sliderInput(
          "limite_hist_prevalencia",
          "Limites do eixo horizontal",
          min = 0,
          max = limites_prevalencia,
          value = c(0, 500),
          step = 1
        ),
        plotlyOutput("prevalencia_hist"),
        width = 12
      )
    )),
    fluidRow(column(
      width = 4,
      box(
        title = "Resultado do teste I de moran",
        status = "primary",
        solidHeader = TRUE,
        #background = "blue",
        h4(
          "O mapa do Índice Local ao lado só deve ser interpretado se a
              estatística de teste abaixo for significativa"
        ),
        tableOutput("tabela_teste_moran"),
        width = 12
      )
    ),
    column(
      width = 8,
      box(
        title = "Mapa do Índice Local de Associação Espacial (LISA)",
        leafletOutput("mapa_lisa_prevalencia", height = "700px"),
        background = "blue",
        width = 12
      )
    )),
    # fluidRow(column(
    #   width = 12,
    #   box(
    #     title = "Gráfico da evolução das prevalências ao nascimento por 10.000
    #           dos 20 municipios com maior número de nascidos vivos no RS considerando apenas as macrorregiões de saúde selecionadas",
    #     background = "blue",
    #     htmlOutput("input_quadradinhos_html"),
    #     plotlyOutput("plot_quadradinhos"),
    #     width = 12
    #   )
    # )),
    
    fluidRow(column(
      width = 12,
      downloadButton("downloadData", "Download Tabela de dados"),
      HTML("<br><br><br>")
    )),
    fluidRow(column(
      width = 12,
      dataTableOutput("tabela", height = 500)
    )),
    
    fluidRow(#column(width = 3),
      column(
        width = 6,
        #tags$img(src="ufrgs_logo.png", height = 100, width = 127),
        tags$img(
          src = "pos_estatistica_logo.png",
          height = 100,
          width = 220
        ),
        tags$img(
          src = "ppg_genetica.png",
          height = 100,
          width = 124
        )
      ))
  )
)
