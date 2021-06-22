############# Aba Cid #############

aba_mapa_cid <- tabItem(
  "mapa_cid",
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
      tags$img(src = "logos_hcpa_ibc.png", height = 107 *
                 0.75),
      tags$img(src = "logo_ime.png", height = 107 *
                 0.75)
    ),
    titlePanel(
      "Mapa da prevalência ao nascimento considerandos os grupos de CIDs, ano e macrorregiões de saúde selecionados"
    ),
    fluidRow(column(
      12,
      selectizeInput(
        "ano_grafico_cid",
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
    )),
    br(),
    fluidRow(
      box(
        title = "Selecione o(s) grupo(s) de CID(s):",
        checkboxGroupInput(label = NULL,
          "checkbox_cid",
          selected = 1,
          choiceNames = cids_values2,
          choiceValues = 1:9
        ),
        background = "blue",
        width = 12
      )
    ),
    fluidRow(
      valueBoxOutput("box_populacao_cid", width = 3),
      valueBoxOutput("box_numero_casos_cid", width = 3),
      valueBoxOutput("box_prevalencia_cid", width = 3)
    ),
    fluidRow(
      box(
        h5("Selecione o tipo de agrupamento"),
        radioButtons(
          "group_regiao_cid",
          label = NULL,
          choices = list("Municípios" = 1,
                         #"Mesorregiões" = 2,
                         "Macrorregiões de saúde" =
                           3),
          selected = 1,
          inline = T
        ),
        title = "Mapa das prevalências ao nascimento de anômalias congênitas considerando os grupos de CIDs, ano e macrorregiões de saúde selecionados",
        background = "blue",
        width = 12,
        leafletOutput("grafico_mapa_proporcao_cid", height = "700px"),
        collapsible = TRUE
      )
    ),
    fluidRow(
      box(
        title = "Gráfico de barras dos 20 municipios com maior prevalencia considerando os grupos de CIDs, ano e macrorregiões de saúde selecionados",
        width = 6,
        background = "blue",
        plotlyOutput("grafico_barras_cid"),
        collapsible = TRUE
        
      ),
      box(
        title = "Serie temporal da prevalência ao nascimento considerando os grupos de CIDs e macrorregiões de saúde selecionados",
        width = 6,
        background =  "blue",
        plotlyOutput("grafico_serie_cid"),
        collapsible = TRUE
      )
    ), 
    fluidRow(
      box(
        title = "Gráfico de densidade da prevalência ao nascimento ao longo dos anos considerando os grupos de CIDs, ano e macrorregiões de saúde selecionados",
        background = "blue",
        sliderInput(
          "limite_dots_cid",
          "Limites do eixo vertical",
          min = 0,
          max = limites_prevalencia,
          value = c(0, limites_prevalencia),
          step = 1
        ),
        plotlyOutput("plot_dots_cid"),
        width = 12,
        collapsible = TRUE
      )
    ), 
    fluidRow(
      box(
        title = "Gráfico da evolução da prevalência ao nascimento considerando os grupos de CIDs e macrorregiões de saúde selecionados",
        background = "blue",
        htmlOutput("input_quadradinhos_html_cid"),
        plotlyOutput("plot_quadradinhos_cid"),
        width = 12,
        collapsible = TRUE
      )
    ), 
    # fluidRow(
    #   box(
    #     title = "Gráfico de Área do Número de nascidos vivos com anomalia congênita por grupo de CID considerando os grupos de CIDs e macrorregiões de saúde selecionados",
    #     background = "blue",
    #     plotlyOutput("plot_area_chart_cid"),
    #     width = 12,
    #     collapsible = TRUE
    #   )
    # ), 
    fluidRow(column(
      h2(
        "Tabela de prevalência ao nascimento agrupada por cada grupo de CID considerando as macrorregiões de saúde selecionadas"
      ),
      width = 12,
      dataTableOutput("tabela_cid_1", height = 500),
      downloadButton("downloadData_cid_1", "Download Tabela de dados"),
      HTML("<br><br><br>")
    )), 
    fluidRow(column(
      h2(
        "Tabela de prevalência ao nascimento com apenas os grupos de CID's e macrorregiões de saúde selecionados"
      ),
      width = 12,
      dataTableOutput("tabela_cid_2", height = 500),
      downloadButton("downloadData_cid_2", "Download Tabela de dados"),
      HTML("<br><br><br>")
    )), 
    fluidRow(column(
      width = 6,
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
