aba_mapa_populacao <- tabItem(
  "mapa_populacao",
  fluidPage(
    fluidRow(
      tags$img(src = "logo_projeto_anomalias_git.png", height = 107 * .75),
      tags$img(
        src = "logo_parceiros_projeto.png",
        height = 107 * .75,
        width = 1275 * .75
      )
    ),
    fluidRow(
      tags$img(src = "ufrgs_logo.png", height = 107 * 0.75),
      tags$img(src = "logos_hcpa_ibc.png", height = 107 * 0.75),
      tags$img(src = "logo_ime.png", height = 107 * 0.75)
    ),
    titlePanel("Mapa dos número de nascidos vivos no ano selecionado"),
    fluidRow(column(
      12,
      selectizeInput(
        "ano_grafico_pop",
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
      valueBoxOutput("box_populacao_pop", width = 3),
      valueBoxOutput("box_numero_casos_pop", width = 3),
      valueBoxOutput("box_prevalencia_pop", width = 3)
    ),
    fluidRow(column(
      width = 12,
      box(
        title = "Mapa dos número de nascidos vivos no ano selecionado considerando apenas o ano e as macrorregiões de saúde selecionados",
        leafletOutput("mapa_pop", height = "700px"),
        width = 12,
        background = "blue"
      )
    )),
    fluidRow(column(
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
    )),
    fluidRow(column(
      width = 12,
      box(
        title = "Gráfico da evolução do número de nascidos vivos considerando apenas as macrorregiões de saúde selecionadas",
        background = "blue",
        htmlOutput("input_quadradinhos_html_pop"),
        plotlyOutput("plot_quadradinhos_pop"),
        width = 12
      )
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
