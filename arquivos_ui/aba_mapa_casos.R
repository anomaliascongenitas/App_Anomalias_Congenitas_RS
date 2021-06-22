aba_mapa_casos <- tabItem(
  "mapa_n_casos",
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
    ),
    titlePanel(
      "Mapa do número de nascidos vivos com anomalias congênitas no ano selecionado"
    ),
    fluidRow(column(
      12,
      selectizeInput(
        "ano_grafico_casos",
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
      valueBoxOutput("box_populacao_casos", width = 3),
      valueBoxOutput("box_numero_casos_casos", width = 3),
      valueBoxOutput("box_prevalencia_casos", width = 3)
    ),
    fluidRow(column(
      width = 12,
      box(
        title = "Mapa do número de nascidos vivos com anomalias congênitas considerando apenas o ano e as macrorregiões de saúde selecionados",
        leafletOutput("mapa_n_casos", height = "700px"),
        width = 12,
        background = "blue"
      )
    )),
    fluidRow(column(
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
    fluidRow(column(
      width = 12,
      box(
        title = "Gráfico da evolução do número de nascidos com anomalias
              20 municipios com maior número de nascidos vivos no RS considerando apenas as macrorregiões de saúde selecionadas",
        background = "blue",
        htmlOutput("input_quadradinhos_html_casos"),
        plotlyOutput("plot_quadradinhos_casos"),
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
