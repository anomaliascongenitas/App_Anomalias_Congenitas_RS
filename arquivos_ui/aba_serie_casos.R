aba_serie_casos <- tabItem(
  "serie_casos",
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
      "Série Temporal dos nascidos vivos com anomalias congenitas por município"
    ),
    fluidRow(column(h4(
      str_c(
        "O ícone no canto superior direito seleciona quais as macrorregiões de saúde ",
        preposicao_sigla_uf,
        " ",
        sigla_uf,
        " serão apresentadas"
      )
    ), width = 6)),
    br(),
    fluidRow(
      sidebarPanel(
        htmlOutput("input_quadradinhos_html_cidade2"),
        sliderInput(
          "limite2",
          "Limites do eixo vertical",
          min = 0,
          max = limites_contagem,
          value = c(0, limites_contagem),
          step = 1
        ),
        width = 12
      ),
      mainPanel(plotlyOutput("grafico_serie_casos", height = "600px"),
                width = 12)
    ),
    br(),
    br(),
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
