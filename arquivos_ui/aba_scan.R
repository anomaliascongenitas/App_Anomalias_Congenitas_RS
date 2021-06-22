aba_scan <- tabItem(
  "scan",
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
    titlePanel("Estatística Scan"),
    br(),
    tags$style(
      HTML(
        ".tabbable > .nav > li > a                  {background-color: #0073b6;  color:white; border-color: white}
         .tabbable > .nav > li[class=active]    > a {background-color: white; color:black}"
      )
    ),
    fluidRow(box(
      title = "Selecione o grupo de CID para a análise do resultado da Estatística Scan",
      radioButtons(label = NULL,
        "cid_scan",
        selected = 1,
        choiceNames  = cids_values2,
        choiceValues = 1:9
      ),
      background = "blue",
      width = 12
    )),
    fluidRow(
      box(
        uiOutput(outputId = "gerar_scan_select_top"),
        mainPanel(width = 12,
                  tabsetPanel(
                    type = "tabs",
                    tabPanel(title = "Versão Resumida", uiOutput("scan_result_texto_red"), br()),
                    tabPanel(title = "Versão Completa", uiOutput("scan_result_texto"), br())
                  )),
        title = "Resultado da Estatística Scan",
        background = "blue",
        width = 12
      )
    ),
    fluidRow(
      column(width = 2),
      column(
        align = "center",
        width = 8,
        box(
          title = "Mapa com o Cluster detectado pela Estatística Scan",
          plotOutput("mapa_scan_cluster", height = "500px"),
          br(),
          textOutput("scan_cluster_texto"),
          br(),
          width = 12,
          background = "blue",
          collapsible = TRUE
        )
      ),
      column(width = 2)
    ),
    fluidRow(column(
      width = 12,
      box(
        title = "Mapa do Escore Relativo da Estatística Scan por munícipio",
        leafletOutput("mapa_scan", height = "700px"),
        width = 12,
        background = "blue",
        collapsible = TRUE
      )
    )),
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
