#library(profvis) ## Pacote para monitorar o desempenho do aplicativo

################################################
options(OutDec= ",") #Muda de ponto para virgula nos decimais! 




anos <- 2010:2019
limites_contagem <- round(max(banco_anomalias_analise$nascidos_vivos_anomalia)*1.2,0)
limites_prevalencia <- max(banco_anomalias_analise$prevalencia)*1.2
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
                  "Sexo indefinido CID Q56",
                  "Síndrome de Down – CID Q90")


header <- shinydashboardPlus::dashboardHeader(
  #enable_rightsidebar = T,
  controlbarIcon = shiny::icon("gears"),
  title = tagList(
    span(class = "logo-lg", str_c("Análise de nasc. vivos com anomalias congênitas ", preposicao_sigla_uf," ", sigla_uf)), 
    icon = icon("tachometer-alt")),
  titleWidth = 650
)

rightsidebar <- dashboardControlbar(icon = "desktop",
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
    menuItem("Mapa da prevalência ao nascimento", tabName = "mapa_prevalencia",icon = icon("globe-americas")),
    menuItem("Mapa nasc. vivos com anomalia", tabName = "mapa_n_casos",icon = icon("globe-americas")),
    menuItem("Mapa nasc. vivos", tabName = "mapa_populacao",icon = icon("globe-americas")),
    menuItem("Série Temp. prevalência ao nascimento", tabName = "serie_prevalencia",icon = icon("chart-line")),
    menuItem("Série Temp. nasc. vivos com anomalia", tabName = "serie_casos",icon = icon("chart-line")),
    menuItem("Estatística Scan", tabName = "scan",icon = icon("search-location")),
    menuItem("Sobre", tabName = "sobre",icon = icon("book"))
  ),
  width = 300
)

source("arquivos_ui/aba_mapa_cid.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
source("arquivos_ui/aba_mapa_prevalencia.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
source("arquivos_ui/aba_mapa_casos.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
source("arquivos_ui/aba_mapa_populacao.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
source("arquivos_ui/aba_serie_casos.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
source("arquivos_ui/aba_serie_prevalencia.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
source("arquivos_ui/aba_scan.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
source("arquivos_ui/aba_sobre.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)


body <- dashboardBody(
  tags$head(tags$style(HTML(
    ".small-box {height: 115px}"
  ))),
  tabItems(
    aba_mapa_cid,
    aba_mapa_prevalencia,
    aba_mapa_casos,
    aba_mapa_populacao,
    aba_serie_casos,
    aba_serie_prevalencia,
    aba_scan,
    aba_sobre
  )
) 
#ys.setlocale(locale="")

shinyUI(dashboardPage(controlbar = rightsidebar, header = header, sidebar = sidebar, 
                          body = body))

