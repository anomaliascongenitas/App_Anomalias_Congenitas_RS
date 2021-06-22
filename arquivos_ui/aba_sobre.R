aba_sobre <- tabItem(
  "sobre",
  fluidPage(
    fluidRow(
      #tags$img(src = "logo_projeto_anomalias_git.png", height = 107 * 0.75),
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
    br(),
    fluidRow(
      userBox(
        title = userDescription(
          title = "Projeto Anomalias Congênitas",
          image = "logo_projeto_anomalias_git.png",
          type = 2,imageElevation = 100
        ),
        h4(str_c(
          "Esse Aplicativo foi desenvolvido pelo grupo de epidemiologia do Projeto Anomalias Congênitas e tem como objetivo
                                                                                    mostrar a distribuição das Anomalias Congênitas por município de  residência da mãe do Estado ",preposicao_sigla_uf," ", uf_extenso,"."
        )),
        h4(
          "Fonte de dados: Sistema de Informação sobre Nascidos Vivos (SINASC)"
        ),
        type = 2,
        collapsible = TRUE,
        status = "primary",
        width = 12
      ),
      h2("Equipe de desenvolvimento do Aplicativo")
    ),
    fluidRow(
      userBox(
        title = userDescription(
          title = "Márcia Helena Barbian",
          subtitle = "Professora do Departamento de Estatística da UFRGS",
          image = 'marcia.png',
          type = 2,
        ),

        width = 4,
        height = 180,
        image = 'marcia.png',
        status = "primary",
        "Contato: mhbarbian@ufrgs.br",
        #footer_padding = F,
        closable =  FALSE
      ),
      userBox(
        title =  userDescription(
          title = "Bruno Alano da Silva",
          subtitle = "Estudante de Estatística da UFRGS",
          image = 'bruno.jpg',
          type = 2
        ),
        width = 4,
        #height = 180,
        status = "primary",
        "Contato: alano.bruno31@gmail.com",
        closable =  FALSE
      ),
      userBox(
        title = userDescription(
          title = "Guilherme Rodrigues Boff",
          subtitle = "Estudante de Estatística da UFRGS",
          image = 'guilherme.jpeg',
          type = 2
        ),
        width = 4,
        #height = 180,
        "Contato: guilherme_rboff@hotmail.com",
        footer_padding = F,
        status = "primary",
        closable =  FALSE
      )
    ),
    fluidRow(
      userBox(
        title = userDescription(
          title = "Fernanda Sales Luiz Vianna",
          subtitle = "Professora do Departamento de Genética da UFRGS",
          image = 'fernanda.gif',
          type = 2
        ),
        width = 4,
        #height = 180,
        status = "primary",
        "Contato: fslvianna@gmail.com",
        footer_padding = F,
        closable =  FALSE
      ),
      userBox(
        title = userDescription(
          title = "Luiza Monteavaro Mariath",
          subtitle = "Doutora em Ciências (Genética e Biologia Molecular) pelo PPGBM da UFRGS",
          image = 'luiza.jpg',
          type = 2
        ),
        width = 4,
        #height = 180,
        status = "primary",
        "Contato: luiza_mariath@hotmail.com",
        footer_padding = F,
        closable =  FALSE
      ),
      userBox(
        title = userDescription(
          title = "Thayne Woycinck Kowalski",
          subtitle = "Professora do Centro Universitário CESUCA, pesquisadora do Núcleo de Bioinformática do HCPA",
          image = 'thayne.jpg',
          type = 2
        ),

        width = 4,
        #height = 180,
        status = "primary",
        "Contato: thaynewk@gmail.com",
        footer_padding = F,
        closable =  FALSE
      )
    ),
    fluidRow(
      h2("Coordenadora do Projeto"),
      userBox(
        title = userDescription(
          title = "Lavinia Schüler Faccini",
          subtitle = "Professora do Departamento de Genética da UFRGS",
          image = 'lavinia.jpg',
          type = 2
        ),

        width = 4,
        #height = 180,
        status = "primary",
        "Contato: lschuler@hcpa.edu.br",
        footer_padding = F,
        closable =  FALSE
      )
    ),
    fluidRow(
      h2("Código Fonte"),
      valueBox(
        "Repositório Github",
        subtitle = div(
          "Confira aqui nosso repositório no GitHub!",
          br(),
          "Contato: projeto.anomalias.congenitas@gmail.com"
        ),
        icon = icon("github"),
        color = "green",
        width = 12,
        href = "https://github.com/anomaliascongenitas/App_Anomalias_Congenitas_RS"
      )
    ),
    br(),
    fluidRow(column(
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
