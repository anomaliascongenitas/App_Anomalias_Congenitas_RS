
dataset_box_filtro <- reactive({
  vetor = c()
  vetor[1] = sum(dataset_anomalia_analise_ano_filtro()$numero_nascidos_vivos)
  vetor[2] = sum(dataset_anomalia_analise_ano_filtro()$nascidos_vivos_anomalia)
  vetor[3] = ifelse(vetor[1] != 0 ,round((vetor[2]/vetor[1])*10^4,3),0)
  vetor
})

dataset_box_filtro_casos <- reactive({
  vetor = c()
  vetor[1] = sum(dataset_anomalia_analise_casos_ano_filtro()$numero_nascidos_vivos)
  vetor[2] = sum(dataset_anomalia_analise_casos_ano_filtro()$nascidos_vivos_anomalia)
  vetor[3] = ifelse(vetor[1] != 0 ,round((vetor[2]/vetor[1])*10^4,3),0)
  vetor
})

dataset_box_filtro_pop <- reactive({
  vetor = c()
  vetor[1] = sum(dataset_anomalia_analise_pop_ano_filtro()$numero_nascidos_vivos)
  vetor[2] = sum(dataset_anomalia_analise_pop_ano_filtro()$nascidos_vivos_anomalia)
  vetor[3] = ifelse(vetor[1] != 0 ,round((vetor[2]/vetor[1])*10^4,3),0)
  vetor
})


################ Box ###########################



###################### Box aba cid ####################

output$box_populacao_cid <- renderValueBox({
  
  valueBox(
    sum(datasetInputcid_ano()$numero_nascidos_vivos),
    "Total nascidos vivos",
    icon = icon("baby",lib = "font-awesome"),
    color = "blue"
  )
})


output$box_numero_casos_cid <- renderValueBox({
  valueBox(
    sum(datasetInputcid_ano()$nascidos_vivos_anomalia),
    "Total nascidos vivos com anomalias congenitas",
    icon = icon("notes-medical",lib = "font-awesome"),
    color = "red"
  )
})

output$box_prevalencia_cid <- renderValueBox({
  valueBox(
    round(sum(datasetInputcid_ano()$nascidos_vivos_anomalia)/sum(datasetInputcid_ano()$numero_nascidos_vivos)*10^4,3),
    "Prevalência ao nascimento no RS por 10000",
    icon = icon("notes-medical"),
    color = "purple"
  )
})



###################### Box aba prevalencia ####################


output$box_populacao <- renderValueBox({
  valueBox(
    dataset_box_filtro()[1],
    "Total nascidos vivos",
    icon = icon("baby",lib = "font-awesome"),
    color = "blue"
  )
})


output$box_numero_casos <- renderValueBox({
  valueBox(
    dataset_box_filtro()[2],
    "Total nascidos vivos com anomalias congenitas",
    icon = icon("notes-medical"),
    color = "red"
  )
})


output$box_prevalencia <- renderValueBox({
  valueBox(
    dataset_box_filtro()[3],
    "Prevalência ao nascimento no RS por 10000",
    icon = icon("notes-medical"),
    color = "purple"
  )
})





###################### Box aba casos ####################


output$box_populacao_casos <- renderValueBox({
  valueBox(
    dataset_box_filtro_casos()[1],
    "Total nascidos vivos",
    icon = icon("baby",lib = "font-awesome"),
    color = "blue"
  )
})

output$box_numero_casos_casos <- renderValueBox({
  valueBox(
    dataset_box_filtro_casos()[2],
    "Total nascidos vivos com anomalias congenitas",
    icon = icon("notes-medical"),
    color = "red"
  )
})

output$box_prevalencia_casos <- renderValueBox({
  valueBox(
    dataset_box_filtro_casos()[3],
    "Prevalência ao nascimento no RS por 10000",
    icon = icon("notes-medical"),
    color = "purple"
  )
})


###################### Box aba nascidos vivos ####################


output$box_populacao_pop <- renderValueBox({
  valueBox(
    dataset_box_filtro_pop()[1],
    "Total nascidos vivos",
    icon = icon("baby",lib = "font-awesome"),
    color = "blue"
  )
})

output$box_numero_casos_pop <- renderValueBox({
  valueBox(
    dataset_box_filtro_pop()[2],
    "Total nascidos vivos com anomalias congenitas",
    icon = icon("notes-medical"),
    color = "red"
  )
})

output$box_prevalencia_pop <- renderValueBox({
  valueBox(
    dataset_box_filtro_pop()[3],
    "Prevalência ao nascimento no RS por 10000",
    icon = icon("notes-medical"),
    color = "purple"
  )
})







