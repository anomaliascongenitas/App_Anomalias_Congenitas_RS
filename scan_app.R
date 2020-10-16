library(scanstatistics)
library(ggplot2)
library(dplyr)
library(sp)
library(magrittr)
library(stringr)
library(INLA)
library(rlist)

## here deve estar localizado na pasta principal onde esta o aplicativo Shiny (arquivo global.r)
localarquivo <- function(x){
  str_c(here::here(),"/",x)
}
source(str_c(here::here(),"/","global.r"),encoding = "UTF-8")


gerar_banco_modelo_aux <- function(cid2){
  
  banco_aux <- banco_cid %>%
    filter(as.numeric(cid) %in% cid2) %>%
    group_by(ANO_NASC,CODMUNRES) %>%
    summarise(NOMEMUN = unique(NOMEMUN),nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia),prevalencia = sum(nascidos_vivos_anomalia/unique(numero_nascidos_vivos))*10^4)
  
  banco_aux2 <- banco_nascimentos %>%
    left_join(banco_aux,by=c("CODMUNRES","ANO_NASC")) 
  
  
  banco_aux2 <- banco_aux2 %>%
    replace_na(list(nascidos_vivos_anomalia = 0, prevalencia = 0)) %>%
    select(1,4,NOMEMUN = NOMEMUN.x,5,nascidos_vivos_anomalia = nascidos_vivos_anomalia, 7)
  
  linha_pinto_bandeira  <- banco_aux2[banco_aux2$NOMEMUN == "Pinto Bandeira",]
  num_linha_bento_goncalves <- which(banco_aux2$NOMEMUN == "Bento Gonçalves")
  banco_aux2[num_linha_bento_goncalves,4:5] = banco_aux2[num_linha_bento_goncalves,4:5] + linha_pinto_bandeira[,4:5]
  
  
  banco_aux2 <- banco_aux2 %>%
    filter(NOMEMUN != "Pinto Bandeira")
  
  return(banco_aux2)
}


mapa_rs_modelo <- sf::st_read(str_c(here::here(),"/shapefiles/43MUE250GC_SIR_2010.shp"), quiet = TRUE) %>%
  mutate(municipio= str_to_lower(NM_MUNICIP))


linha_lagoa_dos_patos=  which(mapa_rs_modelo$municipio=="lagoa dos patos") 
linha_lagoa_mirin=  which(mapa_rs_modelo$municipio=="lagoa mirim")
mapa_rs_modelo  = mapa_rs_modelo[-c(linha_lagoa_dos_patos, linha_lagoa_mirin), ]


lista_completa <- list()






for (i in 1:8) {
  banco_modelo <- gerar_banco_modelo_aux(i)
  
  
  counts <- banco_modelo  %>% 
    select(1,2,5) %>%
    df_to_matrix(time_col = "ANO_NASC", location_col = "CODMUNRES", value_col = "nascidos_vivos_anomalia")
  
  
  # Remove Cibola since cases have been counted towards Valencia. Ideally, this
  # should be accounted for when creating the zones.
  
  banco_modelo$municipio <- str_to_lower(banco_modelo$NOMEMUN) 
  
  banco_modelo_aux <- banco_modelo %>%
    group_by(CODMUNRES) %>%
    summarise(municipio = str_to_lower(unique(NOMEMUN)))
  
  
  banco_modelo_mapa <-  banco_modelo_aux %>%
    left_join(mapa_rs_modelo,by=c("municipio"))%>%
    select(-3,-4)
  
  
  
  banco_modelo_mapa <- banco_modelo_mapa %>%
    arrange(CODMUNRES)
  
  banco_modelo_mapa_sf  = st_as_sf(banco_modelo_mapa)
  banco_modelo_mapa_sf  <- st_transform(banco_modelo_mapa_sf )
  nb <- poly2nb(banco_modelo_mapa_sf)
  nb2INLA("map.adj", nb)
  g <- inla.read.graph(filename = localarquivo("modelagem/Scan/map.adj"))
  
  
  proporcao_media <- sum(banco_modelo$nascidos_vivos_anomalia)/sum(banco_modelo$numero_nascidos_vivos)
  banco_modelo$valor_esperado <- banco_modelo$numero_nascidos_vivos*proporcao_media
  
  ebp_baselines <- banco_modelo  %>% 
    #df_to_matrix(value_col = "mu")
    df_to_matrix(time_col = "ANO_NASC", location_col = "CODMUNRES", value_col = "valor_esperado")
  
  
  
  set.seed(1)
  poisson_result <- scan_eb_poisson(counts = counts, 
                                    zones = g$nbs , #max_duration = 3,
                                    baselines = ebp_baselines,
                                    n_mcsim = 999)
  print(poisson_result)
  poisson
  
  
  
  lista2 <- banco_modelo_aux %>%
    filter(CODMUNRES %in% colnames(counts[,poisson_result$MLC$locations]))
  
  lista2
  
  
  
  lista <- banco_modelo_aux %>%
    filter(CODMUNRES %in% colnames(counts[,poisson_result$MLC$locations]))
  
  
  
  
  
  
  county_scores <- score_locations(poisson_result, g$nbs)
  lista_completa[[i]] <- list(MC_pvalue = poisson_result,cluster = lista2,relative_score = county_scores)
}





lista_completa[[9]] <- banco_modelo




save(lista_completa, file="scan_app.RData")


load("scan_app.RData")
