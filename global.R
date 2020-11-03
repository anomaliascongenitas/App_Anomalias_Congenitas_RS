#library(profvis)
#library(Rcpp)
library(readxl)
#library(RColorBrewer) ??
#library(dplyr)
#library(reshape2)
#library(dygraphs)
#library(xts)
#library(ggrepel)


library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(DT)
library(leaflet)
library(tidyverse)
library(hrbrthemes)
library(plotly)
library(sf)

library(ps)
library(spdep) #######?###########
library(kableExtra)

library(viridis)

#library(haven)
library(ggbeeswarm)
#library(stringr)
################################################
options(OutDec= ".") #Muda de ponto para virgula nos decimais! 


localarquivo <- function(x){
  str_c(here::here(),"/",x)
}

################################################################################
################
################  lendo e adaptando os bancos de dados
################
################################################################################

banco_nascimentos <- read_excel(localarquivo("DNRS201018_selected_17-jul-2020.xlsx")) %>%
  pivot_longer(c(3: 11), names_to = "ANO_NASC", values_to = "numero_nascidos_vivos")%>%
  filter(complete.cases(.)) %>%
  filter(CODMUNRES != 430000) %>%
  mutate(CODMUNRES = as.numeric(CODMUNRES))  %>%
  mutate(ANO_NASC = as.numeric(ANO_NASC)) ## Municipio ignorado


### Cuidado pois o número de linhas e colunas dessa tabela foi fixado, qualquer mudança e o valor deverá ser alterado! ###

# banco_anomalias <- read_excel(localarquivo("DNRS201018_selected_17-jul-2020.xlsx"), 
#                               sheet = "LONG_DATA",range = "A1:E8585")%>%
#   select(1:5) %>%
#   mutate(ANO_NASC = as.character(ANO_NASC), 
#          CODMUNRES = as.character(CODMUNRES))%>%
#   filter(complete.cases(.)) %>%
#   filter(CODMUNRES != 430000) %>%
#   mutate(CODMUNRES = as.numeric(CODMUNRES)) %>%
#   mutate(ANO_NASC = as.numeric(ANO_NASC)) ## Municipio ignorado
# 
# 
# 





banco_2019 <- read.csv2(file= localarquivo("banco_anomalias_2010-2019.csv")) %>%
  rename(ANO_NASC = ANONASC)

banco_cid <- banco_2019

banco_aux <- banco_2019 %>%
  # filter(cid_num %in% 1:9) %>%
  group_by(NUMERODN) %>%
  summarise(ANO_NASC = unique(ANO_NASC),CODMUNRES = unique(CODMUNRES),nascidos_vivos_anomalia = 1)

banco_aux2 <- banco_aux %>%
  group_by(ANO_NASC,CODMUNRES) %>%
  summarise(nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia))


banco_aux3 <- banco_nascimentos %>%
  left_join(banco_aux2,by  = c("CODMUNRES","ANO_NASC" = "ANO_NASC")) %>%
  mutate(nascidos_vivos_anomalia = replace_na(nascidos_vivos_anomalia, 0),prevalencia = nascidos_vivos_anomalia/numero_nascidos_vivos*10^4) %>%
  mutate(prevalencia = ifelse(is.nan(prevalencia),0,prevalencia)) # Pinto Bandeira 2010-2012



banco_anomalias_analise <- banco_aux3 

banco_anomalias_analise$municipio <- str_to_lower(banco_anomalias_analise$NOMEMUN)

remove(banco_aux,banco_aux2,banco_aux3)

### Banco CID

# classificar_cid <- function(x){
#   aux = substr(x,1,3)
#   
#   ifelse(aux=="Q02",1,
#          ifelse(aux %in% c("Q01", "Q05"),2,
#                 ifelse(aux %in% c("Q35","Q36","Q37"),3,
#                        ifelse(aux %in% c("Q71", "Q72", "Q73",  "Q66", "Q69"),4,
#                               ifelse(aux %in% c("Q54"),5,
#                                      ifelse(aux %in% c("Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28"),7,
#                                             ifelse(aux %in% c("Q90",8,0),8,
#                                                    ifelse(aux %in% c("Q56"),9,0))
#                                      )
#                               )
#                        )
#                 )
#          ))
# }


# 
# cid_1 <- classificar_cid(banco_anomalias$CODANOMAL)
# 
# 
# classificar_cid2 <- function(x){
#   ifelse(x %in% c("Q000","Q001","Q002"),2,
#          ifelse(x == "Q743", 4, 
#                 ifelse(x %in% c("Q793","Q792"),6,0)))
# }
# 
# cid_2 <- classificar_cid2(banco_anomalias$CODANOMAL)
# 
# cid_1[cid_1 == 0] = cid_2[cid_1 == 0]
# 
# 
# banco_cid_aux <- banco_anomalias
# 
# banco_cid_aux$cid = factor(cid_1,levels = 1:9,labels =  c("Microcefalia","Defeitos de Tubo Neural","Fendas orais"
#                                                       ,"Defeitos de redução de membros/ pé torto/ artrogripose / polidactilia","hipospadia"
#                                                       ,"Defeitos de parede abdominal","Cardiopatias congênitas","Síndrome de Down","Sexo indefinido"))
# 
# 
# 
# 
# 
# banco_cid_aux2 <- banco_cid_aux %>%
#   group_by(ANO_NASC, CODMUNRES,cid) %>%
#   summarise(NOMEMUN = unique(NOMEMUN),nascidos_vivos_anomalia = n())
# 
# 
# 
# 
# 
# 
# banco_cid <- banco_cid_aux2 %>%
#   left_join(banco_nascimentos,by = c("ANO_NASC","CODMUNRES")) 
# 
# 
# 
# banco_cid <- banco_cid %>% 
#   ungroup() %>%
#   select(ANO_NASC,CODMUNRES,NOMEMUN = NOMEMUN.x,cid, numero_nascidos_vivos,nascidos_vivos_anomalia)
# 
# 


#### Banco anomalia analises


# aux_banco_anomalias = banco_anomalias %>%
#   group_by(NOMEMUN, ANO_NASC, CODMUNRES) %>%
#   summarise(nascidos_vivos_anomalia= n()) 
# 
# 
# 
# banco_anomalias_analise = banco_nascimentos %>% 
#   left_join(aux_banco_anomalias,by = c("CODMUNRES","ANO_NASC")) %>%
#   mutate(NOMEMUN = NOMEMUN.x) %>%
#   subset(select = -c(NOMEMUN.x,NOMEMUN.y)) %>%
#   mutate(municipio = str_to_lower(NOMEMUN)) %>%
#   mutate(nascidos_vivos_anomalia = replace_na(nascidos_vivos_anomalia, 0)) %>%
#   #  na.omit() %>%
#   #  drop_na() %>%
#   mutate(prevalencia = (nascidos_vivos_anomalia/numero_nascidos_vivos)*10000) %>%
#   mutate(prevalencia = replace(prevalencia, is.na(prevalencia), 0)) 


mapa_rs <- sf::st_read(localarquivo("shapefiles/43MUE250GC_SIR.shp"), quiet = TRUE) %>%
  mutate(municipio= str_to_lower(NM_MUNICIP))
linha_lagoa_dos_patos=  which(mapa_rs$municipio=="lagoa dos patos") 
linha_lagoa_mirin=  which(mapa_rs$municipio=="lagoa mirim")
mapa_rs = mapa_rs[-c(linha_lagoa_dos_patos, linha_lagoa_mirin), ]

####################################################################################
### Intervalos cores mapa prevalencia
#Prevalencia
linha_ano_intervalo_defalt = which(banco_anomalias_analise$prevalencia==
                                     max(banco_anomalias_analise$prevalencia))
ano_intervalo_defalt = banco_anomalias_analise$ANO_NASC[linha_ano_intervalo_defalt][1]
variavel_intervalo = banco_anomalias_analise %>% 
  filter(ANO_NASC == ano_intervalo_defalt) %>%
  select(prevalencia)
bins_defalt = classInt::classIntervals(var = variavel_intervalo[[1]], n = 5, style = "fisher")

### nascidos_vivos_anomalia
linha_ano_intervalo_defalt_nascidos_vivos_anomalia = which(banco_anomalias_analise$nascidos_vivos_anomalia==
                                              max(banco_anomalias_analise$nascidos_vivos_anomalia))
ano_intervalo_defalt_nascidos_vivos_anomalia = banco_anomalias_analise$ANO_NASC[linha_ano_intervalo_defalt_nascidos_vivos_anomalia]
variavel_intervalo = banco_anomalias_analise %>% 
  filter(ANO_NASC == ano_intervalo_defalt_nascidos_vivos_anomalia) %>%
  select(nascidos_vivos_anomalia)


bins_defalt_nascidos_vivos_anomalia = classInt::classIntervals(var = variavel_intervalo[[1]], n = 5, style = "fisher")




anos <- 2010:2018
limites_nascidos_vivos_anomalia <- 253
limites_prevalencia <- 2223


#remove(classificar_cid,classificar_cid2,cid_1,cid_2,banco_cid_aux,banco_cid_aux2,aux_banco_anomalias)






####################################################################################
## Moran 
banco_i_moran_matriz = banco_anomalias_analise %>%
  filter(ANO_NASC == 2018) %>%
  right_join(mapa_rs)

w <- poly2nb(banco_i_moran_matriz$geometry, row.names=banco_i_moran_matriz$municipio)
matriz_w <-  nb2listw(w, style='B') #faz a matriz de pesos 0 ou 1
teste_moran <- read.table(localarquivo("teste_moran_rs.txt"), quote="\"", comment.char="")
names(teste_moran)=c("estatistica_teste", "p_valor", "ano_teste")
#####################################################################################################

top_20_munic <- banco_anomalias_analise %>%
  filter(ANO_NASC == 2018) %>%
  arrange(Total) %>%
  top_n(20, numero_nascidos_vivos) %>%
  select(NOMEMUN)

tabela_box <- banco_anomalias_analise %>%
  group_by(ANO_NASC) %>%
  summarise(total_nascidos_vivos = sum(numero_nascidos_vivos),
            total_anomalias = sum(nascidos_vivos_anomalia),
            prevalencia  = total_anomalias / total_nascidos_vivos*10^4)













#remove(banco_meso_analise_aux,banco_anomalias)


banco_macro_saude <- read.csv(localarquivo("MACRORREGIOES_DE_SAUDE.csv")) %>%
  select(IBGE,macrorregiao)

macro_saude_shape <- sf::st_read(localarquivo("shapefiles/macro_saude_rs/macro_saude_rs.shp"), quiet = TRUE) 
names(macro_saude_shape) <- c("IBGE","municipio","macroregiao","macroregiao_num","geometry")

macro_saude_shape <- macro_saude_shape[,3:5]


banco_macro_saude_analise_aux <- banco_anomalias_analise  %>%
  merge(.,banco_macro_saude,by.x=c("CODMUNRES"),by.y = c("IBGE"))


banco_macro_saude_analise <- banco_macro_saude_analise_aux  %>%
  group_by(macrorregiao,ANO_NASC) %>%
  summarise(numero_nascidos_vivos = sum(numero_nascidos_vivos), nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia),
            prevalencia = nascidos_vivos_anomalia/numero_nascidos_vivos*10^4) %>%
  ungroup() 



mapa_rs_modelo <- sf::st_read(str_c(here::here(),"/shapefiles/43MUE250GC_SIR_2010.shp"), quiet = TRUE) %>%
  mutate(municipio= str_to_lower(NM_MUNICIP))


linha_lagoa_dos_patos=  which(mapa_rs_modelo$municipio=="lagoa dos patos") 
linha_lagoa_mirin=  which(mapa_rs_modelo$municipio=="lagoa mirim")
mapa_rs_modelo  = mapa_rs_modelo[-c(linha_lagoa_dos_patos, linha_lagoa_mirin), ]




load("scan_app.RData")











