library(tidyverse)
library(sf)
library(stringr)
library(readxl)
library(leaflet)
library(RColorBrewer)


banco_nascimentos <- read_excel("DNRS201018_selected_17-jul-2020.xlsx") %>%
  pivot_longer(c(3: 11), names_to = "ANO_NASC", values_to = "numero_nascidos_vivos")%>%
  filter(complete.cases(.)) %>%
  filter(NOMEMUN != "Munic?pio ignorado - RS")

banco_anomalias <- read_excel("DNRS201018_selected_17-jul-2020.xlsx", 
                              sheet = "LONG_DATA")%>%
  select(1:5) %>%
  mutate(ANO_NASC = as.character(ANO_NASC), 
         CODMUNRES = as.character(CODMUNRES))%>%
  filter(complete.cases(.)) %>%
  filter(NOMEMUN != "Munic?pio ignorado - RS")

aux_banco_anomalias = banco_anomalias %>%
  group_by(NOMEMUN, ANO_NASC, CODMUNRES) %>%
  summarise(contagem= n()) 

banco_anomalias_analise = banco_nascimentos %>% 
  left_join(aux_banco_anomalias) %>%
  mutate(municipio = str_to_lower(NOMEMUN)) %>%
  mutate(contagem = replace_na(contagem, 0)) %>%
  #  na.omit() %>%
  #  drop_na() %>%
  mutate(prevalencia = (contagem/numero_nascidos_vivos)*10000) %>%
  mutate(prevalencia = replace(prevalencia, is.na(prevalencia), 0)) %>%
  mutate(ANO_NASC = as.numeric(ANO_NASC)) 


mapa_rs <- sf::st_read("shapefiles/43MUE250GC_SIR.shp", quiet = TRUE) %>%
  mutate(municipio= str_to_lower(NM_MUNICIP))
linha_lagoa_dos_patos=  which(mapa_rs$municipio=="lagoa dos patos") 
linha_lagoa_mirin=  which(mapa_rs$municipio=="lagoa mirim")
mapa_rs = mapa_rs[-c(linha_lagoa_dos_patos, linha_lagoa_mirin), ]

banco_i_moran <- banco_anomalias_analise %>%
  right_join(mapa_rs) 
banco_i_moran = st_as_sf(banco_i_moran)
banco_i_moran <- st_transform(banco_i_moran, "+init=epsg:4326") ##leaflet


## I de Moran
#install.packages("spdep")
library(spdep)

banco_i_moran_matriz = banco_i_moran %>%
                        filter(ANO_NASC == 2018)

w <- poly2nb(banco_i_moran_matriz$geometry, row.names = banco_i_moran_matriz$municipio)
ww <-  nb2listw(w, style = 'B') #faz a matriz de pesos 0 ou 1


banco_moran = banco_i_moran %>% filter(ANO_NASC == 2010)
moran_2010=moran.mc(banco_moran$prevalencia, ww, nsim = 99999)
valores_teste=c(moran_2010$statistic[[1]], moran_2010$p.value, 2010) 

for (i in 2011:2018)
{
  aux_banco_i_moran = banco_i_moran %>%
                        filter(ANO_NASC == i)        
  moran_teste = moran.mc(aux_banco_i_moran$prevalencia, ww, nsim = 99999)
  valores_teste = rbind(valores_teste,
                        c(moran_teste$statistic[[1]], moran_teste$p.value, i))
}

valores_teste_tabelado =  data.frame(estatistica = valores_teste[,1], 
                                     p_valor= valores_teste[,2], ano = valores_teste[,3])

write.table(valores_teste_tabelado, file = "teste_moran_rs.txt", row.names = FALSE, col.names = FALSE)
