banco_macro_saude_aux <- read.csv("MACRORREGIOES_DE_SAUDE.csv") %>%
  select(IBGE,macrorregiao)



# banco_macro_aux <- banco_anomalias_analise %>%
#   filter(ANO_NASC == "2018") %>%
#   left_join(mapa_rs)
# 
# banco_macro_saude_final <- merge(x = banco_macro_saude, y = banco_macro_aux, by.x = "Código IBGE",by.y = "CODMUNRES")
# View(banco_macro_saude_final)
# 
# 
# banco_macro_saude_final_ultimate <- banco_macro_saude_final %>%
#   select("Código IBGE","Município","Macrorregião","geometry")
# 
# banco_macro_saude_final_ultimate$macro <- as.numeric(as.factor(banco_macro_saude_final_ultimate$Macrorregião))
# 
# sf::st_write(banco_macro_saude_final_ultimate,"shapefiles/macro_saude.shp", quiet = TRUE)



macro_saude_shape <- sf::st_read("shapefiles/micro_saude_rs/micro_saude_rs.shp", quiet = TRUE) 
names(macro_saude_shape) <- c("IBGE","municipio","macroregiao","macroregiao_num","geometry")

macro_saude_shape <- macro_saude_shape[,3:5]

banco_aux <- banco_anomalias_analise %>%
  filter(ANO_NASC == "2018") %>%
  left_join(banco_macro_saude_aux, by=c("CODMUNRES" = "IBGE"))




banco_aux2 <- banco_aux %>%
  group_by(macrorregiao) %>%
  summarise(numero_nascidos_vivos = sum(numero_nascidos_vivos),
            nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia),
            prevalencia = nascidos_vivos_anomalia/numero_nascidos_vivos*10^4)


banco_grafico <- left_join(banco_aux2,macro_saude_shape,by  = c("macrorregiao"= "macroregiao"))


pal <- colorBin("plasma", domain = banco_grafico$prevalencia, bins = bins_defalt$brks)
pal2 <- function(x){
  ifelse(x==0,"#808080",pal(x))
}

names(tidy)[2] <- "variavel"

tidy = st_as_sf(banco_grafico)
tidy <- st_transform(tidy, "+init=epsg:4326")


leaflet(tidy) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addPolygons(fillColor = ~pal2(prevalencia), 
              weight = 1.5,
              opacity = 0.7,
              fillOpacity = 0.7,
              color = "gray",
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = sprintf("<strong>%s</strong><br/>Prevalência:
                                  %s",
                              tidy$macrorregiao, round(tidy$prevalencia,3)) %>%
                lapply(htmltools::HTML),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "6px 11px"),
                textsize = "13px",
                direction = "bottom")) %>%
  leaflet::addLegend(pal = pal, values = ~tidy$prevalencia, opacity = 0.7, title = "Prevalência",
                     labFormat = labelFormat(digits = 3),
                     position = "bottomright")

