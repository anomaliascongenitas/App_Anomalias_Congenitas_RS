
banco_cardio <- read_excel(localarquivo("dados//2010-2019//Cardiopatias Congenitas 2010-2019.xlsx")) %>%
  filter(UFRES == "RS") %>%
  select(NUMERODN,ANONASC,CODMUNRES) %>%
  mutate(cid_num = 1, cid = "Cardiopatias congênitas")


banco_par_abdom <- read_excel(localarquivo("dados//2010-2019//Defeitos de Parede Abdominal 2010-2019.xlsx")) %>%
  filter(UFRES == "RS") %>%
  select(NUMERODN,ANONASC,CODMUNRES) %>%
  mutate(cid_num = 2, cid = "Defeitos de parede abdominal")


banco_def_membros <- read_excel(localarquivo("dados//2010-2019//Defeitos de Membros 2010-2019.xlsx")) %>%
  filter(UFRES == "RS") %>%
  select(NUMERODN,ANONASC,CODMUNRES) %>%
  mutate(cid_num = 3, cid = "Defeitos de redução de membros/ pé torto/ artrogripose / polidactilia")


banco_tubo_neural <- read_excel(localarquivo("dados//2010-2019//Defeitos do Tubo Neural 2010-2019.xlsx")) %>%
  filter(UFRES == "RS") %>%
  select(NUMERODN,ANONASC,CODMUNRES) %>%
  mutate(cid_num = 4, cid = "Defeitos de Tubo Neural")


banco_fendas_orais <- read_excel(localarquivo("dados//2010-2019//Fendas Orais 2010-2019.xlsx")) %>%
  filter(UFRES == "RS") %>%
  select(NUMERODN,ANONASC,CODMUNRES) %>%
  mutate(cid_num = 5, cid = "Fendas orais")


banco_hispospadia <- read_excel(localarquivo("dados//2010-2019//Anomalias de Orgaos Genitais 2010-2019.xlsx"),sheet = "Q54") %>%
  filter(UFRES == "RS") %>%
  select(NUMERODN,ANONASC,CODMUNRES) %>%
  mutate(cid_num = 6, cid = "hipospadia")

banco_microcefalia <- read_excel(localarquivo("dados//2010-2019//Microcefalia 2010-2019.xlsx")) %>%
  filter(UFRES == "RS") %>%
  select(NUMERODN,ANONASC,CODMUNRES) %>%
  mutate(cid_num = 7, cid = "Microcefalia")

banco_sexo_indef <- read_excel(localarquivo("dados//2010-2019//Anomalias de Orgaos Genitais 2010-2019.xlsx"),sheet = "Q56") %>%
  filter(UFRES == "RS") %>%
  select(NUMERODN,ANONASC,CODMUNRES) %>%
  mutate(cid_num = 8, cid = "Sexo indefinido")

banco_down <- read_excel(localarquivo("dados//2010-2019//Sindrome de Down 2010-2019.xlsx"),range = "A1:Z10477") %>%
  filter(UFRES == "RS") %>%
  select(NUMERODN,ANONASC,CODMUNRES)  %>%
  mutate(cid_num = 9, cid = "Síndrome de Down")







banco_completo_aux <- data.frame(rbind(banco_microcefalia,banco_tubo_neural,banco_fendas_orais,
                                   banco_def_membros,banco_hispospadia,banco_par_abdom,
                                   banco_cardio,banco_down,banco_sexo_indef))

# banco_completo_munic <- banco_completo_aux  %>%
#   group_by(ANONASC,CODMUNRES) %>%
#   summarise(contagem = n())
# 
# sum(banco_completo_munic$contagem)
# 
# 
# banco_resum <- banco_completo_aux %>%
#   group_by(NUMERODN) %>%
#   summarise(ANONASC = unique(ANONASC),CODMUNRES = unique(CODMUNRES)) %>%
#   dplyr::ungroup()
  


write.csv2(x = banco_completo_aux,file= localarquivo("banco_anomalias_2010-2019.csv"),row.names = FALSE)

#banco <- read.csv2(file= localarquivo("banco_anomalias_2010-2019.csv"))









# 
# banco_2019_analise <- banco_2019 %>%
#   left_join(banco_nascimentos,by = c(c("CODMUNRES" = "CODMUNRES") ,c("ANONASC" = "ANO_NASC"))) %>%
#   select(-Total)



# 
# 
# banco_aux <- banco_2019 %>%
#   filter(cid_num %in% 1:9) %>%
#   group_by(NUMERODN) %>%
#   summarise(ANONASC = unique(ANONASC),CODMUNRES = unique(CODMUNRES),nascidos_vivos_anomalia = n())
# 
# banco_aux2 <- banco_aux %>%
#   group_by(ANONASC,CODMUNRES) %>%
#   summarise(nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia))
# 
# 
# banco_aux3 <- banco_nascimentos %>%
#   left_join(banco_aux2,by  = c("CODMUNRES","ANO_NASC" = "ANONASC")) %>%
#   mutate(nascidos_vivos_anomalia = replace_na(nascidos_vivos_anomalia, 0),prevalencia = nascidos_vivos_anomalia/numero_nascidos_vivos*10^4)
# sum(banco_aux3$nascidos_vivos_anomalia,na.rm = TRUE)

