semma <- read_csv("EXPORTADO/semma.csv")
colnames(semma)[16] = "errores"

# ----- SEMMA Exploración de estadísticos -----
print(semma%>% glimpse())

print(semma%>% skim())


# PROMEDIO DE ENCUESTAS: Factores de sesgo =====================================
ggplot(semma %>% 
         filter(as_date(date_elec) == "2019-11-10") %>% 
         # filter(poll_firm == "CIS") %>%
         filter(party == "PSOE"),
       aes(x=(as_date(date_elec) - days_to_elec), 
           y= est_surv_vote,
           size = n,
           color = abs(errores)), )+
  geom_point()+ 
  # scale_x_date(date_breaks= "4 month")+
  geom_smooth()  + 
  theme(axis.text.x = element_text(size = 6)) +
  labs(x = "Fecha") +
  geom_hline(yintercept = 28,
             linetype = 2,
             color = 2) +
  geom_vline(xintercept = as_date("2019-11-04"),
             linetype = 2,
             color = "orange")

# PROMEDIO DE ENCUESTAS: Rangos de evaluación ==================================
ggplot(semma) +  geom_bar(aes(x = days_to_elec))+ 
  labs(title = "Análisis de ventanas temporales",
       subtitle =
         "Distribución y conteo de encuestas según los días que quedan para las elecciones",
       caption =
         "Autor: Enric Palau Payeras | Datos: Spanish elections dataset",
       y = "Conteo de encuestas a x días de la elcción")

ggplot(semma) +  geom_bar(aes(x = days_to_elec))+ 
  labs(title = "Análisis de ventanas temporales",
       subtitle =
         "Distribución y conteo de encuestas según los días que quedan para las elecciones",
       caption =
         "Autor: Enric Palau Payeras | Datos: Spanish elections dataset",
       y = "Conteo de encuestas a x días de la elcción")



# PROMEDIO DE ENCUESTAS: WING EFFECT ===========================================
# Color basado en valor
analisis_house_effect <- semma %>% 
  filter(date_elec == ymd("2019-11-10"), ) %>%
  select(poll_firm, wing, wing_effect_e, prom_carrera_casa_wing, prom_casa_wing, prom_carrera_wing)
analisis_house_effect<-analisis_house_effect[!duplicated(analisis_house_effect), ]

ggplot(analisis_house_effect,
       aes(x = reorder(poll_firm, wing_effect_e), y = wing_effect_e)) +
  geom_bar(stat = "identity",
           show.legend = TRUE,
           aes(fill = wing),      # Color de fondo
           color = "white") +  # Color del borde
  xlab("poll_firm") +
  ylab("wing_effect") +
  # scale_y_continuous(breaks= seq(-2, 2, by = 0.1),
  #                     limits = c(min(semma$error_casa_carrera_wing) - 0.2,
  #                                max(semma$error_casa_carrera_wing) + 0.2)) +
  coord_flip() +
  theme(axis.text.y = element_blank(),  # Eliminar textos eje Y
        axis.ticks.y = element_blank(), # Eliminar ticks eje Y
        panel.grid.major.y = element_blank())+# Eliminar grid horizontal
  theme(panel.grid.major = element_line(colour = "gray92"),
        panel.grid.minor = element_line(colour = "gray92"),
        legend.position = "bottom", legend.direction = "horizontal")

# PROMEDIO DE ENCUESTAS: Efecto de los promedios sobre las estimaciones de voto ===========================================
analisis_promedios <- semma [ , 
                   c( "year_elec",  "est_surv_vote" , 
                      "real_vote", "date_elec", "party", "wing", 
                      "poll_firm", "prom_general_partido", "prom_carrera_partido", 
                      "prom_casa_partido", "prom_carrera_casa_partido", "house_effect_e"
                   )]
analisis_promedios <- analisis_promedios %>% 
  filter(date_elec == ymd("2019-11-10"), )
analisis_promedios <- sqldf('
      SELECT *
      FROM analisis_promedios
      WHERE poll_firm = "CIS"
      OR poll_firm = "SOCIOMÉTRICA"
      OR poll_firm = "NC_REPORT"
                      ')
analisis_promedios <- sqldf('
      SELECT *
      FROM analisis_promedios
      WHERE party = "PSOE"
      OR party = "CS"
      OR party = "UP"
      OR party = "VOX"
      OR party = "PP"
                      ')
analisis_promedios2 <- analisis_promedios [ , 
                              c("party", "poll_firm", "prom_carrera_partido", 
                                "prom_carrera_casa_partido"
                              )]
analisis_promedios2<-analisis_promedios2[!duplicated(analisis_promedios2), ]

ggplot() +
  geom_col(data=analisis_promedios,aes(party, prom_carrera_partido, color=party, fill=party), position="dodge")+
  geom_jitter(data=analisis_promedios, 
              aes(x=party, y=est_surv_vote, 
                  shape = poll_firm ), 
  ) +
  geom_jitter(data=analisis_promedios2, 
              aes(x=party, y=prom_carrera_casa_partido, 
                  shape = poll_firm ), color = "#4876FF", size = 5 , 
              ) +
  geom_point(data=analisis_promedios, 
              aes(x=party, y=real_vote), fill = "red", color = "white", shape = 22, size = 5)+
  theme_grey()+
  labs(caption = "Los cuadros rojos representan el resultado real, las barras son el promedio de mercado, las formas azules el promedio por casa y los puntos negros las estimaciones por encuesta.")
  

# SEMMA: variables objetivo ========================================
ggplot(semma) + 
  geom_histogram(aes(x = errores))+ #distribución del error en todo el histórico
  
  ggplot(semma) + 
  geom_histogram(aes(x = real_vote))+ #distribución del error en todo el histórico
  
  ggplot(semma) + 
  geom_histogram(aes(x = est_surv_vote))




# PROMEDIOS PARTIDOS PRINCIPALES ========================================

analisis_psoe <- semma %>% 
  filter(date_elec == ymd("2019-04-28"), ) %>%
  filter(party == "PSOE" ) #%>%
# filter(poll_firm == "CIS" )
analisis_pp <- semma %>% 
  filter(date_elec == ymd("2019-04-28"), ) %>%
  filter(party == "PP" ) #%>%
# filter(poll_firm == "CIS" )
analisis_VOX <- semma %>% 
  filter(date_elec == ymd("2019-04-28"), ) %>%
  filter(party == "VOX" ) #%>%
# filter(poll_firm == "CIS" )
analisis_UP <- semma %>% 
  filter(date_elec == ymd("2019-04-28"), ) %>%
  filter(party == "UP" ) #%>%
# filter(poll_firm == "CIS" )
analisis_CS <- semma %>% 
  filter(date_elec == ymd("2019-04-28"), ) %>%
  filter(party == "CS" ) #%>%
# filter(poll_firm == "CIS" )

psoevspp<-rbind(analisis_psoe, analisis_pp, analisis_UP, analisis_VOX, analisis_CS)

ggplot(psoevspp 
       %>% 
       #   filter(as_date(date_elec) == "2019-04-28") %>% 
         filter(poll_firm == "CIS")
       ,
       aes(x=(as_date(date_elec) - days_to_elec), 
           y= est_surv_vote,
           size = errores,
           color = party), )+
  geom_point()+ 
  # scale_x_date(date_breaks= "4 month")+
  geom_smooth()  + 
  theme(axis.text.x = element_text(size = 6)) +
  labs(x = "Fecha") #+
  # geom_vline(xintercept = as_date("2019-04-20"),
  #            linetype = 2,
  #            color = "orange")


# HISTÓRICOS ==============================================================

analisis_psoe <- semma %>% 
# filter(date_elec == ymd("2019-04-28"), ) %>%
  filter(party == "PSOE" ) %>%
  filter(poll_firm == "CELESTE-TEL" )
analisis_pp <- semma %>% 
# filter(date_elec == ymd("2019-04-28"), ) %>%
  filter(party == "PP" ) %>%
  filter(poll_firm == "CELESTE-TEL" )

psoevspp<-rbind(analisis_psoe, analisis_pp)
psoevspp<- psoevspp 
psoevspp <- sqldf('
      SELECT *
      FROM psoevspp
      WHERE year_elec > 2008 
      AND year_elec < 2023 
                     ')
ggplot(psoevspp 
       # %>% 
       #   filter(as_date(date_elec) == "2019-04-28") %>% 
       #   filter(poll_firm == "CIS")
       ,
       aes(x=(as_date(date_elec) - days_to_elec), 
           y= est_surv_vote,
           size = errores,
           # shape = poll_firm,
           color = party), )+
  geom_point()+ 
  scale_x_date(date_breaks= "4 month")+
  geom_smooth()  + 
  theme(axis.text.x = element_text(size = 6)) +
  labs(x = "Fecha")


# HISTÓRICOS ==============================================================


semmaa<-semma %>% filter(party== "PP") %>% filter(party=="PSOE")
semmaaa <- semma [ , 
                   c( "year_elec",  "real_vote", "date_elec", "party", "wing", "prom_carrera_partido","error_carrera_partido","prom_carrera_wing","error_carrera_wing"
                   )]
semmaaa<-semmaaa[!duplicated(semmaaa), ]
semmaaa
semmaa<-semmaaa %>% filter(party== "PP") %>% filter(party=="PSOE")
semma_anal <- sqldf('
      SELECT *
      FROM semmaaa
      WHERE party = "PSOE" 
      OR party = "PP"')
distrib_barras <-
  ggplot(semma_anal,
         aes(y = real_vote, x = as.factor(year_elec), fill = party)) +
  # position = "fill" para que sean de igual tamaño las barras
  geom_bar(position = "fill", stat = "identity", width = 0.8) +
  # Barras horizontales
  coord_flip() +
  # Escala de colores (orden alfabético de los partidos)
  scale_fill_manual(values = c("#40A7F1", "#F15858"),
                    labels = c("PP", "PSOE")) +
  # Eje y con el año de la elección
  theme(axis.text.y = element_text(size = 11)) +
  # Leyenda, título (colores bandera alemania) y subtítulo
  labs(fill = "party",
       title = paste0("<span style = 'color:#dd1f00'>ELECCIONES</span> ",
                      "<span style = 'color:#ffce01'>GENERALES</span> ",
                      "<span style = 'color:#dd1f00'>DE ESPAÑA</span>"),
       subtitle = paste0()) +
  theme(
    # título con element_markdown para el html con colores
    plot.title = element_markdown(size = 16, face = "bold", color = "black",
                                  family = "titillium", hjust = 0.5),
    # subtítulo
    plot.subtitle = element_text(size = 1, color = "grey20",
                                 family = "titillium", hjust = 0.5))
distrib_barras

distrib_barras <-
  ggplot(semma_anal,
         aes(y = prom_carrera_wing, x = as.factor(year_elec), fill = wing)) +
  # position = "fill" para que sean de igual tamaño las barras
  geom_bar(position = "fill", stat = "identity", width = 0.8) +
  # Barras horizontales
  coord_flip() +
  # Escala de colores (orden alfabético de los partidos)
  scale_fill_manual(values = c("#40A7F1", "#F15858"),
                    labels = c("Left", "Right")) +
  # Eje y con el año de la elección
  theme(axis.text.y = element_text(size = 11)) +
  # Leyenda, título (colores bandera alemania) y subtítulo
  labs(fill = "wing",
       title = paste0("<span style = 'color:#dd1f00'>ELECCIONES</span> ",
                      "<span style = 'color:#ffce01'>GENERALES</span> ",
                      "<span style = 'color:#dd1f00'>DE ESPAÑA</span>"),
       subtitle = paste0()) +
  theme(
    # título con element_markdown para el html con colores
    plot.title = element_markdown(size = 16, face = "bold", color = "black",
                                  family = "titillium", hjust = 0.5),
    # subtítulo
    plot.subtitle = element_text(size = 1, color = "grey20",
                                 family = "titillium", hjust = 0.5))
distrib_barras

ggplot(semma 
       # %>% 
       #   filter(as_date(date_elec) == "2019-04-28") %>% 
       #   filter(poll_firm == "CIS")
       ,
       aes(x=(as_date(date_elec) - days_to_elec), 
           y= prom_carrera_partido,
           # shape = poll_firm,
           color = party), )+
  geom_line()+ 
  geom_dotplot()+
  theme(axis.text.x = element_text(size = 6)) +
  labs(x = "Fecha")


# EXPLORE ==============================================================

split_semma<-rbind(test_semma, train_semma, test_2023)
split_semma_party <- split_semma %>% #reagrupar partits y casas 
  mutate(party =
           case_when(str_detect(party_AP, "1") ~ "AP",
                     str_detect(party_BNG, "1") ~ "BNG",
                     str_detect(party_CC, "1") ~ "CC",
                     str_detect(party_CC.NC, "1") ~ "CC.NC",
                     str_detect(party_CCC, "1") ~ "CCC",
                     str_detect(party_CDS, "1") ~ "CDS",
                     str_detect(party_CIU, "1") ~ "CIU",
                     str_detect(party_CS, "1") ~ "CS",
                     str_detect(party_CUP, "1") ~ "CUP",
                     str_detect(party_EA, "1") ~ "EA",
                     str_detect(party_EE, "1") ~ "EE",
                     str_detect(party_EH.BILDU, "1") ~ "EH.BILDU",
                     str_detect(party_ERC, "1") ~ "ERC",
                     str_detect(party_EV, "1") ~ "EV",
                     str_detect(party_FN, "1") ~ "FN",
                     str_detect(party_HB, "1") ~ "HB",
                     str_detect(party_IU, "1") ~ "IU",
                     str_detect(party_JC, "1") ~ "JC",
                     str_detect(party_MP, "1") ~ "MP",
                     str_detect(party_NS, "1") ~ "NS",
                     str_detect(party_PA, "1") ~ "PA",
                     str_detect(party_PCE, "1") ~ "PCE",
                     str_detect(party_PNV, "1") ~ "PNV",
                     str_detect(party_PODEMOS, "1") ~ "PODEMOS",
                     str_detect(party_PP, "1") ~ "PP",
                     str_detect(party_PRC, "1") ~ "PRC",
                     str_detect(party_PSOE, "1") ~ "PSOE",
                     str_detect(party_UCD, "1") ~ "UCD",
                     str_detect(party_UP, "1") ~ "UP",
                     str_detect(party_UPYD, "1") ~ "UPYD",
                     str_detect(party_VOX, "1") ~ "VOX",
                     TRUE ~ "OTRAS")) %>%
  mutate(poll_firm =
           case_when(str_detect(poll_firm_ASEP, "1") ~ "ASEP",
                     str_detect(poll_firm_CELESTE.TEL, "1") ~ "CELESTE.TEL",
                     str_detect(poll_firm_CIS, "1") ~"CIS",
                     str_detect(poll_firm_DYM, "1") ~"DYM",
                     str_detect(poll_firm_ELECTOPANEL, "1") ~"ELECTOPANEL",
                     str_detect(poll_firm_GAD3, "1") ~"GAD3",
                     str_detect(poll_firm_GALLUP, "1") ~"GALLUP",
                     str_detect(poll_firm_GESOP, "1") ~"GESOP",
                     str_detect(poll_firm_HAMALGAMA_MÉTRICA, "1") ~"HAMALGAMA_MÉTRICA",
                     str_detect(poll_firm_IMOP, "1") ~"IMOP",
                     str_detect(poll_firm_METROSCOPIA, "1") ~"METROSCOPIA",
                     str_detect(poll_firm_MYWORD, "1") ~"MYWORD",
                     str_detect(poll_firm_NC_REPORT, "1") ~"NC_REPORT",
                     str_detect(poll_firm_NOXA, "1") ~"NOXA",
                     str_detect(poll_firm_OBRADOIRO_SOCIO, "1") ~"OBRADOIRO_SOCIO",
                     str_detect(poll_firm_OPINA, "1") ~"OPINA",
                     str_detect(poll_firm_SIGMA_DOS, "1") ~"SIGMA_DOS",
                     str_detect(poll_firm_SIMPLE_LÓGICA, "1") ~"SIMPLE_LÓGICA",
                     str_detect(poll_firm_SOCIOMÉTRICA, "1") ~"SOCIOMÉTRICA",
                     str_detect(poll_firm_TNS_DEMOSCOPIA, "1") ~"TNS_DEMOSCOPIA",
                     str_detect(poll_firm_VOX_PÚBLICA, "1") ~"VOX_PÚBLICA",
                     TRUE ~ "OTRAS")) %>% 
  mutate(gov_pre=
           case_when(str_detect(gov_pre_UCD, "1") ~ "UCD",
                     str_detect(gov_pre_PP, "1") ~ "PP",
                     str_detect(gov_pre_PSOE, "1") ~ "PSOE",
                     TRUE ~ "OTRAS"))

split_semma_party <- split_semma_party [ , c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                             "n", "exit_poll", "est_surv_vote", "prom_general_partido", "error_general_partido", 
                                             "prom_general_wing", "error_general_wing", "prom_casa_partido", 
                                             "error_casa_partido", "prom_casa_wing", "error_casa_wing", "prom_carrera_partido", 
                                             "error_carrera_partido", "prom_carrera_wing", "error_carrera_wing", 
                                             "prom_carrera_casa_partido", "error_carrera_casa_partido", "prom_carrera_casa_wing", 
                                             "error_carrera_casa_wing", "house_effect_e", "house_effect", 
                                             "wing_effect_e", "wing_effect", "urna_0", "urna_7", "urna_15", 
                                             "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                             "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                             "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                             "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                             "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                             "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                             "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                             "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                             "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                             "errores", "gov_pre", "poll_firm", "party")]

semma_anal <- semma [ , c("date_elec", "id_semma", "year_elec", "party", "wing", "poll_firm", 
                           "n_days_field", "days_to_elec", "porc_surveys_firm", "n", "exit_poll", 
                           "lead_party", "lead2_party", "real_vote", "est_surv_vote", "errores", 
                           "prom_general_partido", "error_general_partido", "prom_general_wing", 
                           "error_general_wing", "prom_casa_partido", "error_casa_partido", 
                           "prom_casa_wing", "error_casa_wing", "prom_carrera_partido", 
                           "error_carrera_partido", "prom_carrera_wing", "error_carrera_wing", 
                           "prom_carrera_casa_partido", "error_carrera_casa_partido", "prom_carrera_casa_wing", 
                           "error_carrera_casa_wing", "house_effect_e", "house_effect", 
                           "wing_effect_e", "wing_effect", "urna_0", "urna_7", "urna_15", 
                           "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                           "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                           "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                           "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                           "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                           "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                           "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                           "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                           "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                           "gov_pre")]


semma_id <- sqldf('
      SELECT a. *
           , b. date_elec
      FROM  split_semma_party  AS a
      LEFT JOIN (
                SELECT *
                FROM semma_anal ) AS b
                ON    (a.year_elec = b.year_elec) 
                AND   (a.poll_firm = b.poll_firm )
                AND   (a.porc_surveys_firm = b.porc_surveys_firm ) 
                AND   (a.n_days_field = b.n_days_field )
                AND   (a.days_to_elec = b.days_to_elec )
                AND   (a.n = b.n )
                AND   (a.est_surv_vote = b.est_surv_vote )
                AND   (a.prom_general_partido = b.prom_general_partido )
                AND   (a.prom_general_wing = b.prom_general_wing )
                AND   (a.party = b.party )
                AND   (a.poll_firm = b.poll_firm )
                AND   (a.errores = b.errores )
                    ')
semma_id <- sqldf('
      SELECT b. *
           , a. date_elec
      FROM  split_semma_party  AS b
      LEFT JOIN (
                SELECT *
                FROM semma_anal ) AS a
                ON    (a.year_elec = b.year_elec) 
                AND   (a.poll_firm = b.poll_firm )
                AND   (a.porc_surveys_firm = b.porc_surveys_firm ) 
                AND   (a.n_days_field = b.n_days_field )
                AND   (a.days_to_elec = b.days_to_elec )
                AND   (a.n = b.n )
                AND   (a.est_surv_vote = b.est_surv_vote )
                AND   (a.prom_general_partido = b.prom_general_partido )
                AND   (a.prom_general_wing = b.prom_general_wing )
                AND   (a.party = b.party )
                AND   (a.poll_firm = b.poll_firm )
                AND   (a.errores = b.errores )
                    ')



library(tidyverse)
library(ggbump)

semma_evo <- train_semma %>% #reagrupar partits y casas 
  mutate(party =
           case_when(str_detect(party_UP, "1") ~ "UP",
                     str_detect(party_CS, "1") ~ "CS",
                     str_detect(party_PP, "1") ~ "PP",
                     str_detect(party_PSOE, "1") ~ "PSOE",
                     str_detect(party_VOX, "1") ~ "VOX",
                     TRUE ~ "OTRAS")) 
 
 
semma_evo <- sqldf('
      SELECT *
      FROM semma
      WHERE party = "PSOE"
      OR party = "CS"
      OR party = "UP"
      OR party = "VOX"
      OR party = "PP"
                      ')

semma_evo <- semma_evo [ , c("date_elec", "year_elec", "days_to_elec", "party", 
                              "real_vote", "prom_carrera_partido",
                              "gov_pre", "gov_exp_edu", "gov_exp_san", "gov_exp_war", "gov_cor_rate"
                                    )]


ggplot(semma_evo %>% 
         filter(as_date(date_elec) >= "2016-06-26"),
       aes(x=(as_date(date_elec) - days_to_elec), 
           y= est_surv_vote,
           size = n,
           color = abs(errores)), )+

  ggplot(semma_last, aes(x = , y = y, color = grupo)) +
  geom_bump(size = 1.5) +
  geom_point(size = 6) +
  scale_color_brewer(palette = "RdBu")


# MAE's DE LAS TOP ==================================================================
analisis_promedios <- semma [ , 
                              c( "year_elec",  "est_surv_vote" , 
                                 "real_vote", "date_elec", "party", "wing", 
                                 "poll_firm", "prom_general_partido", "prom_carrera_partido", 
                                 "prom_casa_partido", "prom_carrera_casa_partido", "house_effect_e", "days_to_elec"
                              )]

analisis_promedios <- analisis_promedios %>% 
  filter(date_elec == ymd("2019-11-10"), )

analisis_promedios <- sqldf('
      SELECT *
      FROM analisis_promedios
      WHERE poll_firm = "CIS"
      OR poll_firm = "NC_REPORT"
      OR poll_firm = "SOCIOMÉTRICA"
                      ')
analisis_promedios <- sqldf('
      SELECT *
      FROM analisis_promedios
      WHERE party = "PSOE"
      OR party = "CS"
      OR party = "UP"
      OR party = "VOX"
      OR party = "PP"
                      ')
analisis_promedios2 <- analisis_promedios [ , 
                                            c("party", "poll_firm", 
                                              "prom_carrera_partido", 
                                              "prom_carrera_casa_partido"
                                            )]

analisis_promedios2<-analisis_promedios2[!duplicated(analisis_promedios2), ]

ggplot() +
  geom_col(data=analisis_promedios,aes(party, real_vote, fill=party), position="dodge")+  
  scale_fill_manual(values = c("#CD8500", "#009ACD", "#CD3700", "#B452CD", "#00CD00" ))+
  geom_jitter(data=analisis_promedios, 
              aes(x=party, y=est_surv_vote, 
                  shape = poll_firm, size = days_to_elec), 
  ) +
  geom_jitter(data=analisis_promedios2, 
              aes(x=party, y=prom_carrera_casa_partido, 
                  shape = poll_firm ), color = "#48D1CC", size = 5 , 
  ) +
  geom_point(data=analisis_promedios, 
             aes(x=party, y=prom_carrera_partido), fill = "lightcoral", color = "black", shape = 22, size = 5)+
  theme_grey()+
  labs(caption = "Los cuadros rojos representan el resultado real, las barras son el promedio de mercado, las formas azules el promedio por casa y los puntos negros las estimaciones por encuesta.")+
  labs(caption = "En negro, las encuestas, azul los promedios de casa por partido, rojo el promedio de mercado y el voto real equivale a las columnas") + theme(plot.caption = element_text(size = 15))
# MAE's DE LAS TOP vs US ==================================================================
analisis_promedios <- semma [ , 
                              c( "year_elec",  "est_surv_vote" , 
                                 "real_vote", "date_elec", "party", "wing", 
                                 "poll_firm", "prom_general_partido", "prom_carrera_partido", 
                                 "prom_casa_partido", "prom_carrera_casa_partido", "house_effect_e"
                              )]

analisis_promedios <- analisis_promedios %>% 
  filter(date_elec == ymd("2019-11-10"), )
analisis_promedios <- sqldf('
      SELECT *
      FROM analisis_promedios
      WHERE poll_firm = "SIGMA_DOS"
      OR poll_firm = "GAD3"
      OR poll_firm = "SOCIOMÉTRICA"
                      ')
analisis_promedios <- sqldf('
      SELECT *
      FROM analisis_promedios
      WHERE party = "PSOE"
      OR party = "CS"
      OR party = "UP"
      OR party = "VOX"
      OR party = "PP"
                      ')
analisis_promedios2 <- analisis_promedios [ , 
                                            c("party", "poll_firm", 
                                              "prom_carrera_partido", 
                                              "prom_carrera_casa_partido"
                                            )]

analisis_promedios2<-analisis_promedios2[!duplicated(analisis_promedios2), ]
# eval2_test_SVM_party_2020_04 <-sqldf('
#       SELECT *
#       FROM eval_test_SVM_party_2019_04
#       WHERE party = "PSOE"
#       OR party = "CS"
#       OR party = "UP"
#       OR party = "VOX"
#       OR party = "PP"
#                   ')
eval2_test_SVM_party_2019_11 <- sqldf('
      SELECT *
      FROM eval_test_SVM_party_2019_11
      WHERE party = "PSOE"
      OR party = "CS"
      OR party = "UP"
      OR party = "VOX"
      OR party = "PP"
                  ')


ggplot() +
  geom_col(data=analisis_promedios,aes(party, real_vote, fill=party), position="dodge") + #voto del partido 
  scale_fill_manual(values = c("#CD8500", "#009ACD", "#CD3700", "#B452CD", "#00CD00" ))+ #color del partido 
  geom_jitter(data=analisis_promedios, 
              aes(x=party, y=est_surv_vote, 
                  shape = poll_firm,), #encuestas individuales (puntos negros)
  ) +
  geom_jitter(data=analisis_promedios2, 
              aes(x=party, y=prom_carrera_casa_partido, 
                  shape = poll_firm ), color = "#48D1CC", size = 5 , #promedio de la casa (punto azul)
  ) +
  geom_point(data=analisis_promedios, 
             aes(x=party, y=prom_carrera_partido), fill = "lightcoral", color = "black", shape = 22, size = 5)+ #promedio de mercado del partido (punto rojo)
  geom_point(data=eval2_test_SVM_party_2019_11, 
              aes(x=party, y=prediccion_de_partido), fill = "#3CB371", color = "black", shape = 22, size = 5, alpha = 0.9)+ #predicción de nuestro modelo(punto verde)
  theme_grey()+
  labs(caption = "En verde, nuestra predicción, el promedio por casa en azul, en negro las encuestas y el voto real es la barra.") + theme(plot.caption = element_text(size = 15))
 
# SERIE TEMPORAL DEL VOTO ==================================================================
# install.packages("CGPfunctions") 
# eval_test_arbol_party<-readRDS("eval_test_arbol_party")
library(CGPfunctions)
newggslopegraph(eval_test_arbol_party, date_elec, real_vote, party,
                Title = "Evolución del voto entre partidos",
                SubTitle = "1982-2019",
                Caption =  "Autor: Enric Palau Payeras | Datos: Spanish elections dataset",
                DataLabelPadding = 0.2,
                DataLabelLineSize = 0.5,
                DataLabelFillColor = "white") +
  theme_gray() +
  scale_color_manual(values = c("firebrick3", "#6CA6CD", "pink", "#FF7F00", 
                                "palegreen3", "orange3",  "orange", "red",
                                "grey", "yellow",  "gold3", "lightblue3", 
                                "darkblue", "darkred",  "darkgreen", "yellow", 
                                "darkolivegreen3", "orchid3",  "steelblue", "red",
                                "darkmagenta", "#FF3E96",  "green2", "limegreen"
                                ))+
  theme(legend.position = "none")



# OTROS GRÁFICOS ==================================================================
print(semma%>% skim())

ggplot(semma) + 
  geom_histogram(aes(x = est_surv_vote))+
  facet_grid(year_elec~.)

ggplot(prom_casa_partido) + 
  geom_histogram(aes(x = prom_casa_partido))

ggplot(semma) + 
  geom_histogram(aes(x = real_vote))+
  facet_grid(year_elec~.)

ggplot(semma) + 
  geom_histogram(aes(x = error))+
  facet_grid(year_elec~.)


partidos <- split(semma, semma$party)
df_psoe<-partidos[["PSOE"]]
df_pp<-partidos[["PP"]]
ggplot(df_exp, aes(x=real_vote, y=est_surv_vote, color=error))+ geom_point(alpha=0.3)+
  facet_grid(year_elec~ 2019, )+
  theme_minimal()


df_psoe_pol <- split(df_psoe, df_psoe$poll_firm)


encuestadoras <- split(semma, semma$poll_firm)

ggplot(semma, aes(x=real_vote, y=est_surv_vote, size=abs(error), color = wing))+ geom_point(alpha=0.3)+
  # facet_grid(year_elec~.)+
  theme_minimal()

ggplot(df_1982, aes(x=real_vote, y=est_surv_vote, color=error))+ geom_point(alpha=0.3)+
  theme_minimal()
ggplot(df_1986, aes(x=real_vote, y=est_surv_vote, color=error, shape=poll_firm))+ geom_point(alpha=0.3)+
  theme_minimal()
ggplot(df_1989, aes(x=real_vote, y=est_surv_vote, color=error))+ geom_point(alpha=0.3)+
  theme_minimal()
ggplot(df_1993, aes(x=real_vote, y=est_surv_vote, color=error))+ geom_point(alpha=0.3)+
  theme_minimal()
ggplot(df_1996, aes(x=real_vote, y=est_surv_vote, color=error))+ geom_point(alpha=0.3)+
  theme_minimal()
ggplot(df_2000, aes(x=real_vote, y=est_surv_vote, color=error))+ geom_point(alpha=0.3)+
  theme_minimal()
ggplot(df_2004, aes(x=real_vote, y=est_surv_vote, color=error))+ geom_point(alpha=0.3)+
  theme_minimal()
ggplot(df_2008, aes(x=real_vote, y=est_surv_vote, color=error))+ geom_point(alpha=0.3)+
  theme_minimal()
ggplot(df_2011, aes(x=real_vote, y=est_surv_vote, color=error))+ geom_point(alpha=0.3)+
  theme_minimal()
ggplot(df_2015, aes(x=real_vote, y=est_surv_vote, color=error))+ geom_point(alpha=0.3)+
  theme_minimal()
ggplot(df_2016, aes(x=real_vote, y=est_surv_vote, color=error))+ geom_point(alpha=0.3)+
  theme_minimal()
ggplot(df_2019_1, aes(x=real_vote, y=est_surv_vote, color=error))+ geom_point(alpha=0.3)+
  theme_minimal()
ggplot(df_2019_2, aes(x=real_vote, y=est_surv_vote, color=error))+ geom_point(alpha=0.3)+
  theme_minimal()


print(semma %>% glimpse())


print(semma %>% skim())

 
# Vemos cuantos casos con x% de observaciones hay.
ggplot(semma) + 
  geom_histogram(aes(x = est_surv_vote))

ggplot(semma) + 
  geom_histogram(aes(x = real_vote))

ggplot(semma) + 
  geom_histogram(aes(x = errores))

ggplot(semma, aes(x=real_vote, y=est_surv_vote, color=poll_firm))+ geom_point(alpha=0.3)+
  facet_grid(year_elec~.)+
  theme_minimal()


ggplot(df_2019_2, aes(x=real_vote, y=est_surv_vote, color=poll_firm, size = abs(error)))+ geom_point(alpha=0.3)+
  facet_grid(wing~.)+
  theme_minimal()

ggplot(df_2019_2, aes(x=real_vote, y=est_surv_vote, color=poll_firm, size = abs(error)))+ geom_point(alpha=0.3)+
  facet_grid(party~.)+
  theme_minimal()



# Vemos cuantos casos con x% de observaciones hay.
ggplot(semma) + 
  geom_histogram(aes(x = est_surv_vote))

ggplot(semma = analisis_psoe,
       mapping = aes(x = n, y = error )) +
  geom_point(color = "#006EA1", alpha = 0.5, size = 4) +
  # Diagonal
  geom_smooth(method=lm, se=FALSE, color = "orange", size = 1.5) +
  labs(title = "Análisis Bivariante",
       subtitle =
         "lot vs Price",
       x = "lot",
       y = "price")



# Categóricas
wing1 <-ggplot(semma) +  geom_bar(aes(x = wing))+ 
  labs(title = "Análisis Univariante",
       subtitle =
         "wing count",
       caption =
         "Autor: Enric Palau Payeras | Datos: Spanish elections dataset",
       y = "Frecuency")
wing2 <-ggplot(data = semma,
               mapping = aes(x = wing, y= est_surv_vote)) +
  geom_point(color = "#006EA1", alpha = 0.5, size = 4) +
  # Diagonal
  geom_smooth(method=lm, se=FALSE, color = "orange", size = 1.5) +
  labs(title = "Análisis Bivariante",
       subtitle =
         "wing vs est_surv_vote",
       x = "wing",
       y = "est_surv_vote")

wing3 <-ggplot(data = semma) + 
  geom_density(aes(x=est_surv_vote, fill= factor(wing)),bins=10, position = "identity",alpha = 0.5) + 
  labs(title = "Análisis Bivariante",
       subtitle = "wing vs est_surv_vote",
       x = "est_surv_vote",
       y = "Frecuency")
print(wing1)
print(wing2)
print(wing3)

# Intervalo  
real_vote1 <-ggplot(semma) + 
  geom_histogram(aes(x = real_vote))+ 
  labs(title = "Análisis Univariante",
       subtitle =
         "real_vote evolution",
       caption =
         "Autor: Enric Palau Payeras | Datos: Spanish elections dataset",
       x= "real_vote",
       y = "Frecuency")

real_vote2 <-ggplot(data = semma,
                    mapping = aes(x = real_vote, y = est_surv_vote )) +
  geom_point(color = "#006EA1", alpha = 0.5, size = 4) +
  # Diagonal
  geom_smooth(method=lm, se=FALSE, color = "orange", size = 1.5) +
  labs(title = "Análisis Bivariante",
       subtitle =
         "lotSize vs Price",
       x = "real_vote",
       y = "est_surv_vote")

print(real_vote1)
print(real_vote2)

# Estudio de correlaciones  
datos <- table(semma$party)
pie(datos)

ggplot(semma, aes(x=error)) + 
  geom_histogram(bins=10, color="black", aes(fill=wing)) +
  facet_grid(year_elec~.)




# Más de otros gráficos posibles
ggplot(semma %>% 
         filter(as_date(date_elec) == "2019-11-10") %>% 
         # filter(poll_firm == "CIS") %>%
         filter(party == "PSOE"),
       aes(x=(as_date(date_elec) - days_to_elec), 
           y= est_surv_vote,
           size = n,
           color = abs(errores)), )+
  # abs(errores)
  
  geom_point()+ 
  # scale_x_date(date_breaks= "4 month")+
  geom_smooth()  + 
  theme(axis.text.x = element_text(size = 6)) +
  labs(x = "Fecha") +
  geom_hline(yintercept = 28,
             linetype = 2,
             color = 2) +
  geom_vline(xintercept = as_date("2019-11-04"),
             linetype = 2,
             color = "orange")

ggplot(semma %>% 
         filter(as_date(date_elec) == "2019-11-10") %>% 
         filter(party == "PSOE"),
       aes(x=(as_date(date_elec) - days_to_elec), 
           y= est_surv_vote,
           size = n,
           color = poll_firm), )+
  geom_point()+ 
  # scale_x_date(date_breaks= "4 month")+
  geom_smooth()  + 
  theme(axis.text.x = element_text(size = 6)) +
  labs(x = "Fecha") +
  geom_hline(yintercept = 28,
             linetype = 2,
             color = 2) +
  geom_vline(xintercept = as_date("2019-11-04"),
             linetype = 2,
             color = "orange")
ggplot(semma %>% 
         filter(as_date(date_elec) == "2019-11-10") %>% 
         filter(party == "PP"),
       aes(x=(as_date(date_elec) - days_to_elec), 
           y= est_surv_vote,
           size = n,
           color = poll_firm), )+
  geom_point()+ 
  # scale_x_date(date_breaks= "4 month")+
  geom_smooth()  + 
  theme(axis.text.x = element_text(size = 6)) +
  labs(x = "Fecha") +
  geom_hline(yintercept = 20.8,
             linetype = 2,
             color = 2) +
  geom_vline(xintercept = as_date("2019-11-04"),
             linetype = 2,
             color = "orange")


ggplot(semma %>% 
         # filter(as_date(date_elec) == "2019-04-28") %>% 
         filter(poll_firm == "CIS") %>%
         filter(party == "PSOE"),
       aes(x=(as_date(date_elec) - days_to_elec), 
           y= est_surv_vote,
           size = n,
           color = abs(errores)), )+
  geom_point()+ 
  scale_x_date(date_breaks= "4 month")+
  geom_smooth()  + 
  theme(axis.text.x = element_text(size = 6)) +
  labs(x = "Fecha") +
  geom_vline(xintercept = as_date("2004-03-14"),
             linetype = 2,
             color = 2)


ggplot(semma %>% filter(party=="PSOE",
                        as_date(date_elec) == "2019-04-28"
                        # year_elec == 2019
),
aes(x= as_date(date_elec) - days_to_elec), 
y= est_surv_vote,
size = n,
color = abs(errores))+
  geom_point()+ 
  scale_x_date(date_breaks= "4 month")+
  geom_smooth()  + 
  theme(axis.text.x = element_text(size = 6)) +
  labs(x = "Fecha") +
  geom_hline(yintercept = 22.7,
             linetype = 2,
             color = 2)

tendencias_2019<-ggplot(semma%>% filter(#party=="PSOE", 
  year_elec == "2016"
), 
aes(x=(as_date(date_elec) - days_to_elec),
    y = errores, 
    color = party)) +
  geom_line(linetype = 3,
            lwd = 1.1)
tendencias_2019

