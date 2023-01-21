
# ----- INTRO -----
cat("Cargando paquetes...\n")
# Borramos todo
rm(list = ls())

# Cargamos paquetes
library(tidyverse) # Manejo de datos + ggplot2
library(readr) # Importar ficheros
library(rvest) # Scrapping
library(purrr) # Operaciones con listas
library(glue)  # Pegar cadenas de texto literal
library(datapasta) # Copypaste tablas
library(lubridate) # Manejo de fechas
library(readxl) # Read xls/xlsx
library(tidyBdE) # datos económicos Banco de España
# install.packages("devtools")
 # devtools::install_github("ropenspain/infoelectoral")
library(infoelectoral)
library(skimr) # resumen numérico
library(corrplot) # visualizar correlaciones
library(ggthemes) # temas para tunear gráficas
library(tidymodels) # depuración datos
library(car) # herramientas regresión
library(glue) # pegar texto + variables fácilmente
library(dplyr) # manipùlar datos
library(sas7bdat) # Incorporar archivos SAS
library(caret) # paquete de modelos
library(sqldf) # paquete para sentencias SQL
library(Metrics) # extraer métricas
library(ggThemeAssist) # customizar gráficos


# ----- SCRIPTS -----
# Estos son los básicos para obtener la base dedatos:

# variables de entorno
cat("Cargando entorno de variables...\n")
source("./variables.R") 

# funciones auxiliares
cat("Cargando funciones auxiliares...\n")
source("./fun_aux.R")

# descarga HISTÓRICO encuestas preprocesadas
# (NO EJECUTAR, solo si se ha cambiado algo)
cat("Construyendo bd historica...\n")
# source("./construccion_bd_historica.R") 

# Encuestas nuevas (EJECUTAR para lo nuevo)
cat("Construyendo bd de encuestas recientes...\n")
source("./current_surveys.R")

# descarga ELECCIONES NACIONALES
cat("Cargando datos de elecciones nacionales...\n")
source("./national_data_elections.R")

# datos BDE (EJECUTAR para lo nuevo, no tarda mucho)
cat("Construyendo bd del BCE...\n")
source("./extract_data_bde.R")

# ----- DATOS ENCUESTADORAS: CARGA FICHEROS -----

cat("Cargando ficheros .csv...\n")

# Historico encuestas preprocesadas: Tenemos con n y sin n

national_surveys <-
  read_csv(file = "./EXPORTADO/national_surveys.csv")

national_surveys_with_n <-
   read_csv(file = "./EXPORTADO/national_surveys_with_n.csv")

# Encuestas nuevas: Próximas elecciones (2023) Tenemos con n y sin n
current_surveys <-
   read_csv(file = "./EXPORTADO/current_surveys.csv")

current_surveys_with_n <-
    read_csv(file = "./EXPORTADO/current_surveys_with_n.csv")

current_surveys_with_n <-
  current_surveys_with_n %>%  
  mutate(year_elec = 2023) %>%
  mutate(date_elec =  as.Date("2023-11-10"))
 
# juntamos nuevas + histórico
national_surveys <-
   bind_rows(national_surveys, current_surveys)
national_surveys_with_n  <-
   bind_rows(national_surveys_with_n, current_surveys_with_n)

# ----- DATOS ENCUESTADORAS: PREPROCESAMOS ENCUESTAS -----
cat("Pivotando y preprocesando encuestas...\n")

# Calculamos el número de encuestas de cada casa
# encuestadora (por si quisiéramos filtrar por frecuencia)
national_surveys <-
  national_surveys_with_n %>%
  # national_surveys %>%
  group_by(poll_firm) %>% 
  mutate(n_surveys_firm = n()) %>% 
  ungroup() %>% 
  mutate(porc_surveys_firm = 100 * n_surveys_firm / n()) %>% 
  relocate(n_surveys_firm, porc_surveys_firm,
           .after = poll_firm)

# Creamos un id encuesta para eliminar duplicados
national_surveys <-
  national_surveys %>% 
  mutate(id_survey =
           glue("{poll_firm}-{field_date_ini}-{field_date_end}")) %>% 
  relocate(id_survey, .before = poll_firm) %>% 
  distinct(id_survey, .keep_all = TRUE)
  
# Pivotamos para pasar los partidos a columnas
national_surveys_longer_with_NA <-
  national_surveys %>% 
  pivot_longer(cols = -(id_survey:lead),
               names_to = "party",
               values_to = "est_surv_vote")

# Obtenemos partido ganador de encuesta
lead_party <- 
  national_surveys_longer_with_NA %>% 
  group_by(id_survey) %>% 
  arrange(desc(est_surv_vote)) %>% 
  slice(1) %>%
  ungroup() %>%
  select(id_survey, party) %>% 
  rename(lead_party = party)

# Obtenemos segundo partido ganador de encuesta
lead2_party <- 
  national_surveys_longer_with_NA %>% 
  group_by(id_survey) %>% 
  arrange(desc(est_surv_vote)) %>% 
  slice(2) %>%
  ungroup() %>% 
  select(id_survey, party) %>% 
  rename(lead2_party = party)

# Cruzamos el partido ganador y segundo
national_surveys_longer_with_NA <-
  national_surveys_longer_with_NA %>% 
  left_join(lead_party, id = id_survey) %>% 
  left_join(lead2_party, id = id_survey) %>% 
  relocate(lead_party, lead2_party, .after = lead)

# Exportamos
# write_csv(national_surveys_longer_with_NA,
#          file = "./EXPORTADO/national_surveys_longer_with_NA.csv")

# Eliminamos partidos que no aparecen en encuestas
national_surveys_longer <-
  national_surveys_longer_with_NA %>% 
  drop_na(est_surv_vote)

# Exportamos
# write_csv(national_surveys_longer,
#           file = "./EXPORTADO/national_surveys_longer.csv")

# ----- DATOS ELECTORALES: CARGA FICHEROS -----
# descarga ELECCIONES NACIONALES
cat("Cargando datos de elecciones nacionales...\n")
source("./national_data_elections.R")

# Votos por partido-elección-municipio
votes_national_by_mun <-
  read_csv(file = "./EXPORTADO/votes_national_by_mun.csv")  
# Votos por elección-partido
votes_national <-
  read_csv(file = "./EXPORTADO/votes_national.csv")  
# Censo por elección
census_national_by_year <-
  read_csv(file  = "./EXPORTADO/census_national_by_year.csv") 

# ----- UNIMOS DATOS ELECTORALES Y ENCUESTAS -----
# Datos estimados (1982-2019) 
base_error <- national_surveys_longer #renombre convencionalde la tabla

#-A) Censo por elección -#

# 1. Renombrar las variables que ya existen en la base de datos de las encuestas.
census_national_by_year <- census_national_by_year %>% rename(votes_census = "votes" ,
                                                              turnout_census = "turnout")
# 2. Crear varible date_elec para join.
# variables de entorno. Cargamos de nuevo en caso de que haberlas tocado.
source("./variables.R") 

# 2.1 Concatenamos Year y Month en date_elections => Variable nueva date_ym
date_elections <- date_elections %>% 
  drop_na  
date_elections <- date_elections %>%  
  rename(anno = year, mes = month) %>% 
  # Si mes = 9 --> necesitamos "09" (van en texto)
  mutate(anno = as.character(anno),
         mes = if_else(mes < 10, paste0("0", as.character(mes)),
                       as.character(mes)),
         tipo_eleccion = "congreso") 
date_elections <-
  date_elections %>% 
  mutate(date_ym =
           glue("{anno}_{mes}")) %>% 
  relocate(date_ym, .before = date_elec)

date_elections <- date_elections %>% 
  select (- (anno : tipo_eleccion))

date_elections <- date_elections %>% 
  distinct(date_ym, date_elec)        

# 2.2 Concatenamos Year y Month en census_national_by_year => Variable nueva date_ym
census_national_by_year <-
  census_national_by_year %>% 
  mutate(date_ym =
           glue("{year}_{month}")) %>% 
  relocate(date_ym, .before = year) 

# 2.3 Join de census_national_by_year y date_elections by date_ym
census_national_by_year <- 
  left_join(census_national_by_year,
            date_elections, 
            by = "date_ym") 

# 3 Dejar columnas de interés
census_national_by_year <- census_national_by_year %>% select (- (date_ym: month)) 

#-B) Voto por elección -#

# 1. Renombrar las variables que ya existen en la base de datos de las encuestas.
votes_national <- votes_national %>% rename(party2 = "party_abbrev")

# 2. Crear varible date_elec para join.
# 2.1 Concatenamos Year y Month en date_elections => Variable nueva date_ym
# 2.2 Concatenamos Year y Month en census_national_by_year => Variable nueva date_ym
votes_national <-
  votes_national %>% 
  mutate(date_ym =
           glue("{year}_{month}")) %>% 
  relocate(date_ym, .before = year) 

# 2.3 Join de census_national_by_year y date_elections by date_ym
votes_national <- 
  left_join(votes_national,
            date_elections, 
            by = "date_ym") 

# 3 Dejar columnas de interés
votes_national <- votes_national %>% select (- (date_ym: month)) 

#-C) UNIÓN DE DATOS: Join censo y voto real -#
datos_reales <- 
  left_join(votes_national,
            census_national_by_year, 
            by = "date_elec") 
datos_reales <- datos_reales %>% mutate(real_vote = (national_votes/votes_census) *  100)
print(datos_reales%>% skim())

# Exportamos datos de encuestas (base_error) y datos electorales (datos_reales)
# write_csv(datos_reales,
#           file = "./EXPORTADO/datos_reales.csv")
# write_csv(base_error,
#           file = "./EXPORTADO/base_error.csv")


# p_real <- read_csv("EXPORTADO/datos_reales.csv")
p_real <- datos_reales #
p_real <- p_real  %>% rename (party = "party2")
p_real <- p_real %>% mutate (p_date = glue("{party}_{date_elec}")) %>% 
  relocate(p_date, .before = party) 

# p_error <- read_csv("EXPORTADO/base_error.csv")
p_error <- base_error
p_error <- p_error %>% mutate (p_date = glue("{party}_{date_elec}")) %>% 
  relocate(p_date, .before = id_survey)

# Unimos datos de encuestas y elecciones por identificador de partido y carrera
base_promedios <- 
  left_join(p_error,
            p_real, 
            by = "p_date") 

base_promedios$party.y <- NULL
base_promedios$date_elec.y <- NULL
base_promedios <- base_promedios %>% rename(date_elec = "date_elec.x" ,
                                            party = "party.x")

base_promedios <- base_promedios[!is.na(base_promedios$real_vote), ]

base_promedios$real_vote <- round(base_promedios$real_vote ,digit=1) # Round off the column for 1 decimal

# write_csv(base_promedios,
#           file = "./EXPORTADO/base_promedios.csv")

# ----- REVISION DATOS ELECTORALES Y ENCUESTAS (creamos la variable de ala. ESTO ABRE LA POSIBILIDAD DE TRATAR EL PROBLEMA COMO CLASIFICACIÓN) -----
# Revisamos el resumen de la base de datos
print(base_promedios%>% skim())

# Revisamos las ocurrencias de los partidos
table(base_promedios$party)

# Revisamos las ocurrencias de las casas
table(base_promedios$poll_firm)

# Reordenamos las variables para trabajar con comodidad
dput(names(base_promedios))
base_promedios <- base_promedios [ , 
                                   c( # 1_Variables de fecha
                                     "id_survey", # Variable id única nos sirve para ordenar el dataframe
                                     # 1_1_ Fechas de elección y partido
                                     "p_date", "year_elec",  # partido por elección; año de elección (por si agrupamos variables economicas luego)
                                     # 1_2_ fechas relativas a las encuestas
                                     "field_date_ini", "field_date_end", # Inicio y fin del trabajo de campo
                                     "n_days_field", "days_to_elec", "exit_poll", # días de trabajo de campo; días que quedan para elección; pie de urna
                                     # 2_Variables de participación
                                     # 2_1_ Encuestas y participantes
                                     "n_surveys_firm", "porc_surveys_firm", #número de encuestas de la casa y su % sobre el dataset
                                     "n", "turnout", #muestra y participación de la encuesta
                                     # 2_2_ Datos del censo
                                     "turnout_census", #participación en la elección
                                     "census", #población
                                     "null_votes", "empty_votes","total_votes", #voto nulo; voto en blanco; votos totales 
                                     # 3_Variables de resultados
                                     # 3_1_ Datos reales 
                                     "national_votes", #votos por partido y elección
                                     "votes_census", #votos por elección
                                     # 3_2_ Datos de victoria  estimada
                                     "lead", "lead_party", "lead2_party", #diferencia del ganador; partido ganador; partido segundo; (ojo que no cuadran pq CS lo hemos perdio y aquí sigue)
                                     # 3_3_ Separador (no se usan en este caso) ütiles si dividimos encuestas de municipio y nacionales
                                     "type_elec", "type_surv",
                                     # 3_4_ Datos básicos 
                                     "date_elec", #elección
                                     "poll_firm", #casa encuestadora
                                     "party", #partido
                                     "real_vote", #%voto real = (real_vote = (national_votes/votes_census) *  100)
                                     "est_surv_vote" #variable objetivo = %voto estimado por casa, partido y elección
                                   )]

# Calculamos los días para la elección de 2023. Recordemos que aún no hay fecha. Partimos del supuesto de que serán como las anterios a 10 de Noviembre
base_2023 <- base_error %>% filter(year_elec == 2023) %>%  mutate (days_to_elec = as.numeric(date_elec - field_date_end))
base_promedios  <-
  bind_rows(base_promedios, base_2023)

# Creamos la variable: Afiliación (Izquierda, Centro, Derecha)
base_promedios$wing <-(base_promedios$wing = base_promedios$party)
# Recodificamos partidos según alineación
# Revisamos en wikipedia las alineaciones de los 24 partidos restantes en nuestro dataframe
base_promedios_wing <-
  base_promedios %>% 
  mutate(wing =
           case_when(str_detect(wing, "VOX")|                                                                            
                       str_detect(wing, "UCD")|
                       str_detect(wing, "FN")|
                       str_detect(wing, "CIU")|
                       str_detect(wing, "CDS")|
                       str_detect(wing, "CC")|
                       str_detect(wing, "AP")|
                       str_detect(wing, "cs")|
                       str_detect(wing, "PP") ~ "RIGHT",
                     TRUE ~ "LEFT"))

print(base_promedios_wing%>% skim())
# Exportamos
# write_csv(base_promedios_wing,
#            file = "./EXPORTADO/base_promedios_wing.csv")
# Importamos
# base_promedios_wing <- read_csv("EXPORTADO/base_promedios_wing.csv")

#  Calculamos los días restantes para la elección e inciamos el proceso de promedio de encuestas
base_promedios_wing <-  base_promedios_wing %>%  mutate (days_to_elec = as.numeric(date_elec - field_date_end))

# ----- PROMEDIAR ENCUESTAS: Concepto básico; promedio simple sin rangos -----
# 0.0. Con el id_survey ya tenemos el promedio (est_surv_vote) de cada encuesta (puntos del gráfico de KIKO)
# y podemos extraer el error por encuesta
base_promedios_wing <-
  base_promedios_wing %>%  
  mutate(error = real_vote - est_surv_vote) %>% 
  relocate( error, .after = est_surv_vote) #sacamos el error por encuesta


hist_error <- select(base_promedios_wing, 
                     date_elec, year_elec, 
                     party, wing, 
                     poll_firm, n_days_field, days_to_elec, id_survey, porc_surveys_firm, n, exit_poll, lead_party, lead2_party, 
                     real_vote, est_surv_vote, error)  #sacamos el error por encuesta

# 0.1. Promedio por partido 
prom_general_partido <- group_by(base_promedios_wing, party)
prom_general_partido <- summarise(prom_general_partido, 
                                  prom_general_partido = mean(est_surv_vote, na.rm = TRUE)
                                  )
prom_general_partido$prom_general_partido <- round(prom_general_partido$prom_general_partido ,digit=1)
hist_error_prom <- sqldf('
      SELECT a.* 
           , b.prom_general_partido AS prom_general_partido
           , (b.prom_general_partido - a.real_vote) AS error_general_partido
      FROM hist_error  AS a
      LEFT JOIN (
                SELECT *
                FROM prom_general_partido ) AS b
                ON   (a.party = b.party ) ')


# 0.2. Promedio por ala
prom_general_wing <- group_by(base_promedios_wing, wing)
prom_general_wing <- summarise(prom_general_wing, prom_general_wing = mean(est_surv_vote, na.rm = TRUE))
prom_general_wing$prom_general_wing <- round(prom_general_wing$prom_general_wing ,digit=1)
hist_error_prom <- sqldf('
      SELECT a.* 
           , b.prom_general_wing
           , (b.prom_general_wing - a.real_vote) AS error_general_wing
      FROM hist_error_prom  AS a
      LEFT JOIN (
                SELECT *
                FROM prom_general_wing ) AS b
                ON   (a.wing = b.wing ) ')

# 0.3. Promedio por partido y casa => HE 
prom_casa_partido <- group_by(base_promedios_wing, poll_firm, party)
prom_casa_partido <- summarise(prom_casa_partido, prom_casa_partido = mean(est_surv_vote, na.rm = TRUE))
prom_casa_partido$prom_casa_partido <- round(prom_casa_partido$prom_casa_partido ,digit=1)
hist_error_prom <- sqldf('
      SELECT a.* 
           , b.prom_casa_partido
           , (b.prom_casa_partido - a.real_vote) AS error_casa_partido
      FROM hist_error_prom  AS a
      LEFT JOIN (
                SELECT *
                FROM prom_casa_partido ) AS b
                ON    (a.poll_firm = b.poll_firm) 
                AND   (a.party = b.party ) ')

# 0.4. Promedio por ala y casa => HEW
prom_casa_wing <- group_by(base_promedios_wing, poll_firm, wing)
prom_casa_wing <- summarise(prom_casa_wing, prom_casa_wing = mean(est_surv_vote, na.rm = TRUE))
prom_casa_wing$prom_casa_wing <- round(prom_casa_wing$prom_casa_wing ,digit=1)
hist_error_prom <- sqldf('
      SELECT a.* 
           , b.prom_casa_wing
           , (b.prom_casa_wing - a.real_vote) AS error_casa_wing
      FROM hist_error_prom  AS a
      LEFT JOIN (
                SELECT *
                FROM prom_casa_wing ) AS b
                ON    (a.poll_firm = b.poll_firm) 
                AND   (a.wing = b.wing ) ')

# 0.5. Promedio por partido  y carrera => HE Endika
prom_carrera_partido <- group_by(base_promedios_wing, date_elec, party)
prom_carrera_partido <- summarise(prom_carrera_partido, prom_carrera_partido = mean(est_surv_vote, na.rm = TRUE))
prom_carrera_partido$prom_carrera_partido <- round(prom_carrera_partido$prom_carrera_partido ,digit=1)
hist_error_prom <- sqldf('
      SELECT a.* 
           , b.prom_carrera_partido
           , (b.prom_carrera_partido - a.real_vote) AS error_carrera_partido
      FROM hist_error_prom  AS a
      LEFT JOIN (
                SELECT *
                FROM prom_carrera_partido ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.party = b.party ) ')

# 0.6. Promedio por ala y casa => HE Endika
prom_carrera_wing <- group_by(base_promedios_wing, date_elec, wing)
prom_carrera_wing <- summarise(prom_carrera_wing, prom_carrera_wing = mean(est_surv_vote, na.rm = TRUE))
prom_carrera_wing$prom_carrera_wing <- round(prom_carrera_wing$prom_carrera_wing ,digit=1)
hist_error_prom <- sqldf('
      SELECT a.* 
           , b.prom_carrera_wing
           , (b.prom_carrera_wing - a.real_vote) AS error_carrera_wing
      FROM hist_error_prom  AS a
      LEFT JOIN (
                SELECT *
                FROM prom_carrera_wing ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.wing = b.wing ) ')

# 0.7. Promedio por partido, casa y carrera => HE Endika
prom_carrera_casa_partido <- group_by(base_promedios_wing, date_elec, poll_firm, party)
prom_carrera_casa_partido <- summarise(prom_carrera_casa_partido, prom_carrera_casa_partido = mean(est_surv_vote, na.rm = TRUE))
prom_carrera_casa_partido$prom_carrera_casa_partido <- round(prom_carrera_casa_partido$prom_carrera_casa_partido ,digit=1)
hist_error_prom <- sqldf('
      SELECT a.* 
           , b.prom_carrera_casa_partido
           , (b.prom_carrera_casa_partido - a.real_vote) AS error_carrera_casa_partido
      FROM hist_error_prom  AS a
      LEFT JOIN (
                SELECT *
                FROM prom_carrera_casa_partido ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.poll_firm = b.poll_firm )
                AND   (a.party = b.party ) ')

# 0.8. Promedio por ala casa y carrera => HE Endika
prom_carrera_casa_wing <- group_by(base_promedios_wing, date_elec, poll_firm, wing)
prom_carrera_casa_wing <- summarise(prom_carrera_casa_wing, prom_carrera_casa_wing = mean(est_surv_vote, na.rm = TRUE))
prom_carrera_casa_wing$prom_carrera_casa_wing <- round(prom_carrera_casa_wing$prom_carrera_casa_wing ,digit=1)
hist_error_prom <- sqldf('
      SELECT a.* 
           , b.prom_carrera_casa_wing
           , (b.prom_carrera_casa_wing - a.real_vote) AS error_carrera_casa_wing
      FROM hist_error_prom  AS a
      LEFT JOIN (
                SELECT *
                FROM prom_carrera_casa_wing ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.poll_firm = b.poll_firm )
                AND   (a.wing = b.wing ) ')

# 0.11. House Effect: "Cuanto Infraestima o sobreestima una eencuestadora a un partido [...] se calcula por partido y carrera"
hist_error_prom <- sqldf('
      SELECT * 
           , (prom_carrera_casa_partido - prom_carrera_partido) AS house_effect_e
           , (prom_carrera_casa_partido - real_vote) AS house_effect
           , (prom_carrera_casa_wing - prom_carrera_wing
           ) AS wing_effect_e
           , (prom_carrera_casa_wing - real_vote) AS wing_effect
      FROM hist_error_prom ')


# ----- PROMEDIAR ENCUESTAS: Rangos de evaluación -----
# A. Estudiar distribución
ggplot(base_promedios_wing) +  geom_histogram(aes(x = days_to_elec))+ 
  labs(title = "Análisis de ventanas temporales",
       subtitle =
         "Distribución y conteo de encuestas según los días que quedan para las elecciones",
       caption =
         "Autor: Enric Palau Payeras | Datos: Spanish elections dataset",
       y = "Conteo de encuestas a x días de la elcción")
table(base_promedios_wing$days_to_elec)

# B. Marcar varios sets de ventanas
hist_error$urna_0 <- case_when(hist_error$days_to_elec <= 0 ~ 1
                                , TRUE ~ 0 )
hist_error$urna_7 <- case_when(hist_error$days_to_elec <= 7 ~ 1
                                , TRUE ~ 0 )
hist_error$urna_15 <- case_when(hist_error$days_to_elec <= 15 ~ 1
                                , TRUE ~ 0 )
hist_error$urna_60 <- case_when(hist_error$days_to_elec <= 60 ~ 1
                                , TRUE ~ 0 )
hist_error$urna_365 <- case_when(hist_error$days_to_elec <= 365 ~ 1
                                , TRUE ~ 0 )

# C. Repetimos los promedios simples, sobre cada una de las ventanas definidas
source("./fun_promedios_ventanas.R") # Son 8 promedios con sus errores (16) y 4 house effects: 20*4 = 80 variables a meter.

# ----- DATOS FUNDAMENTALES -----
# Importamos los datos scrapeados 
BDfundamental <- read_excel("EXPORTADO/BDfundamental.xlsx") #Datos de contexto, ya tratados cronológicamente

# Preprocesamos
semma_fundamental <- BDfundamental %>% 
  rename(date_elec = "date_elect"
         , pobl = "Población"
         , pobl_densidad = "Densidad" 
         , pobl_fem_porc = "Pobl_fem_porc" 
         , pobl_kill = "Número de Homicidios"
         , pobl_kill_percienmil = "Homicidios por 100.000" 
         , pobl_suicide = "Suicidios"
         , pobl_suicide_percienmil = "Suicidios por 100.000" 
         , pobl_life_expectancy = "Esperanza de vida" 
         , pobl_idh = "IDH" 
         , pobl_im_rate = "% Inmigrantes" 
         , pobl_em_rate = "% Emigrantes"
         , pobl_pobreza_rate = "% Riesgo Pobreza" 
         , eco_pib_percap =  "PIB Per Capita"
         , eco_pib_var = "Var. anual PIB Per Capita"
         , eco_deficit = "Déficit (M.€)"
         , eco_debt_percap = "Deuda Per Cápita" 
         , eco_rate_avg = "Salario Medio Mon. Local" 
         , eco_unployement = "Desempleo"
         , eco_smi = "SMI Mon. Local"
         , eco_fisc_ing ="Ingresos fiscales (M. €)"
         , eco_fisc_ing_percap = "Ingresos fiscales (Per capita €)"
         , env_co2_percap = "CO2 t per capita"
         , env_co2 = "CO2 Totales Mt"
         , env_kwh_consum_percap = "Consumo per capita kWh"
         , env_gwh_consum = "Consumo GWh"
         , env_gwh_prod = "Generación GWh"
         , env_gwh_prod_renovable = "Generación renovable GWh"
         , gov_exp_percap = "Gasto público (%PIB)"
         , gov_exp_edu = "Gasto Educación (M.€)"
         , gov_exp_edu_percap = "Gasto Educación Per Capita"
         , gov_exp_war = "Gasto Defensa (M.€)"
         , gov_exp_war_percap = "Gasto Defensa Per Capita"
         , gov_exp_san = "Gasto Salud (M.€)"
         , gov_exp_san_percap = "G. Público Salud Per Capita"
         , gov_exp_pib = "Gasto público (%PIB)"
         , gov_cor_rate =  "Índice de Corrupción"
         , gov_pre = "Gobierno_pre") # Recodificamos las variables

# ----- CREAMOS LA TABLA SEMMA: UNION DE CONJUNTOS => (electorales + encuestas) + fundamentales -----
hist_error_prom <-
  hist_error_prom %>% 
  mutate(id_semma =
           glue("{date_elec}_{id_survey}_{days_to_elec}_{party}")) %>% 
  relocate(id_semma, .before = year_elec)
hist_error_prom$id_survey <- NULL

hist_error <-
  hist_error %>% 
  mutate(id_semma =
           glue("{date_elec}_{id_survey}_{days_to_elec}_{party}")) %>% 
  relocate(id_semma, .before = year_elec)
hist_error$id_survey <- NULL

hist_error_prom <- hist_error_prom %>% 
  filter(porc_surveys_firm > 1) %>% 
  filter(poll_firm != "OTRAS")

hist_error <- hist_error %>% 
  filter(porc_surveys_firm > 1) %>% 
  filter(poll_firm != "OTRAS")

# Split del histórico de datos electorales y encuestas, por carreras
# Nos será útil para trabajar modelos de carrera y/o análisis exploratorio. 
carreras <- split(hist_error_prom, hist_error_prom$date_elec)
source("./fun_datos_carrera.R") # Definimos dataframes para obtener al dato por campaña facilmente

# Juntamos datos fundamentales con el histórico de datos electorales y encuestas
semma <-
  hist_error_prom %>%
  left_join(semma_fundamental, id = date_elec )
#Exportamos
# write_csv(semma,
#             file = "./EXPORTADO/semma.csv")

# ----- SEMMA Explore -----
# variables de entorno
cat("Cargando entorno de gráficos...\n")
source("./analisis.R") 
# Aquí tenémos varias herramientas de exploración básicas, para los gráficos del TFM escrito  tenemos el srcipt de análisis.
# Estudio de tipologías y valores
print(semma%>% glimpse())
# Estudio de estadísticos
print(semma%>% skim())

# Sobre las dimensiones del dataset: 
analisis_psoe <- semma %>% 
  filter(date_elec == ymd("2019-04-28"), ) %>%
  filter(party == "PSOE" ) %>%
  filter(poll_firm == "CIS" )

analisis_pp <- semma %>% 
  filter(date_elec == ymd("2019-04-28"), ) %>%
  filter(party == "PP" ) %>%
  filter(poll_firm == "CIS" )

analisis <- semma %>% 
  filter(date_elec == ymd("2019-04-28"), ) %>%
  filter(poll_firm == "CIS" )

# Jugamos con 6 dimensiones (party, poll_firm, date_elec, days_to_elec, n_days, n)
ggplot(semma) + 
  geom_histogram(aes(x = errores)) #distribución del error en todo el histórico

ggplot(df_2019_1) + 
  geom_histogram(aes(x = error)) #distribución del error en una carrera

ggplot(analisis) + 
  geom_histogram(aes(x = errores)) #distribución del error en una carrera, por una poll_firm


ggplot(analisis_psoe) + 
  geom_histogram(aes(x = errores)) #distribución del error en una carrera, por una poll_firm y un partido

ggplot(analisis) + 
  geom_histogram(aes(x = errores, fill= wing)) +
  facet_grid(party ~.)

ggplot(analisis) + 
  geom_point(aes(x = errores, y = est_surv_vote, color= party)) 


# Solución, incorporar esas dimensiones en gráficos de dispersión. 
ggplot(semma, aes(x= errores , y= est_surv_vote, color= party)) + 
  geom_point(aes(size = days_to_elec ), alpha=0.3) 

ggplot(analisis, aes(x= errores , y= est_surv_vote, color= party)) + 
  geom_point(aes(size = days_to_elec ), alpha=0.3) 

ggplot(analisis_psoe, aes(x= errores , y= est_surv_vote, color= n_days_field )) + 
  geom_point(aes(size = days_to_elec ), alpha=0.3)

ggplot(analisis_pp, aes(x= errores , y= est_surv_vote, color= n_days_field )) + 
  geom_point(aes(size = days_to_elec ), alpha=0.3) 

# Pero ojo, seguimos teniendo una relación 1:N entre voto real del partido y estimaciones o errores
ggplot(df_2019_1, aes(x= real_vote , y= est_surv_vote, color= party, shape= poll_firm)) + 
  geom_point(aes(size = abs(error) ), alpha=0.3) 

ggplot(df_2019_1, aes(x= real_vote , y= est_surv_vote, color= poll_firm, shape= wing)) + 
  geom_point(aes(size = abs(error) ), alpha=0.3) 


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

# ----- SEMMA MODIFY// imputaciones -----
# Metemos nuestra depuración en otro archivo con el fin de ahorrar coste computacional.
# Imputamos con rf, por lo que conviene tener guardados los resultados de la depuración.
semma <- read_csv("EXPORTADO/semma.csv")
colnames(semma)[16] = "errores"
semma$real_vote <- NULL
semma$id_semma <- NULL
semma$date_elec <- NULL
# Test 2023
test_2023 <- semma %>% filter(year_elec == 2023)
semma <- semma %>% filter(!(year_elec == 2023),)

source("./met_SEMMA") #Estudio de métodos de imputación completo en met_SEMMA
#Disponemos aquí el tratamiento final.

rec_semma <-
  recipe(data =semma, errores ~ .)  %>% 
  step_zv(all_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>% 
  step_impute_bag (all_numeric_predictors(), impute_with = imp_vars(all_predictors()),
                   trees = 25,
                   models = NULL,
                   options = list(keepX = FALSE),
                   seed_val = sample.int(10^4, 1),
                   skip = FALSE,
                   id = rand_id("impute_bag")) 
rec_semma
semma <- bake(rec_semma %>% prep(), new_data = NULL)

# ----- SEMMA Sample -----
# Variables de las que deberíamos prescindir al modelar
semma$real_vote <- NULL
semma$id_semma <- NULL
semma$date_elec <- NULL

# Test 2023
test_2023 <- semma %>% filter(year_elec == 2023)
semma <- semma %>% filter(!(year_elec == 2023),)
# write_csv(test_2023, file = "./EXPORTADO/test_2023.csv")

# Train datos 1982-2020
set.seed(1234)
split_semma <- initial_split(semma, prop = 0.8)
train_semma <- training(split_semma)
# write_csv(train_semma, file = "./EXPORTADO/train_semma.csv")

# Test datos 1982-2020
test_semma <- testing(split_semma)
# write_csv(test_semma, file = "./EXPORTADO/test_semma.csv")

# ----- SEMMA Model -----
# Para este archivo "main" sólo exponemos nuestros modelos en test.
source("./modelos")  # Aquí tenemos el proceso de modelos completo. 
# Cargamos nuestros conjuntos test, train y test de 2023
test_2023 <- read_csv("EXPORTADO/test_2023.csv", 
                      col_types = cols(errores = col_number(), 
                                       error_casa_wing = col_number(),
                                       error_general_partido= col_number(), error_general_wing= col_number(), error_casa_partido= col_number(), 
                                       error_casa_wing= col_number(), error_carrera_partido= col_number(), 
                                       error_carrera_wing= col_number(), error_carrera_casa_partido= col_number(), 
                                       error_carrera_casa_wing= col_number(), house_effect= col_number(), wing_effect= col_number(), 
                                       pobl_kill= col_number(), pobl_kill_percienmil= col_number(), pobl_idh= col_number(), pobl_im_rate= col_number(), 
                                       pobl_em_rate= col_number(), gov_exp_edu= col_number(), gov_exp_edu_percap= col_number()))
test_semma <- read_csv("EXPORTADO/test_semma.csv")
train_semma <- read_csv("EXPORTADO/train_semma.csv")
semma <- read_csv("EXPORTADO/semma.csv")
source("./sin_error.R") # Quitamos variables de error relacionadas con el promedio (causan sobeajuste)
#### Arbol de decision: ####
arbol_ganador <- rpart(errores~ ., 
                       data = train_semma ,#Recordemos que en data teníamos los dummy por lo que relanzamos el modelo ganador sobre el auténtico train. 
                       minbucket=36, # (complejidad) número de observaciones mínimas en cada nodo final.BAJO = sobreajuste ; ALTO = error 
                       cp=0.00012212, # segundo término de complejidad con el que no procedemos pues no es común. Fijamos a 0. 
                       method = "anova", # criterio de división. Con “annova” usamos F de Snedecor, prioriza la variable que hace variar más la media de la variable dependiente.
                       maxsurrogate=0 # si no hay muchos missings, a 0 enseña solo las variables que participan
)
# Ilustramos el árbol 
rpart.plot(arbol_ganador,
           extra=1,
           tweak=3,
           nn=TRUE
)
asRules(arbol_ganador)
# Ilustramos Importancia de las variables
imp_arbol_ganador <-arbol_ganador$variable.importance
imp_arbol_ganador <- as.data.frame(imp_arbol_ganador)
dput(rownames(imp_arbol_ganador))

imp_data <- tibble::rownames_to_column(imp_arbol_ganador, "Variables") %>% arrange(desc(imp_arbol_ganador))

imp_data %>%
  mutate(Variables = fct_reorder(as.factor(Variables), imp_arbol_ganador)) %>%
  ggplot( aes(x=Variables, y=imp_arbol_ganador)) +
  theme(text = element_text(family = "ban", size=10))+
  geom_bar(stat="identity", fill="#3D74CF", alpha=.6, width=.4)+
  coord_flip()

# Predecimos en test con el modelo seleccionado
prediccion <- predict(arbol_ganador, newdata = test_semma) #hacemos las predicciones sobre test; recordemos que en test no hay dummies. 
prediccion <- as.data.frame(prediccion)#guardamos las predicciones en test
# saveRDS(prediccion, "prediccion_arbol")

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
obs_test<-tibble::rowid_to_column(test_semma, "ID")

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
pred_test<-tibble::rowid_to_column(prediccion, "ID")

# Juntamos nuestras predicciones con el conjunto de test mediante el row name que R define por defecto
eval_test_arbol <- left_join(obs_test, pred_test, by = "ID") %>%
  mutate(error = prediccion - errores ) %>% # Error del modelo
  mutate(real_vote = est_surv_vote + errores ) %>% # Error de las encuestas (error real)
  mutate(est_real_vote = est_surv_vote + prediccion ) %>% # Estimación de voto del modelo o corrección del modelo aplicada a la encuesta
  mutate(mae_arbol = mean(abs(prediccion - errores)) ) %>%
  mutate(rmse_arbol =  sqrt(mean((prediccion - errores)^2)) ) %>% 
  mutate(r_cua_arbol = 1 - sum(error^2)/sum((errores - mean(errores))^2)) 

# gráfico de error real y error del modelo
ggplot(data = eval_test_arbol,
       mapping = aes(x = prediccion, y = errores)) +
  geom_point(color = "#CC6666", alpha = 0.5, size = 4) +
  # Diagonal
  geom_abline(intercept = 0, slope = 1,
              color = "orange", size = 1.5) +
  labs(title = "Resultados del arbol en test",
       subtitle =
         "Valores deberían estar cercanos a la diagonal. Modelo: Árbol minbucket = 36 (con poda)",
       caption =
         "Autor: Enric Palau Payeras | Datos: Spanish electoral dataset",
       x = "Predicciones",
       y = "Valores Reales")+
  theme_grey
prediccion_2023 <- predict(arbol_ganador, newdata = test_2023)
prediccion_2023 <- as.data.frame(prediccion_2023)

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
obs_test_2023<-tibble::rowid_to_column(test_2023, "ID")

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
pred_test_2023<-tibble::rowid_to_column(prediccion_2023, "ID")

# Juntamos nuestras predicciones con el conjunto de test mediante el row name que R define por defecto
eval_test_arbol_2023 <- left_join(obs_test_2023, pred_test_2023, by = "ID") %>% 
  #recordemos que aquí aún no ha sucedido el evento por lo que no hay ni ERRORES ni VOTO REAL
  mutate(est_real_vote = est_surv_vote + prediccion_2023 ) # Estimación de voto del modelo o corrección del modelo aplicada a la encuesta 

# Predicción de voto por partido
eval_test_arbol_2023 <- eval_test_arbol_2023 %>% #reagrupar partits y casas 
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
                     TRUE ~ "OTRAS"))

eval_test_arbol_2023_party <- select(eval_test_arbol_2023, party, est_real_vote)
eval_test_arbol_2023_party <- group_by(eval_test_arbol_2023_party, party) 
eval_test_arbol_2023_party <- eval_test_arbol_2023_party %>% 
  summarise(prediccion_de_partido = mean(est_real_vote, na.rm = TRUE))

eval_test_arbol_2023_party <- eval_test_arbol_2023_party %>% 
  mutate(partido_estimación =
           glue("{party} = {prediccion_de_partido}"))
# Basic piechart
ggplot(eval_test_arbol_2023_party, aes(x="", y=prediccion_de_partido, fill=partido_estimación)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title = "% de voto en las elecciones de 2023")+ 
  theme(plot.title = element_text(face = "bold"))

#### Bagging y RF ####
rf_ganador <- randomForest(errores~., 
                           data = train_semma, 
                           mtry=83, #nº predictoras (mejor RF)
                           ntree=600, #iteraciones
                           sampsize=5510,#(1-1/4)*7348=5510
                           nodesize=40,#nodos finales max; minbucket del arbol
                           replace=TRUE)

rf_ganador
# Ilustramos Importancia de las variables
imp <- as.data.frame(rf_ganador[["importance"]])
imp_rf <- imp %>% filter(IncNodePurity > 0.000001)
dput(rownames(imp_rf))
imp_data <- tibble::rownames_to_column(imp_rf, "Variables") %>% arrange(desc(IncNodePurity))

library(forcats)
rf_ganador_plot<-ggplot(data = imp_data %>% filter(IncNodePurity > 20), aes(x = reorder(Variables, IncNodePurity),
                                                                            y = IncNodePurity,
                                                                            fill = IncNodePurity)) +
  labs(x = "variable", title = "Reducción del IncNodePurity") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom") + theme(legend.position = "right")
rf_ganador_plot
# Predecimos en test con el modelo seleccionado
prediccion <- predict(rf_ganador, newdata = test_semma) #hacemos las predicciones sobre test; recordemos que en test no hay dummies. 
prediccion <- as.data.frame(prediccion)#guardamos las predicciones en test
# saveRDS(prediccion, "prediccion_rf")

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
obs_test<-tibble::rowid_to_column(test_semma, "ID")

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
pred_test<-tibble::rowid_to_column(prediccion, "ID")

# Juntamos nuestras predicciones con el conjunto de test mediante el row name que R define por defecto
eval_test_rf <- left_join(obs_test, pred_test, by = "ID") %>%
  mutate(error = prediccion - errores ) %>% # Error del modelo
  mutate(real_vote = est_surv_vote + errores ) %>% # Error de las encuestas (error real)
  mutate(est_real_vote = est_surv_vote + prediccion ) # Estimación de voto del modelo o corrección del modelo aplicada a la encuesta
# write_csv(eval_test_rf, file = "./EXPORTADO/eval_test_rf.csv")

# gráfico de error real y error del modelo
ggplot(data = eval_test_rf,
       mapping = aes(x = prediccion, y = errores)) +
  geom_point(color = #"indianred3" 
               "#006EA1"
             , alpha = 0.5, size = 4) +
  # Diagonal
  geom_abline(intercept = 0, slope = 1,
              color = "orange", size = 1.5) +
  labs(title = "Resultados del arbol en test",
       subtitle =
         "Valores deberían estar cercanos a la diagonal. 
         Modelo RF: mtry=90, ntree=230, sampsize=5510, nodesize=2",
       caption =
         "Autor: Enric Palau Payeras | Datos: Spanish electoral dataset",
       x = "Predicciones",
       y = "Valores Reales")
# Predecimos en test con el modelo seleccionado
prediccion_2023 <- predict(rf_ganador, newdata = test_2023)
prediccion_2023 <- as.data.frame(prediccion_2023)

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
obs_test_2023<-tibble::rowid_to_column(test_2023, "ID")

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
pred_test_2023<-tibble::rowid_to_column(prediccion_2023, "ID")

# Juntamos nuestras predicciones con el conjunto de test mediante el row name que R define por defecto
eval_test_rf_2023 <- left_join(obs_test_2023, pred_test_2023, by = "ID") %>% 
  #recordemos que aquí aún no ha sucedido el evento por lo que no hay ni ERRORES ni VOTO REAL
  mutate(est_real_vote = est_surv_vote + prediccion_2023 ) # Estimación de voto del modelo o corrección del modelo aplicada a la encuesta 

# Predicción de voto por partido
eval_test_rf_2023 <- eval_test_rf_2023 %>% #reagrupar partits y casas 
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
                     TRUE ~ "OTRAS"))

eval_test_rf_2023_party <- select(eval_test_rf_2023, party, est_real_vote)
eval_test_rf_2023_party <- group_by(eval_test_rf_2023_party, party) 
eval_test_rf_2023_party <- eval_test_rf_2023_party %>% 
  summarise(prediccion_de_partido = mean(est_real_vote, na.rm = TRUE))



eval_test_rf_2023_party <- eval_test_rf_2023_party %>% 
  mutate(partido_estimación =
           glue("{party} = {prediccion_de_partido}"))
# Basic piechart
ggplot(eval_test_rf_2023_party, aes(x="", y=prediccion_de_partido, fill=partido_estimación)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title = "% de voto en las elecciones de 2023")+ 
  theme(plot.title = element_text(face = "bold"))
#### GBM ####
set.seed(1234)
# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
gbmgrid_1<-expand.grid(shrinkage=c(0.1), # influencia que tiene cada iteraciÃ³n (modelo). BAJO = ^ n.trees = SOBREAJUSTA
                       n.minobsinnode=c(82),# (minbucket arbol; nodesize rf y bagging) BAJO = SOBREAJUSTA
                       n.trees=c(1000),# (ntree bagging) nÃºmero de modelos que forman el essemble. SOBREAJUSTA SI ^
                       interaction.depth=c(2)) # nÃºmero total de divisiones que tiene el Ã¡rbol

# DEFINICIÃ“N DEL ENTRENAMIENTO
control<-trainControl(method = "cv", number=4, savePredictions = "all")
# AJUSTE DEL MODELO
set.seed(1234)
gbm_ganador<- train(errores~.,
                    data = train_semma,
                    method="gbm",
                    trControl=control,
                    tuneGrid= gbmgrid_1,
                    distribution= "gaussian",
                    bag.fraction=1,# 1=GBM; 0.5=SGB
                    verbose=FALSE)

gbm_ganador
# Ilustramos Importancia de las variables
imp_gbm<-summary(gbm_ganador)
imp_gbm <- imp_gbm %>% filter(rel.inf > 0.000001)
dput(rownames(imp_gbm))
imp_data <- tibble::rownames_to_column(imp_gbm, "Variables") %>% arrange(desc(rel.inf))
imp_data$var <- NULL
library(forcats)
gbm_ganador_plot<-ggplot(data = imp_data 
                         # %>% filter(rel.inf > 20)
                         , aes(x = reorder(Variables, rel.inf),
                               y = rel.inf, 
                               fill = rel.inf)) +
  labs(x = "variable", title = "Reducción del rel.inf") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom") + theme(legend.position = "right")
gbm_ganador_plot
# Predecimos en test con el modelo seleccionado
prediccion <- predict(gbm_ganador, newdata = test_semma) #hacemos las predicciones sobre test; recordemos que en test no hay dummies. 
prediccion <- as.data.frame(prediccion)#guardamos las predicciones en test
# saveRDS(prediccion, "prediccion_gbm")

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
obs_test<-tibble::rowid_to_column(test_semma, "ID")

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
pred_test<-tibble::rowid_to_column(prediccion, "ID")

# Juntamos nuestras predicciones con el conjunto de test mediante el row name que R define por defecto
eval_test_gbm <- left_join(obs_test, pred_test, by = "ID") %>%
  mutate(error = prediccion - errores ) %>% # Error del modelo
  mutate(real_vote = est_surv_vote + errores ) %>% # Error de las encuestas (error real)
  mutate(est_real_vote = est_surv_vote + prediccion ) %>% # Estimación de voto del modelo o corrección del modelo aplicada a la encuesta
  mutate(mae_gbm = mean(abs(prediccion - errores)) ) %>%
  mutate(rmse_gbm =  sqrt(mean((prediccion - errores)^2)) ) %>% 
  mutate(r_cua_gbm = 1 - sum(error^2)/sum((errores - mean(errores))^2)) 

# gráfico de error real y error del modelo
ggplot(data = eval_test_gbm,
       mapping = aes(x = prediccion, y = errores)) +
  geom_point(color = "#9F79EE", alpha = 0.5, size = 4) +
  # Diagonal
  geom_abline(intercept = 0, slope = 1,
              color = "orange", size = 1.5) +
  labs(title = "Resultados del gbm en test. MAE = 0.33",
       subtitle = "Valores deberían estar cercanos a la diagonal. Modelo: gbm_5 shrinkage = 0.5; n.minobsinnode = 150; n.trees = 800"
       # "Valores deberían estar cercanos a la diagonal. Modelo: gbm_1 shrinkage = 0.1; n.minobsinnode = 82; n.trees = 1000"
       ,
       caption =
         "Autor: Enric Palau Payeras | Datos: Spanish electoral dataset",
       x = "Predicciones",
       y = "Valores Reales")+
  theme_grey()
prediccion_2023 <- predict(gbm_ganador, newdata = test_2023)
prediccion_2023 <- as.data.frame(prediccion_2023)

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
obs_test_2023<-tibble::rowid_to_column(test_2023, "ID")

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
pred_test_2023<-tibble::rowid_to_column(prediccion_2023, "ID")

# Juntamos nuestras predicciones con el conjunto de test mediante el row name que R define por defecto
eval_test_gbm_2023 <- left_join(obs_test_2023, pred_test_2023, by = "ID") %>% 
  #recordemos que aquí aún no ha sucedido el evento por lo que no hay ni ERRORES ni VOTO REAL
  mutate(est_real_vote = est_surv_vote + prediccion_2023 ) # Estimación de voto del modelo o corrección del modelo aplicada a la encuesta 

# Predicción de voto por partido
eval_test_gbm_2023 <- eval_test_gbm_2023 %>% #reagrupar partits y casas 
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
                     TRUE ~ "OTRAS"))

eval_test_gbm_2023_party <- select(eval_test_gbm_2023, party, est_real_vote)
eval_test_gbm_2023_party <- group_by(eval_test_gbm_2023_party, party) 
eval_test_gbm_2023_party <- eval_test_gbm_2023_party %>% 
  summarise(prediccion_de_partido = mean(est_real_vote, na.rm = TRUE))
eval_test_gbm_2023_party <- eval_test_gbm_2023_party %>% 
  mutate(prediccion_de_partido = abs(prediccion_de_partido))
eval_test_gbm_2023_party <- eval_test_gbm_2023_party %>% 
  mutate(partido_estimación =
           glue("{party} = {prediccion_de_partido}"))
# Basic piechart
ggplot(eval_test_gbm_2023_party, aes(x="", y=prediccion_de_partido, fill=partido_estimación)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title = "% de voto en las elecciones de 2023 (gbm_5)")+ 
  theme(plot.title = element_text(face = "bold"))
#### Regresiones AIC BIC BORUTA (selección de variables) ####
# Esta parte fue realizada en SAS, pero ofrecemos un ejemplo de Boruta, AIC y BIC en R
# Ir a la carpeta de ódigos SAS para ver el procedimiento en SAS. 
# AIC:#
set.seed(1234)
full<-glm(errores~.,data = train_semma, family = gaussian(link = "identity"))
null<-glm(errores~1,data = train_semma, family = gaussian(link = "identity"))
selec1<-stepAIC(null,scope=list(upper=full),
                direction="both",family = gaussian(link = "identity"),trace=FALSE)
vec<-(names(selec1[[1]]))
length(vec)
dput(vec)
# BIC: #
set.seed(1234)
full<-glm(errores~.,data=train_semma, family = gaussian(link = "identity"))
null<-glm(errores~1,data=train_semma, family = gaussian(link = "identity"))

selec1<-stepAIC(null, 
                scope=list(upper=full),
                direction="both",
                family = gaussian(link = "identity"),
                trace=FALSE, 
                k=3.86) #  k=log(n)

vec<-(names(selec1[[1]]))
length(vec)
dput(vec)
# Boruta: #
out.boruta <- Boruta(errores~., data = train_semma)
print(out.boruta)
summary(out.boruta)
sal<-data.frame(out.boruta$finalDecision)
sal2<-sal[which(sal$out.boruta.finalDecision=="Confirmed"),,drop=FALSE]
dput(row.names(sal2))
length(dput(row.names(sal2)))
#### Redes ####
nnetgrid <- expand.grid(size=20, decay=0.5, bag=F)
red_ganador <- train(errores~ house_effect_e+ party_PODEMOS+ party_UP+ 
                       est_surv_vote+ prom_carrera_partido+ prom_carrera_wing+ 
                       party_VOX+ party_PSOE+ party_EA+ gov_pre_PP+ party_CIU+ 
                       party_IU+ party_CC+ party_AP+ pobl_pobreza_rate+ gov_cor_rate+ 
                       party_UPYD+ eco_pib_var+ prom_general_wing+ prom_casa_partido+ 
                       prom_general_partido+ party_UCD+ party_PCE+ pobl_suicide+ 
                       eco_unployement+ party_HB+ eco_fisc_ing_percap+ pobl_life_expectancy+ 
                       pobl_densidad+ days_to_elec+ party_BNG+ poll_firm_ASEP+ 
                       poll_firm_METROSCOPIA+ party_PA+ party_PNV+ party_EE+ 
                       lead2_party_UP+ poll_firm_SOCIOMÉTRICA+ poll_firm_ELECTOPANEL,
                     data=train_semma,
                     method="avNNet",
                     linout = TRUE,
                     maxit=1000,
                     tunegrid=nnetgrid,
                     trace=TRUE)
red_ganador

# Predecimos en test con el modelo seleccionado
prediccion <- predict(red_ganador, newdata = test_semma) #hacemos las predicciones sobre test; recordemos que en test no hay dummies. 
prediccion <- as.data.frame(prediccion)#guardamos las predicciones en test
# saveRDS(prediccion, "prediccion_arbol")

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
obs_test<-tibble::rowid_to_column(test_semma, "ID")

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
pred_test<-tibble::rowid_to_column(prediccion, "ID")

# Juntamos nuestras predicciones con el conjunto de test mediante el row name que R define por defecto
eval_test_red <- left_join(obs_test, pred_test, by = "ID") %>%
  mutate(error = prediccion - errores ) %>% # Error del modelo
  mutate(real_vote = est_surv_vote + errores ) %>% # Error de las encuestas (error real)
  mutate(est_real_vote = est_surv_vote + prediccion ) %>% # Estimación de voto del modelo o corrección del modelo aplicada a la encuesta
  mutate(mae_red = mean(abs(prediccion - errores)) ) %>%
  mutate(rmse_red =  sqrt(mean((prediccion - errores)^2)) ) %>% 
  mutate(r_cua_red = 1 - sum(error^2)/sum((errores - mean(errores))^2)) 

# gráfico de error real y error del modelo
ggplot(data = eval_test_red,
       mapping = aes(x = prediccion, y = errores)) +
  geom_point(color = "#EE799F", alpha = 0.5, size = 4) +
  # Diagonal
  geom_abline(intercept = 0, slope = 1,
              color = "orange", size = 1.5) +
  labs(title = "Resultados del red en test. MAE = 0.77",
       subtitle =
         "Valores deberían estar cercanos a la diagonal. Modelo: Red decay = 0.5; size = 20; n.trees = 1000",
       caption =
         "Autor: Enric Palau Payeras | Datos: Spanish electoral dataset",
       x = "Predicciones",
       y = "Valores Reales")+
  theme_grey()
# Predecimos en test con el modelo seleccionado
prediccion_2023 <- predict(red_ganador, newdata = test_2023)
prediccion_2023 <- as.data.frame(prediccion_2023)

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
obs_test_2023<-tibble::rowid_to_column(test_2023, "ID")

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
pred_test_2023<-tibble::rowid_to_column(prediccion_2023, "ID")

# Juntamos nuestras predicciones con el conjunto de test mediante el row name que R define por defecto
eval_test_red_2023 <- left_join(obs_test_2023, pred_test_2023, by = "ID") %>% 
  #recordemos que aquí aún no ha sucedido el evento por lo que no hay ni ERRORES ni VOTO REAL
  mutate(est_real_vote = est_surv_vote + prediccion_2023 ) # Estimación de voto del modelo o corrección del modelo aplicada a la encuesta 

# Predicción de voto por partido
eval_test_red_2023 <- eval_test_red_2023 %>% #reagrupar partits y casas 
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
                     TRUE ~ "OTRAS"))

eval_test_red_2023_party <- select(eval_test_red_2023, party, est_real_vote)
eval_test_red_2023_party <- group_by(eval_test_red_2023_party, party) 
eval_test_red_2023_party <- eval_test_red_2023_party %>% 
  summarise(prediccion_de_partido = mean(est_real_vote, na.rm = TRUE))
eval_test_red_2023_party <- eval_test_red_2023_party %>% 
  mutate(prediccion_de_partido = abs(prediccion_de_partido))
eval_test_red_2023_party <- eval_test_red_2023_party %>% 
  mutate(partido_estimación =
           glue("{party} = {prediccion_de_partido}"))
# Basic piechart
ggplot(eval_test_red_2023_party, aes(x="", y=prediccion_de_partido, fill=partido_estimación)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title = "% de voto en las elecciones de 2023 (red_1)")+ 
  theme(plot.title = element_text(face = "bold"))
#### SVM ####
SVMgrid<-expand.grid(C=c(5),
                     degree=c(3),
                     scale=c(1))
# DEFINICIÓN DEL ENTRENAMIENTO
control<-trainControl(method = "cv",
                      number=4,
                      savePredictions = "all") 
# AJUSTE DEL MODELO
set.seed(1234)
SVM_ganador <- train(data=train_semma,
                     errores~house_effect_e+ party_PODEMOS+ party_UP+ 
                       est_surv_vote+ prom_carrera_partido+ prom_carrera_wing+ 
                       party_VOX+ party_PSOE+ party_EA+ gov_pre_PP+ party_CIU+ 
                       party_IU+ party_CC+ party_AP+ pobl_pobreza_rate+ gov_cor_rate+ 
                       party_UPYD+ eco_pib_var+ prom_general_wing+ prom_casa_partido+ 
                       prom_general_partido+ party_UCD+ party_PCE+ pobl_suicide+ 
                       eco_unployement+ party_HB+ eco_fisc_ing_percap+ pobl_life_expectancy+ 
                       pobl_densidad+ days_to_elec+ party_BNG+ poll_firm_ASEP+ 
                       poll_firm_METROSCOPIA+ party_PA+ party_PNV+ party_EE+ 
                       lead2_party_UP+ poll_firm_SOCIOMÉTRICA+ poll_firm_ELECTOPANEL,
                     method="svmPoly",
                     trControl=control,
                     tuneGrid=SVMgrid,
                     verbose=FALSE)

SVM_ganador
# Predecimos en test con el modelo seleccionado
prediccion <- predict(SVM_ganador, newdata = test_semma) #hacemos las predicciones sobre test; recordemos que en test no hay dummies. 
prediccion <- as.data.frame(prediccion)#guardamos las predicciones en test
# saveRDS(prediccion, "prediccion_SVM")

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
obs_test<-tibble::rowid_to_column(test_semma, "ID")

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
pred_test<-tibble::rowid_to_column(prediccion, "ID")

# Juntamos nuestras predicciones con el conjunto de test mediante el row name que R define por defecto
eval_test_SVM <- left_join(obs_test, pred_test, by = "ID") %>%
  mutate(error = prediccion - errores ) %>% # Error del modelo
  mutate(real_vote = est_surv_vote + errores ) %>% # Error de las encuestas (error real)
  mutate(est_real_vote = est_surv_vote + prediccion ) %>% # Estimación de voto del modelo o corrección del modelo aplicada a la encuesta
  mutate(mae_SVM = mean(abs(prediccion - errores)) ) %>%
  mutate(rmse_SVM =  sqrt(mean((prediccion - errores)^2)) ) %>% 
  mutate(r_cua_SVM = 1 - sum(error^2)/sum((errores - mean(errores))^2)) 

# gráfico de error real y error del modelo
ggplot(data = eval_test_SVM,
       mapping = aes(x = prediccion, y = errores)) +
  geom_point(color = "#458B74", alpha = 0.5, size = 4) +
  # Diagonal
  geom_abline(intercept = 0, slope = 1,
              color = "orange", size = 1.5) +
  labs(title = "Resultados del SVM_pol_3 en test; MAE = 0.21",
       subtitle =
         "Los puntos deberían estar de la la diagonal. Modelo: SVM polinomial º3",
       caption =
         "Autor: Enric Palau Payeras | Datos: Spanish electoral dataset",
       x = "Predicciones",
       y = "Valores Reales")+
  theme_grey()
prediccion_2023 <- predict(SVM_ganador, newdata = test_2023)
prediccion_2023 <- as.data.frame(prediccion_2023)

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
obs_test_2023<-tibble::rowid_to_column(test_2023, "ID")

# Al no tener el id_semma añadimos un id por el row name que R define por defecto
pred_test_2023<-tibble::rowid_to_column(prediccion_2023, "ID")

# Juntamos nuestras predicciones con el conjunto de test mediante el row name que R define por defecto
eval_test_SVM_2023 <- left_join(obs_test_2023, pred_test_2023, by = "ID") %>% 
  #recordemos que aquí aún no ha sucedido el evento por lo que no hay ni ERRORES ni VOTO REAL
  mutate(est_real_vote = est_surv_vote + prediccion_2023 ) # Estimación de voto del modelo o corrección del modelo aplicada a la encuesta 

# Predicción de voto por partido
eval_test_SVM_2023 <- eval_test_SVM_2023 %>% #reagrupar partits y casas 
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
                     TRUE ~ "OTRAS"))

eval_test_SVM_2023_party <- select(eval_test_SVM_2023, party, est_real_vote)
eval_test_SVM_2023_party <- group_by(eval_test_SVM_2023_party, party) 
eval_test_SVM_2023_party <- eval_test_SVM_2023_party %>% 
  summarise(prediccion_de_partido = mean(est_real_vote, na.rm = TRUE))
eval_test_SVM_2023_party <- eval_test_SVM_2023_party %>% 
  mutate(prediccion_de_partido = abs(prediccion_de_partido))

eval_test_SVM_2023_party <- eval_test_SVM_2023_party %>% 
  mutate(partido_estimación =
           glue("{party} = {prediccion_de_partido}"))

# Basic piechart
ggplot(eval_test_SVM_2023_party, aes(x="", y=prediccion_de_partido, fill=partido_estimación)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title = "% de voto en las elecciones de 2023")+ 
  theme(plot.title = element_text(face = "bold"))+
  theme_grey()