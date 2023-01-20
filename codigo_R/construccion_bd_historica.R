

# ----- DESCARGA ELECCIONES NACIONALES, ENCUESTAS NACIONALES -----

# ----- 1982 - 2011 + 2016 -----
wiki_filter <-
  wiki_info %>%
  filter(year != 2015 & year != 2019)

raw_national_surveys <-
  wiki_filter %>%
  select(link, date_elec, pol_parties) %>% 
  pmap_df(download_survey_wiki)


# ----- 2015 -----
wiki_filter <- wiki_info %>% filter(year == 2015)

# Divididas en 4 tablas
raw_national_surveys_2015_1 <-
  wiki_filter %>% slice(1) %>% 
  select(link, date_elec, pol_parties) %>% 
  mutate(n_tabla = 1) %>% 
  pmap_df(download_survey_wiki) %>% 
  mutate(field_date = paste(field_date, "2015"))
raw_national_surveys_2015_2 <-
  wiki_filter %>% slice(2) %>% 
  select(link, date_elec, pol_parties) %>% 
  mutate(n_tabla = 2) %>% 
  pmap_df(download_survey_wiki) %>% 
  mutate(field_date = paste(field_date, "2014"))
raw_national_surveys_2015_3 <-
  wiki_filter %>% slice(3) %>% 
  select(link, date_elec, pol_parties) %>% 
  mutate(n_tabla = 3) %>% 
  pmap_df(download_survey_wiki) %>% 
  mutate(field_date = paste(field_date, "2013"))
raw_national_surveys_2015_4 <-
  wiki_filter %>% slice(4) %>% 
  select(link, date_elec, pol_parties) %>% 
  mutate(n_tabla = 4) %>% 
  pmap_df(download_survey_wiki) %>% 
  mutate(field_date = paste(field_date, "2012"))

# Juntamos tablas 2015
raw_national_surveys_2015 <-
  bind_rows(raw_national_surveys_2015_1,
            raw_national_surveys_2015_2,
            raw_national_surveys_2015_3,
            raw_national_surveys_2015_4)

# Juntamos con las demás
raw_national_surveys <-
  bind_rows(raw_national_surveys,
        raw_national_surveys_2015)


# ----- 2019 (ABRIL) -----
wiki_filter <-
  wiki_info %>% filter(year == 2019 & month == 4)

# Divididas en 4 tablas
raw_national_surveys_2019_1 <-
  wiki_filter %>% slice(1) %>% 
  select(link, date_elec, pol_parties) %>% 
  mutate(n_tabla = 1) %>% 
  pmap_df(download_survey_wiki) %>% 
  mutate(field_date = paste(field_date, "2019"))
raw_national_surveys_2019_2 <-
  wiki_filter %>% slice(2) %>% 
  select(link, date_elec, pol_parties) %>% 
  mutate(n_tabla = 2) %>% 
  pmap_df(download_survey_wiki) %>% 
  mutate(field_date = paste(field_date, "2018"))
raw_national_surveys_2019_3 <-
  wiki_filter %>% slice(3) %>% 
  select(link, date_elec, pol_parties) %>% 
  mutate(n_tabla = 3) %>% 
  pmap_df(download_survey_wiki) %>% 
  mutate(field_date = paste(field_date, "2017"))
raw_national_surveys_2019_4 <-
  wiki_filter %>% slice(4) %>% 
  select(link, date_elec, pol_parties) %>% 
  mutate(n_tabla = 4) %>% 
  pmap_df(download_survey_wiki) %>% 
  mutate(field_date = paste(field_date, "2016"))

# Juntamos tablas 2019
raw_national_surveys_2019 <-
  bind_rows(raw_national_surveys_2019_1,
            raw_national_surveys_2019_2,
            raw_national_surveys_2019_3,
            raw_national_surveys_2019_4)

# Juntamos con las demás
raw_national_surveys <-
  bind_rows(raw_national_surveys,
            raw_national_surveys_2019)



# ----- 2019 (NOV) -----
wiki_filter <-
  wiki_info %>% filter(year == 2019 & month == 11)

raw_national_surveys_2019_nov <-
  wiki_filter %>% slice(1) %>% 
  select(link, date_elec, pol_parties) %>% 
  mutate(n_tabla = 1) %>% 
  pmap_df(download_survey_wiki) %>% 
  mutate(field_date = paste(field_date, "2019"))

# Juntamos con las demás
raw_national_surveys <-
  bind_rows(raw_national_surveys,
            raw_national_surveys_2019_nov)



# ----- 2023 (2019-2020-2021) -----
 wiki_filter <-
   wiki_info %>% filter(is.na(year))
 
 raw_national_surveys_2023_1 <-
   wiki_filter %>% slice(1) %>% 
   select(link, date_elec, pol_parties) %>% 
   mutate(n_tabla = 1) %>% 
   pmap_df(download_survey_wiki) %>% 
   mutate(field_date = paste(field_date, "2021"))
 
 raw_national_surveys_2023_2 <-
   wiki_filter %>% slice(2) %>% 
   select(link, date_elec, pol_parties) %>% 
   mutate(n_tabla = 2) %>% 
   pmap_df(download_survey_wiki) %>% 
   mutate(field_date = paste(field_date, "2020"))
 
 raw_national_surveys_2023_3 <-
   wiki_filter %>% slice(3) %>% 
   select(link, date_elec, pol_parties) %>% 
   mutate(n_tabla = 3) %>% 
   pmap_df(download_survey_wiki) %>% 
   mutate(field_date = paste(field_date, "2019"))
# # Juntamos con las demás
 raw_national_surveys <-
   bind_rows(raw_national_surveys,
             raw_national_surveys_2023_1,
             raw_national_surveys_2023_2,
             raw_national_surveys_2023_3)



# ----- DEPURACIÓN -----

# Convertimos variables numéricas, pasamos a mayúscula las casas
national_surveys <-
  raw_national_surveys %>%
  select(c(poll_firm:turnout, type_elec:date_elec, lead,
           everything())) %>% 
  mutate(n = parse_number(n),
         turnout = parse_number(turnout),
         poll_firm = str_to_upper(poll_firm))

# Al leer los datos, los valores de votos y escaños se mezclan
# asi que nos quedamos solo con el número hasta el primer decimal
# Pasamos a NA los valores por encima de 100 o negativos
national_surveys <-
  national_surveys %>%
  mutate(across(-(poll_firm:lead),
                function(x) {
                  y <- ifelse(str_sub(as.character(x), star = 1, end = 1) == "?",
                               NA, parse_number(x))
                  y <- round(y, 1)
                  return(ifelse(y < 0 | y > 100, NA, y))
                }))

# Recodificamos casas encuestadores
national_surveys <-
  national_surveys %>%
  mutate(poll_firm = recod_poll_firm(poll_firm))

# Tratamos la fecha de campo
national_surveys <-
  national_surveys %>%
  # Creamos dos fechas inicial-final
  separate(col = "field_date",
           into = c("field_date_ini", "field_date_end"),
           sep = "–") %>%
  # Si está vacía la fecha de fin le imputamos la misma que de inicio
  mutate(field_date_end =
           if_else(is.na(field_date_end), field_date_ini,
                   field_date_end),
         field_date_end = dmy(field_date_end)) %>%
  # La de inicio la pasamos a número, usamos eso como día,
  # le asignamos mismo mes y año que fin
  # Además si el mes no es compatible (por tener 30d) restamos)
  mutate(field_date_ini =
           if_else(is.na(dmy(paste(parse_number(field_date_ini),
                     month(field_date_end),
                     year(field_date_end), sep = "/"))),
                   dmy(paste(parse_number(field_date_ini),
                             month(field_date_end) - 1,
                             year(field_date_end), sep = "/")),
                   dmy(paste(parse_number(field_date_ini),
                             month(field_date_end),
                             year(field_date_end), sep = "/")))) %>%
  # Si ini > fin (porque justo pillase entre meses o años),
  # le restamos un mes
  mutate(field_date_ini =
           if_else(field_date_ini <= field_date_end,
                   field_date_ini,
                   field_date_ini - months(1))) %>%
  # Si es a pie de urna, ambas fechas son iguales y
  # coinciden con día elección
  mutate(n_days_field =
           as.numeric(field_date_end - field_date_ini) + 1,
         days_to_elec = as.numeric(date_elec - field_date_end),
         exit_poll = (n_days_field == 1 & days_to_elec == 0)) %>%
  # Reordenamos columnas
  relocate(c(n_days_field, days_to_elec, exit_poll), .after = n)

# Eliminamos encuestas de un solo 1d de campo sin ser exit poll
national_surveys <-
  national_surveys %>%
  filter(!(n_days_field == 1 & !exit_poll))

# Eliminamos encuestas realizadas por PP/PSOE/VOX
national_surveys <-
  national_surveys %>% 
  filter(!(poll_firm %in% c("PP", "PSOE", "VOX")))



# ----- EXPORTAMOS -----

# Encuestas nacionales procesadas con sample size
# Nos cargamos las que no tengan tamaño muestral
write_csv(national_surveys %>% drop_na(n),
          file = "./EXPORTADO/national_surveys_with_n.csv")

# Encuestas nacionales procesadas
write_csv(national_surveys,
          file = "./EXPORTADO/national_surveys.csv")
   
# Encuestas nacionales en bruto       
write_csv(raw_national_surveys,
          file = "./EXPORTADO/raw_national_surveys.csv")

