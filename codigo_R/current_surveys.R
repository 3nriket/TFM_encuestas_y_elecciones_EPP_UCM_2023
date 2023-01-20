
# ----- DESCARGA WIKI -----

# Elecciones futuras
current_elections <-
  tibble("date_elec" = as_date(NA),
         "year" = year(date_elec),
         "month" = month(date_elec),
         "day" = day(date_elec),
         "wday" = wday(date_elec, week_start = 1)) %>%
  mutate(link = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Spanish_general_election")

current_elections <-
  tibble(current_elections,
         "pol_parties" =
           list(c("PSOE", "PP", "VOX", "UP", "CS", "ERC",
                  "MP", "JC", "PNV", "EH-BILDU", "CUP", "CC/NC",
                  "BNG", "NS", "PRC", "EV")))

# ----- PREPROCESADO -----

raw_current_surveys <-
  current_elections %>%
  select(link, date_elec, pol_parties) %>% 
  mutate(n_tabla = 1) %>% 
  pmap_df(download_survey_wiki) %>% 
  mutate(field_date =
           paste(field_date, as.character(year(today()))))

# Convertimos variables numéricas, pasamos a mayúscula las casas
current_surveys <-
  raw_current_surveys %>%
  select(c(poll_firm:turnout, type_elec:date_elec, lead,
           everything())) %>% 
  mutate(n = parse_number(n),
         turnout = parse_number(turnout),
         poll_firm = str_to_upper(poll_firm))

# Al leer los datos, los valores de votos y escaños se mezclan
# asi que nos quedamos solo con el número hasta el primer decimal
# Pasamos a NA los valores por encima de 100 o negativos
current_surveys <-
  current_surveys %>%
  mutate(across(-(poll_firm:lead),
                function(x) {
                  y <- ifelse(str_sub(as.character(x), star = 1, end = 1) == "?",
                              NA, parse_number(x))
                  y <- round(y, 1)
                  return(ifelse(y < 0 | y > 100, NA, y))
                }))

# Recodificamos casas encuestadores
current_surveys <-
  current_surveys %>%
  mutate(poll_firm = recod_poll_firm(poll_firm))

# Tratamos la fecha de campo
current_surveys <-
  current_surveys %>%
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
current_surveys <-
  current_surveys %>%
  filter(!(n_days_field == 1 & !exit_poll))

# Eliminamos encuestas realizadas por PP/PSOE/VOX
current_surveys <-
  current_surveys %>% 
  filter(!(poll_firm %in% c("PP", "PSOE", "VOX")))


# ----- EXPORTAMOS -----
write_csv(raw_current_surveys, file = "./EXPORTADO/raw_current_surveys.csv")
write_csv(current_surveys, file = "./EXPORTADO/current_surveys.csv")
write_csv(current_surveys %>% drop_na(n),
          file = "./EXPORTADO/current_surveys_with_n.csv")
