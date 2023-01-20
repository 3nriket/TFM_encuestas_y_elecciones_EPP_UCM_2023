# ----- DESCARGA DE DATOS BRUTOS DEL MINISTERIO -----

# Datos en bruto a nivel de municipio
raw_votes_national_by_mun <-
  wiki_info %>%
  drop_na(year) %>% # eliminamos elecciones no celebradas
  select(year, month) %>% 
  # renombramos para tener variables igual que argumentos 
  # de la función municipios()
  rename(anno = year, mes = month) %>% 
  distinct(anno, mes) %>% # valores únicos
  # Si mes = 9 --> necesitamos "09" (van en texto)
  mutate(anno = as.character(anno),
         mes = if_else(mes < 10, paste0("0", as.character(mes)),
                       as.character(mes)),
         tipo_eleccion = "congreso") %>%
  pmap_df(municipios)

# Exportamos
write_csv(raw_votes_national_by_mun,
          file = "./EXPORTADO/raw_votes_national_by_mun.csv")
raw_votes_national_by_mun <- read_csv("EXPORTADO/raw_votes_national_by_mun.csv")

# ----- ESTADÍSTICAS ELECTORALES -----

# Renombramos
votes_national_by_mun <-
  raw_votes_national_by_mun %>% 
  select(anno, mes, censo_ine,
         codigo_ccaa, codigo_provincia,
         codigo_municipio, municipio,
         contains("votos"),
         codigo_partido, denominacion:votos) %>%
  rename(year = anno, month = mes,
         census = censo_ine,
         cod_ccaa = codigo_ccaa, cod_prov = codigo_provincia,
         cod_mun = codigo_municipio,
         mun = municipio,
         empty_votes = votos_blancos,
         null_votes = votos_nulos,
         allowed_votes = votos_candidaturas,
         votes = votos,
         cod_party = codigo_partido, party = denominacion,
         party_abbrev = siglas) %>% 
  # Votos totales: candidaturas + blanco
  mutate(total_votes = allowed_votes + empty_votes)

# Creamos id por municipio
votes_national_by_mun <-
  votes_national_by_mun %>% 
  mutate(id_mun = glue("{cod_ccaa}-{cod_prov}-{cod_mun}")) %>% 
  select(-c(cod_ccaa, cod_prov, cod_mun)) %>% 
  relocate(id_mun, .before = year)

# Recodificación de las siglas de los partidos
votes_national_by_mun <-
  votes_national_by_mun %>% 
   mutate(party_abbrev =
           case_when(# UCD
                     str_detect(party, "CENTRISTES DE CATALUNYA") |
                       str_detect(party_abbrev, "UCD") ~ "UCD",
                     # PSOE
                     str_detect(party, "PARTIDO SOCIALISTA OBRERO") |
                       str_detect(party_abbrev, "PSOE") |
                       str_detect(party_abbrev, "PSE-") ~ "PSOE",
                     # PCE
                     (str_detect(party, "PARTIDO COMUNISTA DE") |
                        str_detect(str_remove_all(party, " "), "P.COMUNISTA DE") |
                        str_detect(party, "PARTIT COMUNISTA D")) &
                       str_detect(party_abbrev, "PCE") ~ "PCE",
                     # AP
                     str_detect(party, "ALIANZA POP") |
                       str_detect(party, "COALICION POPULAR") |
                       str_detect(party_abbrev, "UPN-AP-PDP") ~ "AP",
                     # CIU
                     (str_detect(party, "CONVERGENCIA") &
                        str_detect(party, "UNI")) |
                        str_detect(str_to_upper(party_abbrev), "CIU") ~ "CIU",
                     # FN
                     str_detect(party, "FUERZA") &
                       str_detect(party_abbrev, "FN") ~ "FN",
                     # PA
                     str_detect(party, "ANDALUCISTA") &
                       str_detect(party_abbrev, "PA") ~ "PA",
                     # PNV
                     str_detect(party_abbrev, "PNV") |
                       str_detect(party_abbrev, "P.N.V") |
                       str_detect(party_abbrev, "EAJ") |
                       str_detect(party_abbrev, "E.A.J") ~ "PNV",
                     # HB
                     str_detect(party, "BATASUNA") ~ "HB",
                     # ERC (la valenciana also en ERC)
                     str_detect(party, "ESQUERRA REP") &
                       str_detect(party, "CAT") ~ "ERC",
                     str_detect(party_abbrev, "ESQUERRA") ~ "ERC",
                     # EE
                     str_detect(party, "EUSKADIKO EZKERRA") ~ "EE",
                     # CDS
                     str_detect(party, "CENTRO DEMOCRATICO Y SOCIAL") |
                       str_detect(party_abbrev, "CDS") ~ "CDS",
                     # CS
                     (str_detect(party, "CIUDADANOS") &
                        str_detect(party, "PARTIDO DE LA")) |
                       (str_detect(party, "CIUTADANS-") &
                          str_detect(party, "PARTIDO DE LA")) ~ "CS",
                     # IU
                     (str_detect(party, "IZQUIERDA UNIDA") |
                        str_detect(party, "IU") |
                        str_detect(party_abbrev, "IU")) & 
                       !str_detect(party, "PODEMOS") ~ "IU",
                     str_detect(str_to_upper(party_abbrev), "UPEC-") ~ "IU",
                     str_detect(party_abbrev, "EU-EV") ~ "IU",
                     str_detect(party_abbrev, "EUIB") ~ "IU",
                     str_detect(str_to_upper(party_abbrev), "-UPEC") ~ "IU",
                     # CC
                     str_detect(party, "CANARIA") &
                       str_detect(party_abbrev, "CC") ~ "CC",
                     # EA
                     str_detect(party, "EUSKO ALKARTASUNA") ~ "EA",
                     # PP
                     (str_detect(party, "PARTIDO POPULAR") |
                        str_detect(party_abbrev, "PP")) &
                       !str_detect(party, "PROVERISTA") ~ "PP",
                     # BNG
                     str_detect(party_abbrev, "BNG") ~ "BNG",
                     # UPYD
                     str_detect(party, "PROGRESO Y DEMOCRACIA") |
                       str_detect(str_to_upper(party_abbrev), "UPYD") ~ "UPYD",
                     # AMAIUR
                     str_detect(party, "AMAIUR") ~ "AMAIUR",
                     # COMPROMÍS
                     str_detect(party, "COMPROMÍS") |
                       str_detect(party, "COMPROMIS") |
                       str_detect(party_abbrev,  "COMPROMÍS") |
                       str_detect(party_abbrev,  "COMPROMIS") ~ "CCC",

                     # UP Y PODEMOS
                     str_detect(party, "UNIDOS PODEMOS") |
                       str_detect(party, "UNIDAS PODEMOS") ~ "UP",
                     str_detect(party_abbrev, "PODEMOS") ~ "PODEMOS",
                     str_detect(party, "PODEMOS") & 
                       !(str_detect(party, "UNIDOS PODEMOS") |
                           str_detect(party, "UNIDAS PODEMOS")) ~ "PODEMOS",
                     # CDC
                     str_detect(party_abbrev, "CDC") &
                       str_detect(party, "CONVERGENCIA") ~ "CDC",
                     
                     # PACMA
                     str_detect(party, "PARTIDO ANTITAURINO CONTRA") |
                       str_detect(party_abbrev, "P.A.C.M.A") ~ "PACMA",
                     # # JXC
                     # str_detect(party, "JUNTS PER CATALUNYA") |
                     #   str_detect(party, "JUNTS") |
                     #   str_detect(str_to_upper(party_abbrev), "JxCAT") ~ "JXC",
                     # VOX
                     str_detect(party, "VOX") ~ "VOX",
                     # # PRC
                     # str_detect(party, "CANTABRIA") &
                     #   (str_detect(str_remove_all(party_abbrev, " "), "P.R.C") |
                     #      str_detect(party_abbrev, "PRC")) ~ "PRC",
                     # BILDU
                     str_detect(party, "BILDU") ~ "EH-BILDU",
                     # # MP
                     # str_detect(party, "MÁS PAÍS") ~ "MP",
                     # CUP
                     str_detect(party_abbrev, "CUP") ~ "CUP",
                     # # TE
                     # str_detect(party, "TERUEL EX") ~ "TE",
                     # # NC
                     # str_detect(party_abbrev, "NC") |
                     #   str_detect(party_abbrev, "NCa") ~ "NC",
                     TRUE ~ "OTROS"))

# Suma de votos totales a nivel nacional por eleccion-partido
summary_national_year_party <-
  votes_national_by_mun %>% 
  group_by(year, month, party_abbrev) %>% 
  summarise(national_votes = sum(votes)) %>% 
  ungroup()

# Datos participación por elección
census_national_by_year <-
  votes_national_by_mun %>% 
  distinct(year, month, id_mun, .keep_all = TRUE) %>% 
  group_by(year, month) %>%
  summarise(census = sum(census, na.rm = TRUE),
            votes = sum(total_votes),
            null_votes = null_votes,
            empty_votes = empty_votes,
            turnout = 100 * votes / census) %>%
  distinct(year, month, .keep_all = TRUE) %>%
  ungroup()

# Añadimos los totales de cada partido y
# nos quedamos solo con un registro por año y partido
votes_national <-
  votes_national_by_mun %>% 
  left_join(summary_national_year_party,
            by = c("year", "month", "party_abbrev")) %>% 
  distinct(year, month, party_abbrev, .keep_all = TRUE) %>% 
  select(year, month, party_abbrev:national_votes)

# ----- EXPORTAMOS -----

write_csv(votes_national_by_mun,
          file = "./EXPORTADO/votes_national_by_mun.csv")
write_csv(votes_national,
          file = "./EXPORTADO/votes_national.csv")
write_csv(summary_national_year_party,
          file = "./EXPORTADO/summary_national_year_party.csv")
write_csv(census_national_by_year,
          file  = "./EXPORTADO/census_national_by_year.csv")


