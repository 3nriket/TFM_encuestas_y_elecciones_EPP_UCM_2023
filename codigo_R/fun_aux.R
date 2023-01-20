
# ----- FUNCIONES AUXILIARES

# download_survey_wiki: carga de encuesta desde wikipedia
download_survey_wiki <-
  function(link, date_elec, pol_parties, n_tabla = 1,
           type_elec = "national", type_surv = "national") {
    
    # Obtenemos puntero apuntando a las tablas
    wiki_table <- html_nodes(read_html(link), ".wikitable")
    
    # Obtenemos tabla
    raw_survey <-
      html_table(wiki_table[[n_tabla]], header = TRUE)
    
    # Nombres columnas
    names(raw_survey) <-
      c("poll_firm", "field_date", "n", "turnout",
        pol_parties, "lead")
    
    # Eliminamos duplicados
    raw_survey <-
      raw_survey %>% distinct()
    
    # Añadimos info
    raw_survey <- 
      raw_survey %>%
      mutate("type_elec" = type_elec, "type_surv" = type_surv,
             "year_elec" = year(date_elec),
             "date_elec" = date_elec)
    
    # Output
    return(raw_survey)
    
  }

# recod_poll_firm: recodificamos casas encuestadoras
recod_poll_firm <- function(poll_firm) {
  
  # Pasamos a mayúsculas por si no lo estuviera
  poll_firm <- str_to_upper(poll_firm)
  
  # Recodificamos
  recod_poll_firm <-
    case_when(str_detect(poll_firm, "ENCUESTAMOS") ~ "ENCUESTAMOS",
              str_detect(poll_firm, "MYWORD") ~ "MYWORD",
              str_detect(poll_firm, "CELESTE") ~ "CELESTE-TEL",
              str_detect(poll_firm, "CIS") ~ "CIS",
              str_detect(poll_firm, "TNS DEMOSCOPIA") ~ "TNS_DEMOSCOPIA",
              str_detect(poll_firm, "SW DEMOSCOPIA") ~ "SW_DEMOSCOPIA",
              str_detect(poll_firm, "GAD") ~ "GAD3",
              str_detect(poll_firm, "NETQUEST") ~ "NETQUEST",
              str_detect(poll_firm, "ASEP") ~ "ASEP",
              str_detect(poll_firm, "DEMOMÉTRICA") ~ "DEMOMÉTRICA",
              str_detect(poll_firm, "DYM") ~ "DYM",
              str_detect(poll_firm, "GALLUP") ~ "GALLUP",
              str_detect(poll_firm, "ECO") ~ "ECO",
              str_detect(poll_firm, "GESOP") ~ "GESOP",
              str_detect(poll_firm, "REDONDO") ~ "REDONDO_ASOCIADOS",
              str_detect(poll_firm, "ESTUDIO DE SOCIOLOGÍA") ~
                "ESTUDIO_SOCIO_CONSULTORES",
              str_detect(poll_firm, "INVYMARK") ~ "INVYMARK",
              str_detect(poll_firm, "METROSCOPIA") ~ "METROSCOPIA",
              str_detect(poll_firm, "IPSOS") ~ "IPSOS",
              str_detect(poll_firm, "JM&A") ~ "JM&A",
              str_detect(poll_firm, "OPINA") ~ "OPINA",
              str_detect(poll_firm, "NC REPORT") ~ "NC_REPORT",
              str_detect(poll_firm, "SIMPLE LÓGICA") |
                str_detect(poll_firm, "SIMPLE LOGICA")~ "SIMPLE_LÓGICA",
              str_detect(poll_firm, "SIGMA DOS") ~ "SIGMA_DOS",
              str_detect(poll_firm, "ICP") ~ "ICP",
              str_detect(poll_firm, "NOXA") ~ "NOXA",
              str_detect(poll_firm, "METRA SEIS") ~ "METRA_SEIS",
              str_detect(poll_firm, "VOX PÚBLICA") ~ "VOX_PÚBLICA",
              str_detect(poll_firm, "SONDAXE") ~ "SONDAXE",
              str_detect(poll_firm, "IOPE") ~ "IOPE",
              str_detect(poll_firm, "SOFEMASA") ~ "SOFEMASA",
              str_detect(poll_firm, "DECO") ~ "DECO",
              str_detect(poll_firm, "IBERCONSULTA") ~ "IBERCONSULTA",
              str_detect(poll_firm, "OBRADOIRO DE SOCIOLOXÍA") ~ "OBRADOIRO_SOCIO",
              str_detect(poll_firm, "RTVE") ~ "RTVE",
              str_detect(poll_firm, "SOCIOMÉTRICA") |
                str_detect(poll_firm, "SOCIOMETRICA") ~ "SOCIOMÉTRICA",
              str_detect(poll_firm, "ELECTOPANEL") ~ "ELECTOPANEL",
              str_detect(poll_firm, "KEYDATA") ~ "KEYDATA",
              str_detect(poll_firm, "TC") ~ "TC",
              str_detect(poll_firm, "TYPOL") ~ "TYPOL",
              str_detect(poll_firm, "ASEP") ~ "ASEP",
              str_detect(poll_firm, "CEMOP") ~ "CEMOP",
              str_detect(poll_firm, "EMOPÚBLICA") ~ "EMOPÚBLICA",
              str_detect(poll_firm, "DATA10") |
                str_detect(poll_firm, "DATA 10")~ "DATA10",
              str_detect(poll_firm, "40DB") |
                str_detect(poll_firm, "40 DB") ~ "40DB",
              str_detect(poll_firm, "IMOP") ~ "IMOP",
              str_detect(poll_firm, "TARGET") ~ "TARGET_POINT",
              str_detect(poll_firm, "VOX") ~ "VOX",
              str_detect(poll_firm, "DECO") ~ "DECO",
              str_detect(poll_firm, "INTERCAMPO") ~ "INTERCAMPO",
              str_detect(poll_firm, "TÁBULA V") ~ "TÁBULAV",
              str_detect(poll_firm, "ECO CONSULTING") ~ "ECO_CONSULTING",
              str_detect(poll_firm, "OTR–IS") ~ "OTR–IS",
              str_detect(poll_firm, "ICP–RESEARCH") ~ "ICP–RESEARCH",
              str_detect(poll_firm, "ÁGORA INTEGRAL") |
                str_detect(poll_firm, "AGORA INTEGRAL") ~ "ÁGORA_INTEGRAL",
              str_detect(poll_firm, "LA VANGUARDIA") ~ "VANGUARDIA",
              str_detect(poll_firm, "CADENA SER") ~ "CADENA_SER",
              str_detect(poll_firm, "COPE") ~ "COPE",
              str_detect(poll_firm, "ABC") ~ "ABC",
              str_detect(poll_firm, "HAMALGAMA") ~ "HAMALGAMA_MÉTRICA",
              str_detect(poll_firm, "EL CORREO") |
                str_detect(poll_firm, "ÁBACO") ~ "ÁBACO",
              str_detect(poll_firm, "GRUPO 16") |
                str_detect(poll_firm, "ALEF") ~ "ALEF",
              str_detect(poll_firm, "AP") |
                str_detect(poll_firm, "PP") ~ "PP",
              str_detect(poll_firm, "PSOE") ~ "PSOE",
              TRUE ~ "OTRAS")
  
  # Output
  return(recod_poll_firm)
  
}

# extract_est_vote: extraer % de voto de wiki
extract_est_vote <- function(x) {
  
  # Si tiene ? delante,
  if (str_sub(x, end = 1) == "?") {
    y <- NA 
  } else {
    y <- round(x, 1)
  }
  
  return(ifelse(y < 0 | y > 100 | is.na(y), NA, y))
}

