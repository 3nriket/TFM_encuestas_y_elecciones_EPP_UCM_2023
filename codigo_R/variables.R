
# ----- Fecha de elecciones -----
# Fuente: http://www.infoelectoral.mir.es/listado-de-elecciones-celebradas-por-fecha
date_elections <-
  tibble("date_elec" =
           as_date(c("28/10/1982", "22/06/1986",
                     "29/10/1989", "06/06/1993", "03/03/1996",
                     "12/03/2000", "14/03/2004", "09/03/2008",
                     "20/11/2011",
                     rep("20/12/2015", 4), "26/06/2016",
                     rep("28/04/2019", 4), "10/11/2019",
                     rep(Inf, 3)),
                   format = "%d/%m/%Y"),
         "year" = year(date_elec),
         "month" = month(date_elec),
         "day" = day(date_elec),
         "wday" = wday(date_elec, week_start = 1))

# ----- Links wikipedia -----
wiki_info <-
  date_elections %>%
  mutate(link = glue("https://en.wikipedia.org/wiki/Opinion_polling_for_the_{year}_Spanish_general_election"),
         link = if_else(year == 2019 & month == 11,
                        "https://en.wikipedia.org/wiki/Opinion_polling_for_the_November_2019_Spanish_general_election",
                        as.character(link)),
         link = if_else(is.na(year),
                        "https://en.wikipedia.org/wiki/Nationwide_opinion_polling_for_the_next_Spanish_general_election_(2019%E2%80%932021)",
                        as.character(link)))



# ----- Añadimos orden partidos tablas -----
wiki_info <-
  tibble(wiki_info,
         "pol_parties" =
           list(#1982
                c("UCD", "PSOE", "PCE", "AP", "CIU", "FN", "PA",
                  "PNV", "HB", "ERC", "EE", "PADE", "CDS"),
                # 1986
                c("PSOE", "AP", "UCD", "PCE", "CIU", "CDS",
                  "PNV", "HB", "ERC", "EE", "PA", "PRD",
                  "CG", "MUC", "IU"),
                # 1989
                c("PSOE", "AP", "CDS", "UCD", "CIU", "IU", "PNV",
                  "HB", "EE", "PA", "ERC", "PAR", "CC",
                  "UV", "LV", "PDP", "EA", "PP", "ARM"),
                # 1993
                c("PSOE", "PP", "IU", "CDS", "CIU", "PNV", "HB",
                  "PA", "UV", "EA", "EE", "ERC", "PAR", "CC",
                  "BNG", "PAP"),
                # 1996
                c("PSOE", "PP", "IU", "CIU", "CDS", "PNV", "CC",
                  "HB", "ERC", "PAR", "EA", "BNG", "UV", "PA"),
                # 2000
                c("PP", "PSOE", "IU", "CIU", "PNV", "CC", "BNG", "EH",
                  "ERC", "NI/IC"),
                # 2004
                c("PP", "PSOE", "IU", "CIU", "PNV", "BNG", "CC", "ERC"),
                # 2008
                c("PSOE", "PP", "IU", "CIU", "ERC", "PNV", "CC",
                  "BNG", "UPYD"),
                # 2011
                c("PSOE", "PP", "IU", "CIU", "PNV", "UPYD", "ERC",
                  "BNG", "CC", "AMAIUR"),
                # 2015-1 (2015)
                c("PP", "PSOE", "IU", "UPYD", "CIU", "EH-BILDU", "PNV", "ERC",
                  "BNG", "CC", "CCC", "CS", "PODEMOS", "CDC"),
                # 2015-2 (2014)
                c("PP", "PSOE", "IU", "UPYD", "CIU", "EH-BILDU", "PNV", "ERC",
                  "BNG", "CC", "CCC", "CS", "PODEMOS"),
                # 2015-3 (2013)
                c("PP", "PSOE", "IU", "UPYD", "CIU", "EH-BILDU", "PNV", "ERC",
                  "BNG", "CC", "CCC", "CS"),
                # 2015-4 (2012)
                c("PP", "PSOE", "IU", "UPYD", "CIU", "EH-BILDU", "PNV", "ERC",
                  "BNG", "CC", "CCC"),
                # 2016
                c("PP", "PSOE", "PODEMOS", "CS", "IU", "ERC", "CDC", 
                  "PNV", "PACMA", "EH-BILDU", "CC", "UP"),
                # 2019-1 (2019)
                c("PP", "PSOE", "UP", "CS", "ERC", "PDECAT", 
                  "PNV", "PACMA", "EH-BILDU", "CC", "VOX", "CCC", 
                  "JC", "NS"),
                # 2019-2 (2018)
                c("PP", "PSOE", "UP", "CS", "ERC", "PDECAT", 
                  "PNV", "PACMA", "EH-BILDU", "CC", "VOX"),
                # 2019-3 (2017)
                c("PP", "PSOE", "UP", "CS", "ERC", "PDECAT", 
                  "PNV", "PACMA", "EH-BILDU", "CC", "VOX"),
                # 2019-4 (2016)
                c("PP", "PSOE", "UP", "CS", "ERC", "PDECAT", 
                  "PNV", "PACMA", "EH-BILDU", "CC", "VOX"),
                # 2019 - noviembre
                c("PSOE", "PP", "CS", "UP", "VOX", "ERC", "JXC", 
                  "PNV", "EH-BILDU", "CCC", "CC/NC", "NS", "PRC",
                  "MP", "CUP"),
                # 2023-1 (2021)
                c("PSOE", "PP", "VOX", "UP", "CS", "ERC",
                  "MP", "JXC", "PNV", "EH-BILDU", "CUP", "CC/NC",
                  "BNG", "NS", "PRC", "TE"),
                # 2023-2 (2020)
                c("PSOE", "PP", "VOX", "UP", "CS", "ERC",
                  "MP", "JXC", "PNV", "EH-BILDU", "CUP", "CC/NC",
                  "BNG", "NS", "PRC", "TE"),
                # 2023-3 (2019)
                c("PSOE", "PP", "VOX", "UP", "CS", "ERC",
                  "MP", "JXC", "PNV", "EH-BILDU", "CUP", "CC/NC",
                  "BNG", "NS", "PRC", "TE")))
