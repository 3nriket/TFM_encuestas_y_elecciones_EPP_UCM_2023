base_promedios_wing <- read_csv("EXPORTADO/base_promedios_wing.csv")
base_promedios_wing <- base_promedios_wing %>%  
  mutate(error = real_vote - est_surv_vote) %>% 
  relocate( error, .after = est_surv_vote) #sacamos el error por encuesta

hist_error <- select(base_promedios_wing, 
                     date_elec, year_elec, 
                     party, wing, 
                     poll_firm, n_days_field, days_to_elec, id_survey, porc_surveys_firm, n, exit_poll, lead_party, lead2_party, 
                     real_vote, est_surv_vote, error)  #sacamos el error por encuesta

hist_error$urna_0 <- case_when(hist_error_prom$days_to_elec <= 0 ~ 1
                               , TRUE ~ 0 )
hist_error$urna_7 <- case_when(hist_error_prom$days_to_elec <= 7 ~ 1
                               , TRUE ~ 0 )
hist_error$urna_15 <- case_when(hist_error_prom$days_to_elec <= 15 ~ 1
                                , TRUE ~ 0 )
hist_error$urna_60 <- case_when(hist_error_prom$days_to_elec <= 60 ~ 1
                                , TRUE ~ 0 )
hist_error$urna_365 <- case_when(hist_error_prom$days_to_elec <= 365 ~ 1
                                 , TRUE ~ 0 )

urna_0 <- sqldf('
      SELECT date_elec , party , wing , poll_firm , days_to_elec , 
             id_survey , real_vote , est_surv_vote , error 
      FROM hist_error
      WHERE urna_0 = 1 ')
urna_7 <- sqldf('
      SELECT date_elec , party , wing , poll_firm , days_to_elec , 
             id_survey , real_vote , est_surv_vote , error 
      FROM hist_error
      WHERE urna_7 = 1 ')
urna_15 <- sqldf('
      SELECT date_elec , party , wing , poll_firm , days_to_elec , 
             id_survey , real_vote , est_surv_vote , error 
      FROM hist_error
      WHERE urna_15 = 1 ')
urna_60 <- sqldf('
      SELECT date_elec , party , wing , poll_firm , days_to_elec , 
             id_survey , real_vote , est_surv_vote , error 
      FROM hist_error
      WHERE urna_60 = 1 ')
urna_365 <- sqldf('
      SELECT date_elec , party , wing , poll_firm , days_to_elec , 
             id_survey , real_vote , est_surv_vote , error 
      FROM hist_error
      WHERE urna_365 = 1 ')

urna_x <- urna_0 #renombrar según la urna

hist_error_urna_x <- select(urna_x, 
                            date_elec, party, wing, poll_firm, days_to_elec, id_survey, real_vote, est_surv_vote, error)  #sacamos el error por encuesta
# 0.1. Promedio por partido
x <- group_by(urna_x, party)
x <- summarise(x, prom_general_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_general_partido <- round(x$prom_general_partido ,digit=1)
urna_x <- sqldf('
      SELECT a.* 
           , b.prom_general_partido AS prom_general_partido
           , (b.prom_general_partido - a.real_vote) AS error_general_partido
      FROM hist_error_urna_x  AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON   (a.party = b.party ) ')
# 0.2. Promedio por ala
x <- group_by(urna_x, wing)
x <- summarise(x, prom_general_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_general_wing <- round(x$prom_general_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_general_wing
           , (b.prom_general_wing - a.real_vote) AS error_general_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON   (a.wing = b.wing ) ')

# 0.3. Promedio por partido y casa => HE 
x <- group_by(urna_x, poll_firm, party)
x <- summarise(x, prom_casa_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_casa_partido <- round(x$prom_casa_partido ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_casa_partido
           , (b.prom_casa_partido - a.real_vote) AS error_casa_partido
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.poll_firm = b.poll_firm) 
                AND   (a.party = b.party ) ')

# 0.4. Promedio por ala y casa => HEW
x <- group_by(urna_x, poll_firm, wing)
x <- summarise(x, prom_casa_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_casa_wing <- round(x$prom_casa_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_casa_wing
           , (b.prom_casa_wing - a.real_vote) AS error_casa_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.poll_firm = b.poll_firm) 
                AND   (a.wing = b.wing ) ')

# 0.5. Promedio por partido  y carrera => HE Endika
x <- group_by(urna_x, date_elec, party)
x <- summarise(x, prom_carrera_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_partido <- round(x$prom_carrera_partido ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_partido
           , (b.prom_carrera_partido - a.real_vote) AS error_carrera_partido
      FROM urna_x  AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.party = b.party ) ')

# 0.6. Promedio por ala y casa => HE Endika
x <- group_by(urna_x, date_elec, wing)
x <- summarise(x, prom_carrera_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_wing <- round(x$prom_carrera_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_wing
           , (b.prom_carrera_wing - a.real_vote) AS error_carrera_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.wing = b.wing ) ')

# 0.7. Promedio por partido, casa y carrera => HE Endika
x <- group_by(urna_x, date_elec, poll_firm, party)
x <- summarise(x, prom_carrera_casa_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_casa_partido <- round(x$prom_carrera_casa_partido ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_casa_partido
           , (b.prom_carrera_casa_partido - a.real_vote) AS error_carrera_casa_partido
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.poll_firm = b.poll_firm )
                AND   (a.party = b.party ) ')

# 0.8. Promedio por ala casa y carrera => HE Endika
x <- group_by(urna_x, date_elec, poll_firm, wing)
x <- summarise(x, prom_carrera_casa_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_casa_wing <- round(x$prom_carrera_casa_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_casa_wing
           , (b.prom_carrera_casa_wing - a.real_vote) AS error_carrera_casa_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.poll_firm = b.poll_firm )
                AND   (a.wing = b.wing ) ')


#0.11. House Effect: "Cuanto Infraestima o sobreestima una eencuestadora a un partido [...] se calcula por partido y carrera"
urna_x  <- sqldf('
      SELECT * 
           , (prom_carrera_casa_partido - prom_carrera_partido) AS house_effect_e
           , (prom_carrera_casa_partido - real_vote) AS house_effect
           , (prom_carrera_casa_wing - prom_carrera_wing
           ) AS wing_effect_e
           , (prom_carrera_casa_wing - real_vote) AS wing_effect
      FROM urna_x  ')

urna_0 <- urna_x
####URNA_7####
urna_x <- urna_7 #renombrar según la urna

hist_error_urna_x <- select(urna_x, 
                            date_elec, party, wing, poll_firm, days_to_elec, id_survey, real_vote, est_surv_vote, error)  #sacamos el error por encuesta
# 0.1. Promedio por partido
x <- group_by(urna_x, party)
x <- summarise(x, prom_general_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_general_partido <- round(x$prom_general_partido ,digit=1)
urna_x <- sqldf('
      SELECT a.* 
           , b.prom_general_partido AS prom_general_partido
           , (b.prom_general_partido - a.real_vote) AS error_general_partido
      FROM hist_error_urna_x  AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON   (a.party = b.party ) ')
# 0.2. Promedio por ala
x <- group_by(urna_x, wing)
x <- summarise(x, prom_general_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_general_wing <- round(x$prom_general_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_general_wing
           , (b.prom_general_wing - a.real_vote) AS error_general_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON   (a.wing = b.wing ) ')

# 0.3. Promedio por partido y casa => HE 
x <- group_by(urna_x, poll_firm, party)
x <- summarise(x, prom_casa_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_casa_partido <- round(x$prom_casa_partido ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_casa_partido
           , (b.prom_casa_partido - a.real_vote) AS error_casa_partido
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.poll_firm = b.poll_firm) 
                AND   (a.party = b.party ) ')

# 0.4. Promedio por ala y casa => HEW
x <- group_by(urna_x, poll_firm, wing)
x <- summarise(x, prom_casa_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_casa_wing <- round(x$prom_casa_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_casa_wing
           , (b.prom_casa_wing - a.real_vote) AS error_casa_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.poll_firm = b.poll_firm) 
                AND   (a.wing = b.wing ) ')

# 0.5. Promedio por partido  y carrera => HE Endika
x <- group_by(urna_x, date_elec, party)
x <- summarise(x, prom_carrera_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_partido <- round(x$prom_carrera_partido ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_partido
           , (b.prom_carrera_partido - a.real_vote) AS error_carrera_partido
      FROM urna_x  AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.party = b.party ) ')

# 0.6. Promedio por ala y casa => HE Endika
x <- group_by(urna_x, date_elec, wing)
x <- summarise(x, prom_carrera_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_wing <- round(x$prom_carrera_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_wing
           , (b.prom_carrera_wing - a.real_vote) AS error_carrera_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.wing = b.wing ) ')

# 0.7. Promedio por partido, casa y carrera => HE Endika
x <- group_by(urna_x, date_elec, poll_firm, party)
x <- summarise(x, prom_carrera_casa_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_casa_partido <- round(x$prom_carrera_casa_partido ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_casa_partido
           , (b.prom_carrera_casa_partido - a.real_vote) AS error_carrera_casa_partido
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.poll_firm = b.poll_firm )
                AND   (a.party = b.party ) ')

# 0.8. Promedio por ala casa y carrera => HE Endika
x <- group_by(urna_x, date_elec, poll_firm, wing)
x <- summarise(x, prom_carrera_casa_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_casa_wing <- round(x$prom_carrera_casa_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_casa_wing
           , (b.prom_carrera_casa_wing - a.real_vote) AS error_carrera_casa_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.poll_firm = b.poll_firm )
                AND   (a.wing = b.wing ) ')


#0.11. House Effect: "Cuanto Infraestima o sobreestima una eencuestadora a un partido [...] se calcula por partido y carrera"
urna_x  <- sqldf('
      SELECT * 
           , (prom_carrera_casa_partido - prom_carrera_partido) AS house_effect_e
           , (prom_carrera_casa_partido - real_vote) AS house_effect
           , (prom_carrera_casa_wing - prom_carrera_wing
           ) AS wing_effect_e
           , (prom_carrera_casa_wing - real_vote) AS wing_effect
      FROM urna_x  ')

urna_7 <- urna_x
####URNA_15####

urna_x <- urna_15 #renombrar según la urna

hist_error_urna_x <- select(urna_x, 
                            date_elec, party, wing, poll_firm, days_to_elec, id_survey, real_vote, est_surv_vote, error)  #sacamos el error por encuesta
# 0.1. Promedio por partido
x <- group_by(urna_x, party)
x <- summarise(x, prom_general_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_general_partido <- round(x$prom_general_partido ,digit=1)
urna_x <- sqldf('
      SELECT a.* 
           , b.prom_general_partido AS prom_general_partido
           , (b.prom_general_partido - a.real_vote) AS error_general_partido
      FROM hist_error_urna_x  AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON   (a.party = b.party ) ')
# 0.2. Promedio por ala
x <- group_by(urna_x, wing)
x <- summarise(x, prom_general_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_general_wing <- round(x$prom_general_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_general_wing
           , (b.prom_general_wing - a.real_vote) AS error_general_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON   (a.wing = b.wing ) ')

# 0.3. Promedio por partido y casa => HE 
x <- group_by(urna_x, poll_firm, party)
x <- summarise(x, prom_casa_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_casa_partido <- round(x$prom_casa_partido ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_casa_partido
           , (b.prom_casa_partido - a.real_vote) AS error_casa_partido
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.poll_firm = b.poll_firm) 
                AND   (a.party = b.party ) ')

# 0.4. Promedio por ala y casa => HEW
x <- group_by(urna_x, poll_firm, wing)
x <- summarise(x, prom_casa_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_casa_wing <- round(x$prom_casa_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_casa_wing
           , (b.prom_casa_wing - a.real_vote) AS error_casa_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.poll_firm = b.poll_firm) 
                AND   (a.wing = b.wing ) ')

# 0.5. Promedio por partido  y carrera => HE Endika
x <- group_by(urna_x, date_elec, party)
x <- summarise(x, prom_carrera_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_partido <- round(x$prom_carrera_partido ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_partido
           , (b.prom_carrera_partido - a.real_vote) AS error_carrera_partido
      FROM urna_x  AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.party = b.party ) ')

# 0.6. Promedio por ala y casa => HE Endika
x <- group_by(urna_x, date_elec, wing)
x <- summarise(x, prom_carrera_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_wing <- round(x$prom_carrera_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_wing
           , (b.prom_carrera_wing - a.real_vote) AS error_carrera_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.wing = b.wing ) ')

# 0.7. Promedio por partido, casa y carrera => HE Endika
x <- group_by(urna_x, date_elec, poll_firm, party)
x <- summarise(x, prom_carrera_casa_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_casa_partido <- round(x$prom_carrera_casa_partido ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_casa_partido
           , (b.prom_carrera_casa_partido - a.real_vote) AS error_carrera_casa_partido
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.poll_firm = b.poll_firm )
                AND   (a.party = b.party ) ')

# 0.8. Promedio por ala casa y carrera => HE Endika
x <- group_by(urna_x, date_elec, poll_firm, wing)
x <- summarise(x, prom_carrera_casa_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_casa_wing <- round(x$prom_carrera_casa_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_casa_wing
           , (b.prom_carrera_casa_wing - a.real_vote) AS error_carrera_casa_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.poll_firm = b.poll_firm )
                AND   (a.wing = b.wing ) ')


#0.11. House Effect: "Cuanto Infraestima o sobreestima una eencuestadora a un partido [...] se calcula por partido y carrera"
urna_x  <- sqldf('
      SELECT * 
           , (prom_carrera_casa_partido - prom_carrera_partido) AS house_effect_e
           , (prom_carrera_casa_partido - real_vote) AS house_effect
           , (prom_carrera_casa_wing - prom_carrera_wing
           ) AS wing_effect_e
           , (prom_carrera_casa_wing - real_vote) AS wing_effect
      FROM urna_x  ')

urna_15 <- urna_x
####URNA_60####
urna_x <- urna_60 #renombrar según la urna

hist_error_urna_x <- select(urna_x, 
                            date_elec, party, wing, poll_firm, days_to_elec, id_survey, real_vote, est_surv_vote, error)  #sacamos el error por encuesta
# 0.1. Promedio por partido
x <- group_by(urna_x, party)
x <- summarise(x, prom_general_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_general_partido <- round(x$prom_general_partido ,digit=1)
urna_x <- sqldf('
      SELECT a.* 
           , b.prom_general_partido AS prom_general_partido
           , (b.prom_general_partido - a.real_vote) AS error_general_partido
      FROM hist_error_urna_x  AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON   (a.party = b.party ) ')
# 0.2. Promedio por ala
x <- group_by(urna_x, wing)
x <- summarise(x, prom_general_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_general_wing <- round(x$prom_general_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_general_wing
           , (b.prom_general_wing - a.real_vote) AS error_general_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON   (a.wing = b.wing ) ')

# 0.3. Promedio por partido y casa => HE 
x <- group_by(urna_x, poll_firm, party)
x <- summarise(x, prom_casa_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_casa_partido <- round(x$prom_casa_partido ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_casa_partido
           , (b.prom_casa_partido - a.real_vote) AS error_casa_partido
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.poll_firm = b.poll_firm) 
                AND   (a.party = b.party ) ')

# 0.4. Promedio por ala y casa => HEW
x <- group_by(urna_x, poll_firm, wing)
x <- summarise(x, prom_casa_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_casa_wing <- round(x$prom_casa_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_casa_wing
           , (b.prom_casa_wing - a.real_vote) AS error_casa_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.poll_firm = b.poll_firm) 
                AND   (a.wing = b.wing ) ')

# 0.5. Promedio por partido  y carrera => HE Endika
x <- group_by(urna_x, date_elec, party)
x <- summarise(x, prom_carrera_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_partido <- round(x$prom_carrera_partido ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_partido
           , (b.prom_carrera_partido - a.real_vote) AS error_carrera_partido
      FROM urna_x  AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.party = b.party ) ')

# 0.6. Promedio por ala y casa => HE Endika
x <- group_by(urna_x, date_elec, wing)
x <- summarise(x, prom_carrera_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_wing <- round(x$prom_carrera_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_wing
           , (b.prom_carrera_wing - a.real_vote) AS error_carrera_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.wing = b.wing ) ')

# 0.7. Promedio por partido, casa y carrera => HE Endika
x <- group_by(urna_x, date_elec, poll_firm, party)
x <- summarise(x, prom_carrera_casa_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_casa_partido <- round(x$prom_carrera_casa_partido ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_casa_partido
           , (b.prom_carrera_casa_partido - a.real_vote) AS error_carrera_casa_partido
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.poll_firm = b.poll_firm )
                AND   (a.party = b.party ) ')

# 0.8. Promedio por ala casa y carrera => HE Endika
x <- group_by(urna_x, date_elec, poll_firm, wing)
x <- summarise(x, prom_carrera_casa_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_casa_wing <- round(x$prom_carrera_casa_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_casa_wing
           , (b.prom_carrera_casa_wing - a.real_vote) AS error_carrera_casa_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.poll_firm = b.poll_firm )
                AND   (a.wing = b.wing ) ')


#0.11. House Effect: "Cuanto Infraestima o sobreestima una eencuestadora a un partido [...] se calcula por partido y carrera"
urna_x  <- sqldf('
      SELECT * 
           , (prom_carrera_casa_partido - prom_carrera_partido) AS house_effect_e
           , (prom_carrera_casa_partido - real_vote) AS house_effect
           , (prom_carrera_casa_wing - prom_carrera_wing
           ) AS wing_effect_e
           , (prom_carrera_casa_wing - real_vote) AS wing_effect
      FROM urna_x  ')

urna_60 <- urna_x

####URNA_365####
urna_x <- urna_365 #renombrar según la urna

hist_error_urna_x <- select(urna_x, 
                            date_elec, party, wing, poll_firm, days_to_elec, id_survey, real_vote, est_surv_vote, error)  #sacamos el error por encuesta
# 0.1. Promedio por partido
x <- group_by(urna_x, party)
x <- summarise(x, prom_general_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_general_partido <- round(x$prom_general_partido ,digit=1)
urna_x <- sqldf('
      SELECT a.* 
           , b.prom_general_partido AS prom_general_partido
           , (b.prom_general_partido - a.real_vote) AS error_general_partido
      FROM hist_error_urna_x  AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON   (a.party = b.party ) ')
# 0.2. Promedio por ala
x <- group_by(urna_x, wing)
x <- summarise(x, prom_general_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_general_wing <- round(x$prom_general_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_general_wing
           , (b.prom_general_wing - a.real_vote) AS error_general_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON   (a.wing = b.wing ) ')

# 0.3. Promedio por partido y casa => HE 
x <- group_by(urna_x, poll_firm, party)
x <- summarise(x, prom_casa_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_casa_partido <- round(x$prom_casa_partido ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_casa_partido
           , (b.prom_casa_partido - a.real_vote) AS error_casa_partido
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.poll_firm = b.poll_firm) 
                AND   (a.party = b.party ) ')

# 0.4. Promedio por ala y casa => HEW
x <- group_by(urna_x, poll_firm, wing)
x <- summarise(x, prom_casa_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_casa_wing <- round(x$prom_casa_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_casa_wing
           , (b.prom_casa_wing - a.real_vote) AS error_casa_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.poll_firm = b.poll_firm) 
                AND   (a.wing = b.wing ) ')

# 0.5. Promedio por partido  y carrera => HE Endika
x <- group_by(urna_x, date_elec, party)
x <- summarise(x, prom_carrera_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_partido <- round(x$prom_carrera_partido ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_partido
           , (b.prom_carrera_partido - a.real_vote) AS error_carrera_partido
      FROM urna_x  AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.party = b.party ) ')

# 0.6. Promedio por ala y casa => HE Endika
x <- group_by(urna_x, date_elec, wing)
x <- summarise(x, prom_carrera_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_wing <- round(x$prom_carrera_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_wing
           , (b.prom_carrera_wing - a.real_vote) AS error_carrera_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.wing = b.wing ) ')

# 0.7. Promedio por partido, casa y carrera => HE Endika
x <- group_by(urna_x, date_elec, poll_firm, party)
x <- summarise(x, prom_carrera_casa_partido = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_casa_partido <- round(x$prom_carrera_casa_partido ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_casa_partido
           , (b.prom_carrera_casa_partido - a.real_vote) AS error_carrera_casa_partido
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.poll_firm = b.poll_firm )
                AND   (a.party = b.party ) ')

# 0.8. Promedio por ala casa y carrera => HE Endika
x <- group_by(urna_x, date_elec, poll_firm, wing)
x <- summarise(x, prom_carrera_casa_wing = mean(est_surv_vote, na.rm = TRUE))
x$prom_carrera_casa_wing <- round(x$prom_carrera_casa_wing ,digit=1)
urna_x  <- sqldf('
      SELECT a.* 
           , b.prom_carrera_casa_wing
           , (b.prom_carrera_casa_wing - a.real_vote) AS error_carrera_casa_wing
      FROM urna_x   AS a
      LEFT JOIN (
                SELECT *
                FROM x ) AS b
                ON    (a.date_elec = b.date_elec) 
                AND   (a.poll_firm = b.poll_firm )
                AND   (a.wing = b.wing ) ')


#0.11. House Effect: "Cuanto Infraestima o sobreestima una eencuestadora a un partido [...] se calcula por partido y carrera"
urna_x  <- sqldf('
      SELECT * 
           , (prom_carrera_casa_partido - prom_carrera_partido) AS house_effect_e
           , (prom_carrera_casa_partido - real_vote) AS house_effect
           , (prom_carrera_casa_wing - prom_carrera_wing
           ) AS wing_effect_e
           , (prom_carrera_casa_wing - real_vote) AS wing_effect
      FROM urna_x  ')

urna_365 <- urna_x

