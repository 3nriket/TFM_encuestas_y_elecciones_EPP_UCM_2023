# ----- Metodología SEMMA -----
# Borramos variables del environment
rm(list = ls())
library(rpart)
library(rpart.plot)
library(rattle)
library(pROC)
library(dplyr)
library(caret)
library(dummies)
# library(MASS)
library(reshape)
library(skimr) # resumen numérico
library(corrplot) # visualizar correlaciones
library(ggthemes) # temas para tunear gráficas
library(ggThemeAssist)
library(tidymodels) # depuración datos
library(tidyverse) # modelos
library(lubridate) # fechas
library(car) # herramientas regresión
library(themis) # oversampling
library(glue) # pegar texto + variables fácilmente
library(performance) # evaluación y diagnosis modelos
library(dplyr)
library(sas7bdat)
library(randomForest)
library(gam)
library(klaR)
library(Boruta)
library(MXM)
library(plyr)
library(ggplot2) 
library(plotly)
library(gridExtra)
library(sas7bdat)
library(nnet)
library(h2o)
library(reshape)
library(recipes)
library(rpart.plot)
library(sqldf)
library(Metrics)
# Para añadir fuentes tipográficas
library(showtext)
# install.packages("ggplot2")
library(ggplot2)


# ----- SEMMA MODIFY// imputaciones -----
semma <- read_csv("EXPORTADO/semma.csv")
colnames(semma)[16] = "errores"
semma$real_vote <- NULL
semma$id_semma <- NULL
semma$date_elec <- NULL
# Test 2023
test_2023 <- semma %>% filter(year_elec == 2023)
semma <- semma %>% filter(!(year_elec == 2023),)


rec_semma_arboles <-
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
rec_semma_arboles
data_arbol <- bake(rec_semma_arboles %>% prep(), new_data = NULL)

rec_semma_estadisticos <-
  recipe(data =semma, errores ~ .)  %>% 
  step_zv(all_predictors()) %>%
  step_impute_mode (all_nominal_predictors()) %>% 
  step_impute_median (all_numeric_predictors())
rec_semma_estadisticos
data_estadisticos <- bake(rec_semma_estadisticos %>% prep(), new_data = NULL)

# Train datos 1982-2020
set.seed(1234)
split_estadisticos <- initial_split(data_estadisticos, prop = 0.8)
split_arbol <- initial_split(data_arbol, prop = 0.8)
train_estadisticos <- training(split_estadisticos)
train_arbol <- training(split_arbol)
# Test datos 1982-2020
test_estadisticos <- testing(split_estadisticos)
test_arbol <- testing(split_arbol)

surce()

bag_estadisticos <- randomForest(errores~., 
                                 data = train_estadisticos, 
                                 mtry=63, #nº predictoras (mejor RF)
                                 ntree=500, #iteraciones
                                 sampsize=5510,#(1-1/4)*7348=5510
                                 nodesize=200,#nodos finales max; minbucket del arbol
                                 replace=TRUE)
bag_estadisticos
# Predecimos en test con el modelo seleccionado
prediccion <- predict(bag_estadisticos, newdata = test_arbol) #hacemos las predicciones sobre test; recordemos que en test no hay dummies. 
prediccion <- as.data.frame(prediccion)#guardamos las predicciones en test
# Al no tener el id_semma añadimos un id por el row name que R define por defecto
prediccion<-tibble::rowid_to_column(prediccion, "ID")
# Al no tener el id_semma añadimos un id por el row name que R define por defecto
obs_test_estadisticos<-tibble::rowid_to_column(test_arbol, "ID")
# Juntamos nuestras predicciones con el conjunto de test mediante el row name que R define por defecto
eval_test_imp_estadisticos <- left_join(obs_test_estadisticos, prediccion, by = "ID") %>%
  mutate(error_a = prediccion - errores ) %>% # Error del modelo
  mutate(real_vote = est_surv_vote + errores ) %>% # Error de las encuestas (error real)
  mutate(est_real_vote = est_surv_vote + prediccion ) %>% # Estimación de voto del modelo o corrección del modelo aplicada a la encuesta
  mutate(mae_a = mean(abs(prediccion - errores)) ) %>%
  mutate(rmse_a =  sqrt(mean((prediccion - errores)^2)) ) %>% 
  mutate(r_cua_a = 1 - sum(error_a^2)/sum((errores - mean(errores))^2)) %>% 
  mutate(modelo = "media/moda")
# write_csv(eval_test_imp_estadisticos, file = "./EXPORTADO/eval_test_imp_estadisticos.csv")

bag_arbol <- randomForest(errores~., 
                          data = train_arbol, 
                          mtry=63, #nº predictoras (mejor RF)
                          ntree=500, #iteraciones
                          sampsize=5510,#(1-1/4)*7348=5510
                          nodesize=100,#nodos finales max; minbucket del arbol
                          replace=TRUE)
bag_arbol
# Predecimos en test con el modelo seleccionado
prediccion <- predict(bag_arbol, newdata = test_arbol) #hacemos las predicciones sobre test; recordemos que en test no hay dummies. 
prediccion <- as.data.frame(prediccion)#guardamos las predicciones en test
# Al no tener el id_semma añadimos un id por el row name que R define por defecto
prediccion<-tibble::rowid_to_column(prediccion, "ID")
# Al no tener el id_semma añadimos un id por el row name que R define por defecto
obs_test_arbol<-tibble::rowid_to_column(test_arbol, "ID")
# Juntamos nuestras predicciones con el conjunto de test mediante el row name que R define por defecto
eval_test_imp_arbol <- left_join(obs_test_arbol, prediccion, by = "ID") %>%
  mutate(error_a = prediccion - errores ) %>% # Error del modelo
  mutate(real_vote = est_surv_vote + errores ) %>% # Error de las encuestas (error real)
  mutate(est_real_vote = est_surv_vote + prediccion ) %>% # Estimación de voto del modelo o corrección del modelo aplicada a la encuesta
  mutate(mae_a = mean(abs(prediccion - errores)) ) %>%
  mutate(rmse_a =  sqrt(mean((prediccion - errores)^2)) ) %>% 
  mutate(r_cua_a = 1 - sum(error_a^2)/sum((errores - mean(errores))^2)) %>% 
  mutate(modelo = "bagging")
# write_csv(eval_test_imp_arbol, file = "./EXPORTADO/eval_test_imp_arbol.csv")

eval_test_test <- obs_test_arbol %>%
  mutate(prediccion = errores ) %>%
  mutate(error_a = 0 ) %>% # Error del modelo
  mutate(real_vote = est_surv_vote + errores ) %>% # Error de las encuestas (error real)
  mutate(est_real_vote = real_vote ) %>% # Estimación de voto del modelo o corrección del modelo aplicada a la encuesta
  mutate(mae_a = 0 ) %>%
  mutate(rmse_a = 0 ) %>% 
  mutate(r_cua_a = 1) %>% 
  mutate(modelo = "Encuestas(real)")  
# write_csv(eval_test_test, file = "./EXPORTADO/eval_test_test.csv")

union1<-rbind(eval_test_imp_arbol, eval_test_imp_estadisticos, eval_test_test)
cols <- c("#6495ED","#FFD700", "#F08080")
dens <- c(0.7, 0.2, 0.7)
# Gráfico de densidad en ggplot2
tres<-ggplot(union1, aes(x = abs(prediccion), fill = modelo, alpha = modelo)) +
  geom_density() + 
  scale_alpha_manual(values = dens) +
  scale_fill_manual(values = cols) + theme(legend.position = "bottom", legend.direction = "horizontal") +labs(x = "Distribución del error")
tres<-tres + theme(legend.background = element_rect(fill = NA),
    legend.position = c(0.60, 0.80))
tres
union2<-rbind(eval_test_imp_arbol, eval_test_imp_estadisticos)
cols <- c("#6495ED", "#F08080")
dens <- c(0.5, 0.5)
# Gráfico de densidad en ggplot2
dos<-ggplot(union2, aes(x = error_a, fill = modelo, alpha = modelo)) +
  geom_density() + 
  scale_alpha_manual(values = dens) +
  scale_fill_manual(values = cols) + theme(legend.position = "bottom", legend.direction = "horizontal") +labs(x = "Distribución del error cometido") + theme(legend.background = element_rect(fill = NA),
    legend.position = c(0.8, 0.9))
dos
cols <- c("#6495ED", "#F08080")
cuatro<-ggplot(data = union2,
       mapping = aes(x = prediccion, y = errores, color = modelo)) +
  scale_color_manual(values = cols) +
  geom_point(alpha = 0.5, size = 4) +
  # Diagonal
  geom_abline(intercept = 0, slope = 1,
              color = "#B596FF", size = 1.5) +
  labs(x = "Predicciones",
       y = "Valores Reales")+
  theme_grey() + theme(legend.background = element_rect(fill = NA),
    legend.position = c(0.78, 0.07), legend.direction = "horizontal") +labs(title = "Ajuste de modelos por método de imputación")
cuatro
cols <- c("#FFFFBF")

# Gráfico de densidad en ggplot2
uno <- ggplot(eval_test_test, aes(x = errores, fill = modelo)) +
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = cols)
uno <-uno + theme(legend.background = element_rect(fill = NA),
                  legend.position = c(0.84, 0.87))
uno

comparativa_imputacion <- ggarrange(cuatro, tres, uno, dos,  
                                    ncol = 2, nrow = 2)
comparativa_imputacion


a <- train_semma %>% #reagrupar partits y casas 
  mutate(party =
           case_when(str_detect(party_PP, "1") ~ "PP",
                     str_detect(party_PSOE, "1") ~ "PSOE",
                     str_detect(party_UCD, "1") ~ "UCD",
                     TRUE ~ "OTRAS")) %>% 
  mutate(gov_pre =
           case_when(str_detect(gov_pre_PP, "1") ~ "PP",
                     str_detect(gov_pre_PSOE, "1") ~ "PSOE",
                     str_detect(gov_pre_UCD , "1") ~"UCD",
                     TRUE ~ "OTRAS"))

a <- a %>% filter(party != "OTRAS")
a <- a %>%  mutate(real_vote = est_surv_vote + errores)
semma_fundamental <-  a %>% 
  select(year_elec,  party, real_vote, prom_carrera_partido,
         pobl_densidad, pobl_fem_porc, pobl, pobl_kill, pobl_kill_percienmil, pobl_suicide, pobl_suicide_percienmil, pobl_life_expectancy, pobl_idh, pobl_im_rate, pobl_em_rate, pobl_pobreza_rate,  
         eco_smi, eco_rate_avg, eco_fisc_ing, eco_fisc_ing_percap, eco_debt_percap, eco_deficit, eco_pib_var,
         env_gwh_prod, env_gwh_prod_renovable, env_gwh_consum, env_kwh_consum_percap, env_co2, env_co2_percap, eco_unployement, eco_pib_percap, 
         gov_exp_pib, gov_cor_rate, gov_exp_war, gov_exp_war_percap, gov_exp_san, gov_exp_san_percap, gov_exp_edu, gov_exp_edu_percap, gov_pre)
semma_fundamental <-  semma_fundamental %>% mutate(year_elec = as_factor(year_elec) )
semma_fundamental <-  semma_fundamental %>%  mutate( party = gov_pre)

semma_fundamental <-  semma_fundamental %>% select(year_elec, party, real_vote, pobl_densidad, pobl_fem_porc, pobl, pobl_kill, pobl_kill_percienmil, pobl_suicide, pobl_suicide_percienmil, pobl_life_expectancy, pobl_idh, pobl_im_rate, pobl_em_rate, pobl_pobreza_rate,  
                                                   eco_smi, eco_rate_avg, eco_fisc_ing, eco_fisc_ing_percap, eco_debt_percap, eco_deficit, eco_pib_var,
                                                   env_gwh_prod, env_gwh_prod_renovable, env_gwh_consum, env_kwh_consum_percap, env_co2, env_co2_percap, eco_unployement, eco_pib_percap, 
                                                   gov_exp_pib, gov_cor_rate, gov_exp_war, gov_exp_war_percap, gov_exp_san, gov_exp_san_percap, gov_exp_edu, gov_exp_edu_percap, gov_pre )

semma_fundamental<-semma_fundamental[!duplicated(semma_fundamental), ]
semma_fundamental 

newggslopegraph(semma_fundamental, year_elec, real_vote, gov_pre,
                Title = "Evolución del voto por partido y carrera",
                SubTitle = "1982-2023",
                Caption = "Enric Palau Payeras") +
  theme_gray() +
  theme(legend.position = "none")


# ----- SEMMA Explore Num -----
print(semma%>% glimpse())

print(semma%>% skim())
# ----- SEMMA Explore Graf -----
# Sobre las dimensiones del dataset: 


 

ggplot(semma) +  geom_histogram(aes(x = days_to_elec))
ggplot(semma) +  geom_bar(aes(x = days_to_elec))

# gráfico de voto real y voto según encuestas 

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
  geom_histogram(aes(x = errores))+ #distribución del error en todo el histórico

ggplot(semma) + 
  geom_histogram(aes(x = real_vote))+ #distribución del error en todo el histórico

ggplot(semma) + 
  geom_histogram(aes(x = est_surv_vote))


ggplot(df_2019_1) + 
  geom_histogram(aes(x = errores)) #distribución del errores en una carrera

ggplot(analisis) + 
  geom_histogram(aes(x = errores)) #distribución del error en una carrera, por una poll_firm


ggplot(analisis_psoe) + 
  geom_histogram(aes(x = errores)) #distribución del error en una carrera, por una poll_firm y un partido

ggplot(analisis) + 
  geom_histogram(aes(x = errores, fill= wing)) +
  facet_grid(party ~.)

ggplot(analisis) + 
  geom_point(aes(x = errores, y = est_surv_vote, color= party)) 

plot_error <- ggplot(semma) + 
  theme_base() +
  geom_histogram(aes(x = errores)) #distribución del error en todo el histórico
plot_error
plot_est <- ggplot(semma) + 
  theme_base() +
  geom_histogram(aes(x = est_surv_vote)) #distribución del error en todo el histórico
plot_est
plot_real <- ggplot(semma) + 
  theme_base() +
  geom_histogram(aes(x = real_vote)) #distribución del error en todo el histórico
plot_real

plots_obj <- ggarrange(plot_error, plot_est, plot_real,
                       ncol = 3, nrow = 1)
plots_obj

# Solución, incorporar esas dimensiones en gráficos de dispersión. 
ggplot(semma, aes(x= errores , y= est_surv_vote, color= party)) + 
  geom_point(aes(size = days_to_elec ), alpha=0.3) 

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
  geom_point(aes(size = abs(errores) ), alpha=0.3) 

ggplot(df_2019_1, aes(x= real_vote , y= est_surv_vote, color= poll_firm, shape= wing)) + 
  geom_point(aes(size = abs(errores) ), alpha=0.3) 



# Gráficos categóricas
ggplot(df_2019_1) +  geom_bar(aes(x = poll_firm, fill= party)) + 
  labs(title = "Análisis Univariante",
       subtitle =
         "wing count",
       caption =
         "Autor: Enric Palau Payeras | Datos: semma dataset",
       y = "Frecuency")

ggplot(semma, aes(x = wing, y = errores)) +
  geom_boxplot(outlier.colour = "hotpink") +
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4) +
  labs(title = "Análisis Bivariante",
       subtitle = "wing vs error",
       caption = "Source: Gapminder. Jenny Bryan rocks in gapminder vignette!!", 
       x = "wing", y = "error") 


# Gráficos continua
ggplot(analisis, aes(x=wing, y=errores, fill=wing) ) +
  geom_boxplot(alpha=0.3, outlier.colour = "blue") +
  labs(x="wing", y="errores") +
  scale_x_discrete(labels=c("Left","Right")) +
  guides(fill=FALSE) +
  coord_flip() +              
  geom_point(stat= "summary", fun.y=mean, shape=16, size=4, color="red") +
  geom_jitter(width = 0.1, alpha = 0.2)



b<-semma %>% filter(as_date(date_elec) == "2019-04-28")
 table(b$poll_firm)

semma_anal <- sqldf('
      SELECT *
      FROM semma
      WHERE poll_firm = "CIS"
      AND party = "PSOE" 
      OR party = "PP"
                ')
semma_anal <- semma %>% filter(party == "PSOE")
semma_anal <- semma_anal %>% filter(poll_firm == "CIS")



