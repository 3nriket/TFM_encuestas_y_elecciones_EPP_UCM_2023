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
# Para añadir fuentes tipográficas
library(showtext)
library(ggpubr)


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
source("./sin_error.R")
# Ya hemos depurado los datos.
# Ya hemos imputado missings.
# Ahora tenemos que entrenar con los datos depurados. 


# ----- MODELOS: 1. Pasaremos por la selección de varibles con: -----
#### 1.1. Modelos basados en árboles: ####
#### 1.1.1. Arbol de regresión: ####
# Buscamos los parámetros y construimos el modelo. 
# MÉTDO 1: Introducción a la construcción de un árbol, con poda
# Preprocesado y modelado
library(tree)
set.seed(123)
arbol_regresion <- tree::tree(
  formula = errores ~ .,
  data = train_semma,
  split = "deviance",
  mincut = 20, # número mínimo de observaciones que debe de tener al menos uno de los nodos hijos para que se produzca la división
  minsize = 70 # número mínimo de observaciones que debe de tener un nodo para que pueda dividirse.
)
summary(arbol_regresion)
# Estructura del árbol creado
par(mar = c(1,1,1,1))
plot(x = arbol_regresion, type = "proportional")
text(x = arbol_regresion, splits = TRUE, pretty = 0, cex = 0.8, col = "firebrick")
# Pruning (const complexity pruning) por validación cruzada
# El árbol se crece al máximo posible para luego aplicar el pruning
arbol_regresion <- tree(
  formula = errores ~ .,
  data = train_semma,
  split = "deviance",
  mincut = 1,
  minsize = 2,
  mindev = 0)
# Búsqueda por validación cruzada
set.seed(123)
cv_arbol <- cv.tree(arbol_regresion, K = 4)
# Tamaño óptimo encontrado
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
paste("Tamaño óptimo encontrado:", size_optimo)
resultados_cv <- data.frame(
  n_nodos = cv_arbol$size,
  deviance = cv_arbol$dev,
  alpha = cv_arbol$k)

p1 <- ggplot(data = resultados_cv, aes(x = n_nodos, y = deviance)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = size_optimo, color = "red") +
  labs(title = "Error vs tamaño del árbol") +
  theme_grey()

p2 <- ggplot(data = resultados_cv, aes(x = alpha, y = deviance)) +
  geom_line() +
  geom_point() +
  labs(title = "Error vs penalización alpha") +
  theme_grey()
ggarrange(p1, p2)

arbol_final <- prune.tree(
  tree = arbol_regresion,
  best = size_optimo
)

par(mar = c(1,1,1,1))
plot(x = arbol_final, type = "proportional")
text(x = arbol_final, splits = TRUE, pretty = 0, cex = 0.8, col = "firebrick")

# Error de test del modelo inicial
prediccion <- predict(arbol_regresion , newdata = test_semma)
test_mae <- mean(abs(prediccion - test_semma$errores))
test_rmse <- sqrt(mean((prediccion - test_semma$errores)^2))
paste("Error de test (mae) del árbol:", round(test_mae,2))
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
# write_csv(eval_test_arbol, file = "./EXPORTADO/eval_test_arbol.csv")

# gráfico de error real y error del modelo
arbol_sin_poda<-ggplot(data = eval_test_arbol, 
                       mapping = aes(x = prediccion, y = errores)) +
  geom_point(color = "#006EA1", alpha = 0.5, size = 4) +
  # Diagonal
  geom_abline(intercept = 0, slope = 1,
              color = "orange", size = 1.5) +
  geom_vline(xintercept = test_mae, color = "red") +
  geom_hline(yintercept = test_mae, color = "red") +
  theme_grey()+
  labs(title = "Resultados del arbol en test",
       subtitle = 
         "Los valores deberían estar cercanos a la diagonal. Modelo: Árbol minbucket = 70 (sin poda)" ,
       x = "Predicciones",
       y = "Valores Reales")

# Error de test del modelo final
prediccion <- predict(arbol_final, newdata = test_semma)
test_mae <- mean(abs(prediccion - test_semma$errores))
test_rmse <- sqrt(mean((prediccion - test_semma$errores)^2))
paste("Error de test (mae) del árbol:", round(test_mae,2))
prediccion <- as.data.frame(prediccion)

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
# write_csv(eval_test_arbol, file = "./EXPORTADO/eval_test_arbol.csv")

# gráfico de error real y error del modelo
arbol_con_poda<-ggplot(data = eval_test_arbol, 
                       mapping = aes(x = prediccion, y = errores)) +
  geom_point(color = "#CC6666", alpha = 0.5, size = 4) +
  # Diagonal
  geom_abline(intercept = 0, slope = 1,
              color = "orange", size = 1.5) +
  geom_vline(xintercept = test_mae, color = "red") +
  geom_hline(yintercept = test_mae, color = "red") +
  theme_grey()+
  labs(title = "Resultados del arbol en test" ,
       subtitle = 
         " Los valores deberían estar cercanos a la diagonal. Modelo: Árbol minbucket = 70 (con poda)" ,
       x = "Predicciones",
       y = "Valores Reales")

comparativa_arbol <- ggarrange(arbol_sin_poda, arbol_con_poda,
                                ncol = 1, nrow = 2)
comparativa_arbol

test_mae <- mean(abs(predicciones - test_semma$errores))
test_rmse <- sqrt(mean((predicciones - test_semma$errores)^2))
paste("Error de test (mae) del árbol final:", round(test_mae,2))


# MÉTDO 2:  PARA LOCALIZAR LOS PARÁMETROS ÓPTIMOS (Bucle de validación cruzada simple)
simple_cross_validation <- function(list_of_minbucket) {
  # estructura de los resultados
  statistics <- data.frame(minbucket   = integer(),
                           RMSE = double(),
                           Rsquared = double(),
                           MAE = double())
  
  set.seed(1234)
  
  # DEFINICIÓN DEL ENTRENAMIENTO
  control<-trainControl(method = "cv",number=4,savePredictions = "all")
  
  # HIPERPARÁMETROS, CP y MINBUCKET
  arbolgrid <-  expand.grid(cp=c(0)) 
  
  # Comienza el bucle
  for (bucket in list_of_minbucket)
  {
    # Arbol
    arbolcaret<- train(errores ~ ., 
                       data=train_semma,
                       method="rpart",
                       trControl=control,
                       tuneGrid=arbolgrid,
                       control = rpart.control(minbucket = bucket))
    RMSE = arbolcaret[["results"]][["RMSE"]]
    Rsquared = arbolcaret[["results"]][["Rsquared"]]
    MAE = arbolcaret[["results"]][["MAE"]]
    #Insertamos en Dataframe
    statistics[nrow(statistics) + 1,] <- c(bucket,RMSE,Rsquared, MAE)
  }
  return(statistics)
}

    # arbol_1 ======================================================================
      # GRID DE PARÁMETROS
    set.seed(1234)
    list_minbucket <- seq(from=70, to=1500, by=30) # red de minbuckets: desde un 1% hasta un 20% de las observaciones de 50 en 50
    cross_val_simple <- simple_cross_validation(list_minbucket) %>% arrange(desc(MAE))
   
      # AJUSTE DEL MODELO
    size_optimo <- rev(cross_val_simple$minbucket)[which.min(rev(cross_val_simple$MAE))]
    paste("Tamaño óptimo encontrado:", size_optimo)
    
    ggplot(data = cross_val_simple, aes(x = minbucket, y = MAE)) +
      geom_line() + 
      geom_point() +
      geom_vline(xintercept = size_optimo, color = "red") +
      labs(title = "Error vs tamaño del árbol") +
      theme_grey()
    
    set.seed(1234)
    list_minbucket <- seq(from=70, to=600, by=2) # red de minbuckets: desde un 1% hasta un 20% de las observaciones de 50 en 50
    cross_val_simple <- simple_cross_validation(list_minbucket) %>% arrange(desc(MAE))
    
    # AJUSTE DEL MODELO
    size_optimo <- rev(cross_val_simple$minbucket)[which.min(rev(cross_val_simple$MAE))]
    paste("Tamaño óptimo encontrado:", size_optimo)
    
    ggplot(data = cross_val_simple, aes(x = minbucket, y = MAE)) +
      geom_line() + 
      geom_point() +
      geom_vline(xintercept = size_optimo, color = "red") +
      labs(title = "Error vs tamaño del árbol") +
      theme_bw()
    
      # ILUSTRACIÓN DEL MEJOR ÁRBOL E IMPORTANCIA DE LAS VARIABLES
      arbol_1 <- rpart(errores~ ., 
                     data = train_semma ,
                     minbucket=70, # (complejidad) número de observaciones mínimas en cada nodo final.BAJO = sobreajuste ; ALTO = error 
                     cp=0, # segundo término de complejidad con el que no procedemos pues no es común. Fijamos a 0. 
                     method = "anova", # criterio de división. Con “annova” usamos F de Snedecor, prioriza la variable que hace variar más la media de la variable dependiente.
                     maxsurrogate=0 # si no hay muchos missings, a 0 enseña solo las variables que participan
                      )
      # Ilustramos el árbol 
      rpart.plot(arbol_1, 
                 type = 4, 
                 extra = 1, 
                 branch.lty = 3, 
                 box.palette = "RdYlGn")
      rpart.plot(arbol_1,
               extra=1,
               tweak=3,
               type = 1,
               branch.lty = 3,
               box.palette = "RdYlGn",
               nn=TRUE
      )
      asRules(arbol_1)
    
      # Ilustramos Importancia de las variables
      par(cex=0.7) #antes de gráficos cambia tamaño texto
      barplot(arbol_1$variable.importance,col="orange")
    
      imp_arbol_1 <-arbol_1$variable.importance
      imp_data_arbol_1 <- as.data.frame(imp_arbol_1)
      dput(rownames(imp_data_arbol_1))
      
      imp_data <- tibble::rownames_to_column(imp_data_arbol_1, "Variables") %>% arrange(desc(imp_arbol_1))
      imp_data %>%
      mutate(Variables = fct_reorder(as.factor(Variables), imp_arbol_1)) %>%
      ggplot( aes(x=Variables, y=imp_arbol_1)) +
      theme(text = element_text(family = "ban", size=10))+
      geom_bar(stat="identity", fill="#3D74CF", alpha=.6, width=.4)+
      theme_grey()+
      coord_flip()
    
      # COMPARATIVA DE ÁRBOLES POR VALIDACIÓN CRUZADA REPETIDA
      detach(package:plyr)
      source("cruzada arbol continua.R") 
      medias_arbol_1<-cruzadaarbol(data= train_semma,
                                   vardep="errores",
                                   listconti=c("est_surv_vote", "house_effect_e", "prom_general_partido", 
                                               "prom_carrera_partido", "days_to_elec", "party_UP", "party_VOX", 
                                               "pobl_fem_porc", "gov_exp_edu_percap", "pobl_im_rate", "prom_casa_partido", 
                                               "prom_carrera_wing", "party_PSOE", "prom_carrera_casa_wing", 
                                               "eco_deficit", "pobl_suicide_percienmil", "pobl_kill", "party_CCC", 
                                               "party_BNG", "gov_exp_war", "party_ERC", "pobl_suicide", "porc_surveys_firm", 
                                               "wing_LEFT"), # dput(rownames(imp_data_arbol_1))
                                   listclass=c(""),
                                   grupos=4, # validamos sobre 1/4 de nuestras observaciones
                                   sinicio=1234,
                                   repe=10, # Reiteramos las predicciones de nuestro modelo 10 veces. Sube variabilidad
                                   cp=0,
                                   minbucket=70) # parámetro que identifica el modelo
      medias_arbol_1$modelo="arbol_1"
      saveRDS(medias_arbol_1, "arbol_1")
      medias_arbol_1 <- readRDS("arbol_1")
      
      prediccion <- predict(arbol_1 , newdata = test_semma)
      test_mae <- mean(abs(prediccion - test_semma$errores))
      test_rmse <- sqrt(mean((prediccion - test_semma$errores)^2))
      paste("Error de test (mae) del árbol:", round(test_mae,2))
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
      # write_csv(eval_test_arbol, file = "./EXPORTADO/eval_test_arbol.csv")
      
      # gráfico de error real y error del modelo
      ggplot(data = eval_test_arbol, 
                             mapping = aes(x = prediccion, y = errores)) +
        geom_point(color = "#006EA1", alpha = 0.5, size = 4) +
        # Diagonal
        geom_abline(intercept = 0, slope = 1,
                    color = "orange", size = 1.5) +
        theme_grey()+
        labs(title = "Resultados del arbol en test: MAE = 1.48358",
             subtitle = 
               "Los valores deberían estar cercanos a la diagonal. Modelo: Árbol minbucket = 70 (sin poda)" ,
             x = "Predicciones",
             y = "Valores Reales")
      
      
    # arbol_2 ======================================================================
      arbolcaret <- rpart(errores ~ ., data = train_semma, cp = 0)
      printcp(arbolcaret)
      plotcp(arbolcaret)
      head(arbolcaret$cptable, 10)
      xerror <- arbolcaret$cptable[,"xerror"]
      imin.xerror <- which.min(xerror)
      # Valor óptimo
      arbolcaret$cptable[imin.xerror, ]
      # Límite superior "oneSE rule" y complejidad mínima por debajo de ese valor
      upper.xerror <- xerror[imin.xerror] + arbolcaret$cptable[imin.xerror, "xstd"]
      icp <- min(which(xerror <= upper.xerror))
      cp <- arbolcaret$cptable[icp, "CP"]
      
      # ILUSTRACIÓN DEL ÁRBOL E IMPORTANCIA DE LAS VARIABLES
      arbol_2 <- rpart(errores~ ., 
                     data = train_semma ,
                     minbucket=199, # (complejidad) número de observaciones mínimas en cada nodo final.BAJO = sobreajuste ; ALTO = error 
                     cp=0.00012, # segundo término de complejidad con el que no procedemos pues no es común. Fijamos a 0. 
                     method = "anova", # criterio de división. Con “annova” usamos F de Snedecor, prioriza la variable que hace variar más la media de la variable dependiente.
                     maxsurrogate=0 # si no hay muchos missings, a 0 enseña solo las variables que participan
                      )
      # Ilustramos el árbol 
      rpart.plot(arbol_2,
               extra=1,
               tweak=1.2,
               nn=TRUE
                )
      asRules(arbol_2)
    
      # Ilustramos Importancia de las variables
      imp_arbol_2 <-arbol_2$variable.importance
      imp_data_arbol_2 <- as.data.frame(imp_arbol_2)
      dput(rownames(imp_data_arbol_2))
      

      imp_data <- tibble::rownames_to_column(imp_data_arbol_2, "Variables") %>% arrange(desc(imp_arbol_2))
      
      imp_data %>%
      mutate(Variables = fct_reorder(as.factor(Variables), imp_arbol_2)) %>%
      ggplot( aes(x=Variables, y=imp_arbol_2)) +
        theme(text = element_text(family = "ban", size=10))+
        geom_bar(stat="identity", fill="#3D74CF", alpha=.6, width=.4)+
        theme_grey()+
        coord_flip()
    
      # COMPARATIVA DE ÁRBOLES POR VALIDACIÓN CRUZADA REPETIDA
      medias_arbol_2<-cruzadaarbol(data= train_semma,
                                 vardep="errores",
                                 listconti=c("est_surv_vote", "house_effect_e", "prom_carrera_partido", 
                                             "prom_general_partido", "days_to_elec", "pobl_fem_porc", "party_UP", 
                                             "party_VOX", "prom_carrera_casa_partido", "party_PODEMOS", "env_gwh_consum", 
                                             "party_PSOE", "prom_carrera_wing", "env_co2_percap", "party_CS", 
                                             "gov_exp_edu", "pobl_kill", "pobl", "prom_carrera_casa_wing", 
                                             "party_AP", "gov_exp_war", "gov_exp_pib", "pobl_kill_percienmil", 
                                             "party_UPYD", "year_elec", "wing_LEFT", "env_co2", "party_IU", 
                                             "n", "env_kwh_consum_percap", "prom_casa_partido", "pobl_em_rate", 
                                             "prom_general_wing", "lead2_party_UCD", "gov_exp_edu_percap", 
                                             "lead2_party_PSOE", "party_CCC", "pobl_suicide", "n_days_field", 
                                             "eco_pib_percap", "eco_pib_var", "pobl_suicide_percienmil", "prom_casa_wing", 
                                             "wing_effect_e", "env_gwh_prod", "party_BNG", "porc_surveys_firm", 
                                             "poll_firm_CELESTE.TEL", "pobl_densidad", "urna_365", "eco_deficit", 
                                             "gov_pre_PSOE", "eco_unployement", "lead_party_PP", "poll_firm_IMOP", 
                                             "party_ERC", "lead2_party_PP", "poll_firm_NC_REPORT", "gov_exp_war_percap", 
                                             "party_PNV", "pobl_idh", "pobl_life_expectancy", "urna_15", "gov_pre_PP", 
                                             "party_HB", "poll_firm_SIMPLE_LÓGICA", "eco_fisc_ing_percap", 
                                             "pobl_pobreza_rate", "poll_firm_DYM", "poll_firm_SOCIOMÉTRICA", 
                                             "party_EA", "party_PA", "lead_party_PSOE", "lead2_party_UP", 
                                             "eco_fisc_ing", "urna_7", "lead_party_PODEMOS"),
                                 listclass=c(""),
                                 grupos=4, # validamos sobre 1/4 de nuestras observaciones
                                 sinicio=1234,
                                 repe=10, # Reiteramos las predicciones de nuestro modelo 10 veces. Sube variabilidad
                                 cp=0.00012,
                                 minbucket=199)
      medias_arbol_2$modelo="arbol_2"
      saveRDS(medias_arbol_2, "arbol_2")
      medias_arbol_2 <- readRDS("arbol_2")
      
      prediccion <- predict(arbol_2 , newdata = test_semma)
      test_mae <- mean(abs(prediccion - test_semma$errores))
      test_rmse <- sqrt(mean((prediccion - test_semma$errores)^2))
      paste("Error de test (mae) del árbol:", round(test_mae,2))
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
      # write_csv(eval_test_arbol, file = "./EXPORTADO/eval_test_arbol.csv")
      
      # gráfico de error real y error del modelo
      ggplot(data = eval_test_arbol, 
             mapping = aes(x = prediccion, y = errores)) +
        geom_point(color = "#006EA1", alpha = 0.5, size = 4) +
        # Diagonal
        geom_abline(intercept = 0, slope = 1,
                    color = "orange", size = 1.5) +
        theme_grey()+
        labs(title = "Resultados del arbol en test: MAE = 1.95",
             subtitle = 
               "Los valores deberían estar cercanos a la diagonal. Modelo: Árbol minbucket = 199 (con poda)" ,
             x = "Predicciones",
             y = "Valores Reales")
      
    # arbol_3 ======================================================================
      # ILUSTRACIÓN DEL ÁRBOL E IMPORTANCIA DE LAS VARIABLES
      arbol_3 <- rpart(errores~ ., 
                       data = train_semma ,
                       minbucket=36, # (complejidad) número de observaciones mínimas en cada nodo final.BAJO = sobreajuste ; ALTO = error 
                       cp=0.00012212, # segundo término de complejidad con el que no procedemos pues no es común. Fijamos a 0. 
                       method = "anova", # criterio de división. Con “annova” usamos F de Snedecor, prioriza la variable que hace variar más la media de la variable dependiente.
                       maxsurrogate=0 # si no hay muchos missings, a 0 enseña solo las variables que participan
      )
      # Ilustramos el árbol 
      rpart.plot(arbol_3,
                 extra=1,
                 tweak=1.1,
                 nn=TRUE
      )
      asRules(arbol_3)
      
      # Ilustramos Importancia de las variables
      imp_arbol_3 <-arbol_3$variable.importance
      imp_data_arbol_3 <- as.data.frame(imp_arbol_3)
      dput(rownames(imp_data_arbol_3))
      
      imp_data <- tibble::rownames_to_column(imp_data_arbol_3, "Variables") %>% arrange(desc(imp_arbol_3))
      
      imp_data %>% 
        mutate(Variables = fct_reorder(as.factor(Variables), imp_arbol_3)) %>%
        ggplot( aes(x=Variables, y=imp_arbol_3)) +
        theme(text = element_text(family = "ban", size=10))+
        geom_bar(stat="identity", fill="#3D74CF", alpha=.6, width=.4)+
        theme_grey()+
        coord_flip()
      
      # COMPARATIVA DE ÁRBOLES POR VALIDACIÓN CRUZADA REPETIDA
      medias_arbol_3<-cruzadaarbol(data= train_semma,
                                 vardep="errores",
                                 listconti=c("est_surv_vote", "house_effect_e", "prom_carrera_partido", 
                                             "prom_general_partido", "prom_carrera_casa_partido", "days_to_elec", 
                                             "party_UP", "pobl_fem_porc", "party_VOX", "party_PODEMOS", "pobl_kill", 
                                             "pobl_im_rate", "party_PSOE", "gov_exp_edu", "prom_carrera_wing", 
                                             "pobl", "party_UPYD", "party_IU", "prom_carrera_casa_wing", "pobl_kill_percienmil", 
                                             "eco_rate_avg", "env_co2", "prom_casa_partido", "gov_exp_edu_percap", 
                                             "party_CCC", "pobl_em_rate", "pobl_suicide", "party_BNG"),
                                 listclass=c(""),
                                 grupos=4, # validamos sobre 1/4 de nuestras observaciones
                                 sinicio=1234,
                                 repe=10, # Reiteramos las predicciones de nuestro modelo 10 veces. Sube variabilidad
                                 cp=0.00012212,
                                 minbucket=36)
      medias_arbol_3$modelo="arbol_3"
      saveRDS(medias_arbol_3, "arbol_3")
      medias_arbol_3 <- readRDS("arbol_3")
      
      prediccion <- predict(arbol_3 , newdata = test_semma)
      test_mae <- mean(abs(prediccion - test_semma$errores))
      test_rmse <- sqrt(mean((prediccion - test_semma$errores)^2))
      paste("Error de test (mae) del árbol:", round(test_mae,2))
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
      # write_csv(eval_test_arbol, file = "./EXPORTADO/eval_test_arbol.csv")
      
      # gráfico de error real y error del modelo
      ggplot(data = eval_test_arbol, 
             mapping = aes(x = prediccion, y = errores)) +
        geom_point(color = "#006EA1", alpha = 0.5, size = 4) +
        # Diagonal
        geom_abline(intercept = 0, slope = 1,
                    color = "orange", size = 1.5) +
        theme_grey()+
        labs(title = "Resultados del arbol en test: MAE = 1.11",
             subtitle = 
               "Los valores deberían estar cercanos a la diagonal. Modelo: Árbol minbucket = 36 (con poda)" ,
             x = "Predicciones",
             y = "Valores Reales")

    # arbol_4 ======================================================================
      list_minbucket <- seq(from=1000, to=1500, by=5) # red de minbuckets más cerrada que la anterior
      cross_val_simple <- simple_cross_validation(list_minbucket) %>% arrange(desc(MAE))
       # ILUSTRACIÓN DEL MEJOR ÁRBOL E IMPORTANCIA DE LAS VARIABLES
      arbol_4 <- rpart(errores~ ., 
                       data = train_semma ,
                       minbucket=1020, # (complejidad) número de observaciones mínimas en cada nodo final.BAJO = sobreajuste ; ALTO = error 
                       cp=0, # segundo término de complejidad con el que no procedemos pues no es común. Fijamos a 0. 
                       method = "anova", # criterio de división. Con “annova” usamos F de Snedecor, prioriza la variable que hace variar más la media de la variable dependiente.
                       maxsurrogate=0 # si no hay muchos missings, a 0 enseña solo las variables que participan
      )
      # Ilustramos el árbol 
      rpart.plot(arbol_4,
                 extra=1,
                 tweak=1.1,
                 nn=TRUE
      )
      asRules(arbol_4)
      # variables_arbol_4<-c("house_effect_e", "est_surv_vote", "prom_casa_partido")
      # Ilustramos Importancia de las variables
      imp_arbol_4 <-arbol_4$variable.importance
      imp_data_arbol_4 <- as.data.frame(imp_arbol_4)
      dput(rownames(imp_data_arbol_4)) #lista de variables a insertar en vc_rep
      
      imp_data <- tibble::rownames_to_column(imp_data_arbol_4, "Variables") %>% arrange(desc(imp_arbol_4))
      imp_data %>%
        mutate(Variables = fct_reorder(as.factor(Variables), imp_arbol_4)) %>%
        ggplot( aes(x=Variables, y=imp_arbol_4)) +
        theme(text = element_text(family = "ban", size=10))+
        geom_bar(stat="identity", fill="#3D74CF", alpha=.6, width=.4)+
        coord_flip()
      
      # COMPARATIVA DE ÁRBOLES POR VALIDACIÓN CRUZADA REPETIDA
      source("cruzada arbol continua.R") 
      medias_arbol_4<-cruzadaarbol(data= train_semma,
                                   vardep="errores",
                                   listconti=c("house_effect_e", "est_surv_vote", "prom_casa_partido"), # dput(rownames(imp_data_arbol_4))
                                   listclass=c(""),
                                   grupos=4, # validamos sobre 1/4 de nuestras observaciones
                                   sinicio=1234,
                                   repe=10, # Reiteramos las predicciones de nuestro modelo 10 veces. Sube variabilidad
                                   cp=0,
                                   minbucket=1020) # parámetro que identifica el modelo
      medias_arbol_4$modelo="arbol_4"
      saveRDS(medias_arbol_4, "arbol_4")
      medias_arbol_4 <- readRDS("arbol_4")
      
      prediccion <- predict(arbol_4 , newdata = test_semma)
      test_mae <- mean(abs(prediccion - test_semma$errores))
      test_rmse <- sqrt(mean((prediccion - test_semma$errores)^2))
      paste("Error de test (mae) del árbol:", round(test_mae,2))
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
      # write_csv(eval_test_arbol, file = "./EXPORTADO/eval_test_arbol.csv")
      
      # gráfico de error real y error del modelo
      ggplot(data = eval_test_arbol, 
             mapping = aes(x = prediccion, y = errores)) +
        geom_point(color = "#006EA1", alpha = 0.5, size = 4) +
        # Diagonal
        geom_abline(intercept = 0, slope = 1,
                    color = "orange", size = 1.5) +
        theme_grey()+
        labs(title = "Resultados del arbol en test: MAE = 1.11",
             subtitle = 
               "Los valores deberían estar cercanos a la diagonal. Modelo: Árbol minbucket = 36 (con poda)" ,
             x = "Predicciones",
             y = "Valores Reales")
      
    # arbol_5 ======================================================================
      # ILUSTRACIÓN DEL MEJOR ÁRBOL E IMPORTANCIA DE LAS VARIABLES
      arbol_5 <- rpart(errores~ ., 
                       data = train_semma ,
                       minbucket=150, # (complejidad) número de observaciones mínimas en cada nodo final.BAJO = sobreajuste ; ALTO = error 
                       cp=0, # segundo término de complejidad con el que no procedemos pues no es común. Fijamos a 0. 
                       method = "anova", # criterio de división. Con “annova” usamos F de Snedecor, prioriza la variable que hace variar más la media de la variable dependiente.
                       maxsurrogate=0 # si no hay muchos missings, a 0 enseña solo las variables que participan
      )
      # Ilustramos el árbol
      par(cex=1)
      rpart.plot(arbol_5,
                 extra=1,
                 tweak=1.1,
                 nn=TRUE
      )
      asRules(arbol_5)
      
      # Ilustramos Importancia de las variables
      imp_arbol_5 <-arbol_5$variable.importance
      imp_data_arbol_5 <- as.data.frame(imp_arbol_5)
      dput(rownames(imp_data_arbol_5)) #lista de variables a insertar en vc_rep
      
      imp_data <- tibble::rownames_to_column(imp_data_arbol_5, "Variables") %>% arrange(desc(imp_arbol_5))
      imp_data %>%
        mutate(Variables = fct_reorder(as.factor(Variables), imp_arbol_5)) %>%
        ggplot( aes(x=Variables, y=imp_arbol_5)) +
        theme(text = element_text(family = "ban", size=15))+
        geom_bar(stat="identity", fill="#3D74CF", alpha=.6, width=.4)+
        theme_grey()+
        coord_flip()
      
      # COMPARATIVA DE ÁRBOLES POR VALIDACIÓN CRUZADA REPETIDA
      source("cruzada arbol continua.R") 
      medias_arbol_5<-cruzadaarbol(data= train_semma,
                                   vardep="errores",
                                   listconti=c("house_effect_e", "est_surv_vote", "prom_general_partido", 
                                               "prom_carrera_partido", "days_to_elec", "party_UP", "party_VOX", 
                                               "gov_exp_edu_percap", "env_gwh_consum", "prom_casa_partido", 
                                               "prom_carrera_casa_wing", "prom_general_wing", "prom_casa_wing"), # dput(rownames(imp_data_arbol_5))
                                   listclass=c(""),
                                   grupos=4, # validamos sobre 1/4 de nuestras observaciones
                                   sinicio=1234,
                                   repe=10, # Reiteramos las predicciones de nuestro modelo 10 veces. Sube variabilidad
                                   cp=0,
                                   minbucket=150) # parámetro que identifica el modelo
      medias_arbol_5$modelo="arbol_5"
      saveRDS(medias_arbol_5, "arbol_5")
      medias_arbol_5 <- readRDS("arbol_5")
      
      prediccion <- predict(arbol_5 , newdata = test_semma)
      test_mae <- mean(abs(prediccion - test_semma$errores))
      test_rmse <- sqrt(mean((prediccion - test_semma$errores)^2))
      paste("Error de test (mae) del árbol:", round(test_mae,2))
      
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
      # write_csv(eval_test_arbol, file = "./EXPORTADO/eval_test_arbol.csv")
      
      # gráfico de error real y error del modelo
      ggplot(data = eval_test_arbol, 
             mapping = aes(x = prediccion, y = errores)) +
        geom_point(color = "#006EA1", alpha = 0.5, size = 4) +
        # Diagonal
        geom_abline(intercept = 0, slope = 1,
                    color = "orange", size = 1.5) +
        theme_grey()+
        labs(title = "Resultados del arbol en test: MAE = 1.11",
             subtitle = 
               "Los valores deberían estar cercanos a la diagonal. Modelo: Árbol minbucket = 36 (con poda)" ,
             x = "Predicciones",
             y = "Valores Reales")
  # EVALUAMOS EL MEJOR ARBOL =====================================================
  # Evaluamos mejor árbol
      medias_arbol_1 <- readRDS("arbol_1") #minbucket = 70
      medias_arbol_2 <- readRDS("arbol_2") #minbucket = 199 cp 0,00012
      medias_arbol_3 <- readRDS("arbol_3") #minbucket = 36 cp 0,00012
      medias_arbol_4 <- readRDS("arbol_4") #minbucket = 1020
      medias_arbol_5 <- readRDS("arbol_5") #minbucket = 150
      
 union<-rbind(medias_arbol_1, medias_arbol_2, medias_arbol_3, medias_arbol_4, medias_arbol_5 )
  par(cex.axis=1)
  ggplot(union, aes(x=modelo, y=error, fill=modelo)) +
    geom_boxplot(outlier.colour="black", outlier.shape=1,
                 outlier.size=2) +
    theme_grey()+
    labs(x = "Modelos de árbol", y = 'MAE', title = "Boxplot vc repetida árboles") 
  #Las observaciones fuera del boxplot (circulos) son errores outlier, o mejor dicho, errores muy anómalos para el modelo.

  # PREDICCIONES EN TEST =========================================================
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
    
  # CONCLUSIÓN SOBRE HISTÓRICO, MEJOR ARBOL ====================================
    eval_test_arbol_party <- eval_test_arbol %>% #reagrupar partits y casas 
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
      mutate(lead_party =
               case_when(str_detect(lead_party_CS, "1") ~ "CS",
                         str_detect(lead_party_PODEMOS, "1") ~ "PODEMOS",
                         str_detect(lead_party_PP, "1") ~"PP",
                         str_detect(lead_party_PSOE, "1") ~"PSOE",
                         str_detect(lead_party_UCD, "1") ~"UCD",
                         TRUE ~ "OTRAS")) %>% 
      mutate(lead2_party =
               case_when(str_detect(lead2_party_AP, "1") ~ "AP",
                         str_detect(lead2_party_ARM, "1") ~ "ARM",
                         str_detect(lead2_party_CS, "1") ~"CS",
                         str_detect(lead2_party_EA, "1") ~"EA",
                         str_detect(lead2_party_PODEMOS, "1") ~"PODEMOS",
                         str_detect(lead2_party_PP, "1") ~ "PP",
                         str_detect(lead2_party_PSOE, "1") ~ "PSOE",
                         str_detect(lead2_party_UCD, "1") ~"UCD",
                         str_detect(lead2_party_UP, "1") ~"UP",
                         str_detect(lead2_party_VOX, "1") ~"VOX",
                         TRUE ~ "OTRAS")) %>% 
      mutate(gov_pre =
               case_when(str_detect(gov_pre_PP, "1") ~ "PP",
                         str_detect(gov_pre_PSOE, "1") ~ "PSOE",
                         str_detect(gov_pre_UCD, "1") ~"UCD",
                         TRUE ~ "OTRAS"))
    
  
    eval_test_arbol_party2 <- eval_test_arbol_party %>% select(
                                 "year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                 "n",  "est_surv_vote", "prom_general_partido", "prom_general_wing", 
                                 "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido", 
                                 "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing", 
                                 "house_effect_e", "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                 "urna_60", "urna_365", 
                                 "errores", "party", "poll_firm", 
                                 "lead_party", "lead2_party", "gov_pre", "error", "real_vote", "est_real_vote"
                                 ) 
    
   semma_id <-
      semma %>% 
      mutate(id_fin =
               glue("{year_elec}_{n_days_field}_{days_to_elec}_{n}_{party}_{poll_firm}")) 
    semma_id <-
      semma_id %>% 
      mutate(id_fin = as.character(id_fin))
    
    eval_test_arbol_party2 <-
      eval_test_arbol_party2 %>% 
      mutate(id_fin =
               glue("{year_elec}_{n_days_field}_{days_to_elec}_{n}_{party}_{poll_firm}")) 
    
    eval_test_arbol_party2 <-
      eval_test_arbol_party2 %>% 
      mutate(id_fin = as.character(id_fin))
    
    eval_test_arbol_party3 <- sqldf('
      SELECT a.* 
           , b.wing
           , b.date_elec
           , b.id_semma
      FROM eval_test_arbol_party2  AS a
      LEFT JOIN (
                SELECT *
                FROM semma_id ) AS b
            ON   (a.id_fin = b.id_fin)
            ')
    
    eval_test_arbol_party3 <-
      eval_test_arbol_party3 %>% 
      mutate(wing =
               case_when(str_detect(party, "VOX")|                                                                            
                           str_detect(party, "UCD")|
                           str_detect(party, "FN")|
                           str_detect(party, "CIU")|
                           str_detect(party, "CDS")|
                           str_detect(party, "CC")|
                           str_detect(party, "AP")|
                           str_detect(party, "cs")|
                           str_detect(party, "PP") ~ "RIGHT",
                         TRUE ~ "LEFT"))
    
    eval_test_arbol_party3 <-
      eval_test_arbol_party3 %>% 
      mutate(date_elec =
               case_when(str_detect(year_elec, "1982") ~ "1982-10-28",                                                                        
                         str_detect(year_elec, "1986") ~ "1986-06-22",
                         str_detect(year_elec, "1989") ~ "1989-10-29",
                         str_detect(year_elec, "1993") ~ "1993-06-06",
                         str_detect(year_elec, "1996") ~ "1996-03-03",
                         str_detect(year_elec, "2000") ~ "2000-03-12",
                         str_detect(year_elec, "2004") ~ "2004-03-14",
                         str_detect(year_elec, "2008") ~ "2008-03-09",
                         str_detect(year_elec, "2011") ~ "2011-11-20",
                         str_detect(year_elec, "2015") ~ "2015-12-20",
                         str_detect(year_elec, "2016") ~ "2016-06-26",
                         str_detect(date_elec, "2019-04-28 02:00:00") ~ "2019-04-28",
                         str_detect(date_elec, "2019-11-10 01:00:00") ~ "2019-11-10",
                         TRUE ~ "NA"))
    
    # MEDIA POR PARTY Y CARRERA: ¿Medias de las predicciones = predicción del voto real?
    eval_test_arbol_party <- group_by(eval_test_arbol_party3, date_elec, party) 
    eval_test_arbol_party <- summarise(eval_test_arbol_party, prediccion_de_partido = mean(est_real_vote, na.rm = TRUE))
    eval_test_arbol_party <- eval_test_arbol_party %>% filter(!(date_elec == "NA"),)
    # Falta añadir con un join el valor real para hacer la comparativa. 
    eval_test_arbol_party
    semma_dos <- semma %>% mutate(date_elec2 = as.character(date_elec))
    
    eval_test_arbol_party <- sqldf('
      SELECT a.* 
           , b.real_vote
           , b.prom_carrera_partido
      FROM eval_test_arbol_party  AS a
      LEFT JOIN (
                SELECT *
                FROM semma_dos ) AS b
            ON (a.date_elec = b.date_elec2)
            AND (a.party = b.party)
            ')
    eval_test_arbol_party<-eval_test_arbol_party[!duplicated(eval_test_arbol_party), ]
    eval_test_arbol_party$prediccion_de_partido <- round(eval_test_arbol_party$prediccion_de_partido ,digit=2) # Round off the column for 2 decimal
    
    # install.packages("CGPfunctions")
    library(CGPfunctions)
    newggslopegraph(eval_test_arbol_party, date_elec, prediccion_de_partido, party,
                    Title = "Evolución del PIB",
                    SubTitle = "1982-2019",
                    Caption =  "Autor: Enric Palau Payeras | Datos: Spanish elections dataset") +
      theme_gray() +
      theme(legend.position = "none")

    carreras <- split(eval_test_arbol_party, eval_test_arbol_party$date_elec)
    eval_test_arbol_party_2019_11 <-carreras[["2019-11-10"]]
    
    a<-ggplot(eval_test_arbol_party_2019_11) +
      geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                       y = party, yend = party)) +
      geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
      geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
      theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2019-11-10)")+
      theme(legend.position = "bottom")
    
    eval_test_arbol_party_2019_04 <-carreras[["2019-04-28"]]
    
    b<-ggplot(eval_test_arbol_party_2019_04) +
      geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                       y = party, yend = party)) +
      geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
      geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
      theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2019-04-28)")+
      theme(legend.position = "bottom")
    
    eval_test_arbol_party_2016 <-carreras[["2016-06-26"]]
    
    c<-ggplot(eval_test_arbol_party_2016) +
      geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                       y = party, yend = party)) +
      geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
      geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
      theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2016-06-26)")+
      theme(legend.position = "bottom")
    
    eval_test_arbol_party_2015 <-carreras[["2015-12-20"]]
    
    d<-ggplot(eval_test_arbol_party_2015) +
      geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                       y = party, yend = party)) +
      geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
      geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
      theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2015-12-20)")+
      theme(legend.position = "bottom")
    
    comparativa_test_arbol <- ggarrange(a, b, c, d,
                                 ncol = 2, nrow = 2)
    comparativa_test_arbol
    
    
  # PREDICCIONES EN TEST 2023 =========================================================
    # Predecimos en test con el modelo seleccionado
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
      

  
  
#### 1.1.2. Bagging y RF: ####
    rf_statistics <- data.frame(samplesize = integer(),
                                nodesize   = integer(),
                                mtry = integer(),
                                RMSE = double(),
                                Rsquared = double(),
                                MAE = double())
    
    
    for (s_size in seq(from=300, to=trunc(nrow(data)*3/4), by=100))# SAMPSIZE; (k-1)*n dónde k=número de grupos
    {
      for (n_size in seq(from=round(s_size*0.01), to=round(s_size*0.1), by=10))#nodos finales max
      {
        for (n_mtry in c(129))#nºvariables si no lo tocas es bagging, si es inferior es RF
        {
          set.seed(1234)
          print("----------")
          print(n_size) #3
          print(s_size) #300
          print(n_mtry) #14
          cat("/n")
          rfgrid<-expand.grid(mtry=c(n_mtry))
          
          set.seed(1234)
          control<-trainControl(method = "cv",number=4,savePredictions = "all")
          
          bucle_rf<- train(data = data,
                           errores ~.,
                           method = "rf",
                           trControl = control,
                           tuneGrid=rfgrid,
                           linout = FALSE,
                           sample_size=s_size,
                           ntree=1000,#iteraciones
                           nodesize=n_size,
                           replace=TRUE)
          
          RMSE = bucle_rf[["results"]][["RMSE"]]
          Rsquared = bucle_rf[["results"]][["Rsquared"]]
          MAE = bucle_rf[["results"]][["MAE"]]
          #Insertamos en Dataframe
          rf_statistics[nrow(rf_statistics) + 1,] <- c(s_size,n_size,n_mtry,RMSE,Rsquared, MAE)
        }
      }
    }
    write.csv(rf_statistics, "rf_statistics.csv")
    b1_rf_statistics<-read.csv("rf_statistics.csv")
    b1_rf_statistics$X<-NULL
    completo<-b1_rf_statistics[order(b1_rf_statistics$MAE),]
    ggplot(completo, aes(x=samplesize, y=MAE , 
                         color=nodesize, pch=factor(mtry))) +
      geom_point(position=position_dodge(width=0.5),size=3) +
      theme_grey()
    
    # bag_arbol_1:  ======================================================================
    # HIPERPARÁMETROS y AJUSTE: NTREE
    library(randomForest)
    set.seed(1234)
    bag_arbol_1<-randomForest(errores~., 
                              data = train_semma, 
                              mtry=129, #nº predictoras (133 con dummy 74 sin)
                              ntree=1000, #iteraciones nº de árboles
                              sampsize=5510,#(1-1/4)*7348=5510
                              nodesize=70,#nodos finales max; minbucket del arbol
                              replace=TRUE)
    
    # OUT OF BAG  (AJUSTE: NTREE)
    plot(bag_arbol_1$mse) # 250 o casi 400
    
    bag_arbol_1_OOB<-randomForest(errores~., 
                                  data = train_semma, 
                                  mtry=129, #nº predictoras (133 con dummy 74 sin)
                                  ntree=120, #iteraciones nº de árboles
                                  sampsize=5510,#(1-1/4)*7348=5510
                                  nodesize=70,#nodos finales max; minbucket del arbol
                                  replace=TRUE)
    plot(bag_arbol_1_OOB$mse)
    
    # IMPORTANCIA DE VARIABLES
    imp <- as.data.frame(bag_arbol_1_OOB[["importance"]])
    dput(rownames(imp))
    imp_data <- tibble::rownames_to_column(imp, "Variables") %>% arrange(desc(IncNodePurity))
    
    library(forcats)
    bag_arbol_1_OOB_plot<-ggplot(data = imp_data, aes(x = reorder(Variables, IncNodePurity),
                                                      y = IncNodePurity,
                                                      fill = IncNodePurity)) +
      labs(x = "variable", title = "Reducción del IncNodePurity") +
      geom_col() +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "bottom")
    bag_arbol_1_OOB_plot 
    # OUT OF BAG  (AJUSTE: NTREE)
    plot(bag_arbol_1_OOB$mse) # 215 o casi 400
    # VALIDACIÓN CRUZADA REPETIDA
    detach(package:plyr)
    source("cruzada rf continua.R")
    
    medias_bag_arbol_1<-cruzadarf(data = train_semma, 
                                  vardep="errores",
                                  listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                              "n",  "est_surv_vote", "prom_general_partido", "prom_general_wing", 
                                              "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido", 
                                              "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing", 
                                              "house_effect_e", "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                              "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                              "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                              "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                              "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                              "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                              "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                              "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                              "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                              "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                              "party_AP", "party_BNG", "party_CC", "party_CCC", "party_CDS", 
                                              "party_CIU", "party_CS", "party_CUP", "party_EA", "party_EE", 
                                              "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", "party_IU", 
                                              "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", "party_PP", 
                                              "party_PSOE", "party_UCD", "party_UP", "party_UPYD", "party_VOX", 
                                              "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                              "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                              "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                              "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                              "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                              "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                              "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                              "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                              "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                              "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                              "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                              "gov_pre_UCD"), 
                                  listclass=c(""), 
                                  grupos=4, 
                                  sinicio=1234,
                                  repe=10, 
                                  nodesize=70, #Inspirado en el árbol
                                  replace=TRUE,
                                  ntree=120, #Parametrizado con el OOB
                                  mtry=129) #predictoras
    medias_bag_arbol_1$modelo="bag_arbol_1"
    saveRDS(medias_bag_arbol_1, "bag_arbol_1")
    medias_bag_arbol_1 <- readRDS("bag_arbol_1")
    
    medias_bag_arbol_1_OBB<-cruzadarf(data = train_semma, 
                                      vardep="errores",
                                      listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                                  "n",  "est_surv_vote", "prom_general_partido", "prom_general_wing", 
                                                  "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido", 
                                                  "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing", 
                                                  "house_effect_e", "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                                  "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                                  "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                                  "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                                  "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                                  "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                                  "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                                  "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                                  "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                                  "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                                  "party_AP", "party_BNG", "party_CC", "party_CCC", "party_CDS", 
                                                  "party_CIU", "party_CS", "party_CUP", "party_EA", "party_EE", 
                                                  "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", "party_IU", 
                                                  "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", "party_PP", 
                                                  "party_PSOE", "party_UCD", "party_UP", "party_UPYD", "party_VOX", 
                                                  "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                                  "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                                  "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                                  "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                                  "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                                  "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                                  "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                                  "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                                  "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                                  "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                                  "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                                  "gov_pre_UCD"), 
                                      listclass=c(""), 
                                      grupos=4, 
                                      sinicio=1234,
                                      repe=10, 
                                      nodesize=70, #Inspirado en el árbol
                                      replace=TRUE,
                                      ntree=77, #Parametrizado con el OOB
                                      mtry=129) #predictoras
    medias_bag_arbol_1_OBB$modelo="bag_arbol_1_OBB"
    saveRDS(medias_bag_arbol_1_OBB, "bag_arbol_1_OBB")
    medias_bag_arbol_1_OBB <- readRDS("bag_arbol_1_OBB")
    
     # rf_bag_arbol_1:  ======================================================================
    library(randomForest)
    # HIPERPARÁMETROS,DEL BUCLE
    set.seed(1234)
    rfgrid<-expand.grid(mtry=c(20: 129)) #Inferior al de bagging para que sea rf
    # DEFINICIÓN DEL ENTRENAMIENTO
    control<-trainControl(method = "cv",number=4, savePredictions = "all")  
    # AJUSTE DEL MODELO
    set.seed(1234)
    rf_bag_arbol_1<- train(errores~.,
                           data = train_semma,
                           method="rf",
                           trControl=control,
                           tuneGrid=rfgrid, # A encontrar
                           linout = FALSE,
                           ntree= 120, #iteraciones => OOB
                           nodesize= 70,#nodos finales max; minbucket del árbol
                           sampsize= 5510,
                           replace=TRUE,
                           importance=TRUE)
    rf_bag_arbol_1
    # BOOTSTRAPPING = mtry
    plot(rf_bag_arbol_1) 
    
    # library(plyr)
    source("cruzada rf continua.R")
    medias_rf_bag_arbol_1<-cruzadarf(data = train_semma,
                                     vardep="errores",
                                      listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                              "n",  "est_surv_vote", "prom_general_partido", "prom_general_wing", 
                                              "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido", 
                                              "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing", 
                                              "house_effect_e", "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                              "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                              "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                              "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                              "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                              "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                              "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                              "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                              "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                              "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                              "party_AP", "party_BNG", "party_CC", "party_CCC", "party_CDS", 
                                              "party_CIU", "party_CS", "party_CUP", "party_EA", "party_EE", 
                                              "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", "party_IU", 
                                              "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", "party_PP", 
                                              "party_PSOE", "party_UCD", "party_UP", "party_UPYD", "party_VOX", 
                                              "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                              "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                              "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                              "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                              "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                              "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                              "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                              "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                              "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                              "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                              "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                              "gov_pre_UCD"),
                                     listclass= c(""),
                                     grupos = 4,
                                     sinicio = 1234,
                                     repe = 10,
                                     replace = TRUE,
                                     ntree= 120,
                                     sampsize=2100,
                                     nodesize= 21,
                                     mtry = 129)
    medias_rf_bag_arbol_1$modelo="rf_bag_arbol_1"
    saveRDS(medias_rf_bag_arbol_1, "rf_bag_arbol_1")
    medias_rf_bag_arbol_1 <- readRDS("rf_bag_arbol_1")
    
    
    medias_rf_bag_arbol_1_btp<-cruzadarf(data = train_semma,
                                         vardep="errores",
                                         listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                                     "n",  "est_surv_vote", "prom_general_partido", "prom_general_wing", 
                                                     "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido", 
                                                     "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing", 
                                                     "house_effect_e", "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                                     "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                                     "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                                     "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                                     "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                                     "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                                     "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                                     "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                                     "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                                     "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                                     "party_AP", "party_BNG", "party_CC", "party_CCC", "party_CDS", 
                                                     "party_CIU", "party_CS", "party_CUP", "party_EA", "party_EE", 
                                                     "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", "party_IU", 
                                                     "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", "party_PP", 
                                                     "party_PSOE", "party_UCD", "party_UP", "party_UPYD", "party_VOX", 
                                                     "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                                     "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                                     "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                                     "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                                     "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                                     "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                                     "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                                     "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                                     "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                                     "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                                     "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                                     "gov_pre_UCD"),
                                         listclass= c(""),
                                         grupos = 4,
                                         sinicio = 1234,
                                         repe = 10,
                                         replace = TRUE,
                                         ntree= 600,
                                         sampsize= 5510,
                                         nodesize= 70, 
                                         mtry = 90)
    medias_rf_bag_arbol_1_btp$modelo="rf_bag_arbol_1_btp"
    saveRDS(medias_rf_bag_arbol_1_btp, "rf_bag_arbol_1_btp")
    medias_rf_bag_arbol_1_btp <- readRDS("rf_bag_arbol_1_btp")
  
    

  
    
    
    
    
    # bag_arbol_2//rf_bag_arbol_2:  ======================================================================
    # (3/4*nfilas)
    rf_statistics <- data.frame(samplesize = integer(),
                                nodesize   = integer(),
                                mtry = integer(),
                                RMSE = double(),
                                Rsquared = double(),
                                MAE = double())
    
    
    for (s_size in seq(from=300, to=trunc(nrow(data)*3/4), by=100))# SAMPSIZE; (k-1)*n dónde k=número de grupos
    {
      for (n_size in seq(from=round(s_size*0.01), to=round(s_size*0.1), by=10))#nodos finales max
      {
        for (n_mtry in c(25,50,77,125))#nºvariables
        {
          set.seed(1234)
          print("----------")
          print(n_size) #3
          print(s_size) #300
          print(n_mtry) #14
          cat("/n")
          rfgrid<-expand.grid(mtry=c(n_mtry))
          
          set.seed(1234)
          control<-trainControl(method = "cv",number=4,savePredictions = "all")
          
          bucle_rf<- train(data = data,
                           errores ~.,
                           method = "rf",
                           trControl = control,
                           tuneGrid=rfgrid,
                           linout = FALSE,
                           sample_size=s_size,
                           ntree=1000,#iteraciones
                           nodesize=n_size,
                           replace=TRUE)
          
          RMSE = bucle_rf[["results"]][["RMSE"]]
          Rsquared = bucle_rf[["results"]][["Rsquared"]]
          MAE = bucle_rf[["results"]][["MAE"]]
          #Insertamos en Dataframe
          rf_statistics[nrow(rf_statistics) + 1,] <- c(s_size,n_size,n_mtry,RMSE,Rsquared, MAE)
        }
      }
    }
    write.csv(rf_statistics, "rf_statistics.csv")
    b1_rf_statistics<-read.csv("rf_statistics.csv")
    b1_rf_statistics$X<-NULL
    
    # saveRDS(completo, "completo")
    completo<-b1_rf_statistics[order(b1_rf_statistics$MAE),]
    ggplot(completo, aes(x=samplesize, y=MAE , 
                         color=nodesize, pch=factor(mtry))) +
      geom_point(position=position_dodge(width=0.5),size=3) +
      theme_grey()
    #mtry = 77 # samplesize = menor 1000 # nodesize = 100
    
    # HIPERPARÁMETROS y AJUSTE: NTREE
        library(randomForest)
        set.seed(1234)
        bag_arbol_2<-randomForest(errores~., 
                            data = train_semma, 
                            mtry=130, #nº predictoras (123 con dummy 63 sin)
                            ntree=1000, #iteraciones
                            sampsize=5510,#(1-1/4)*7348=5510
                            nodesize= 100,#nodos finales max; minbucket del arbol
                            replace=TRUE)
        
        # OUT OF BAG  (AJUSTE: NTREE)
        plot(bag_arbol_2$mse) #170 
        
        
        bag_arbol_2_OOB<-randomForest(errores~., 
                                  data = train_semma, 
                                  mtry=130, #nº predictoras (123 con dummy 63 sin)
                                  ntree=300, #iteraciones
                                  sampsize= 5510,#(1-1/4)*7348=5510
                                  nodesize= 100,#nodos finales max; minbucket del arbol
                                  replace=TRUE)
        plot(bag_arbol_2_OOB$mse)#150 o 230 o 40
        
        # IMPORTANCIA DE VARIABLES
        imp <- as.data.frame(bag_arbol_2_OOB[["importance"]])
        dput(rownames(imp))
        imp_data <- tibble::rownames_to_column(imp, "Variables") %>% arrange(desc(IncNodePurity))
        
        library(forcats)
        bag_arbol_2_OOB_plot<-ggplot(data = imp_data, aes(x = reorder(Variables, IncNodePurity),
                                                      y = IncNodePurity,
                                                      fill = IncNodePurity)) +
          labs(x = "variable", title = "Reducción del IncNodePurity") +
          geom_col() +
          coord_flip() +
          theme_bw() +
          theme(legend.position = "bottom")
        bag_arbol_2_OOB_plot 
        
        
        # VALIDACIÓN CRUZADA REPETIDA
        detach(package:plyr)
        source("cruzada rf continua.R")
        dput(names(data_lineal))
        medias_bag_arbol_2<-cruzadarf(data= train_semma, 
                                      vardep="errores",
                                      listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                                  "n",  "est_surv_vote", "prom_general_partido", "prom_general_wing", 
                                                  "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido", 
                                                  "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing", 
                                                  "house_effect_e", "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                                  "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                                  "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                                  "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                                  "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                                  "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                                  "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                                  "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                                  "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                                  "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                                  "party_AP", "party_BNG", "party_CC", "party_CCC", "party_CDS", 
                                                  "party_CIU", "party_CS", "party_CUP", "party_EA", "party_EE", 
                                                  "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", "party_IU", 
                                                  "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", "party_PP", 
                                                  "party_PSOE", "party_UCD", "party_UP", "party_UPYD", "party_VOX", 
                                                  "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                                  "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                                  "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                                  "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                                  "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                                  "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                                  "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                                  "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                                  "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                                  "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                                  "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                                  "gov_pre_UCD"), 
                                      listclass=c(""), 
                                      grupos=4, 
                                      sinicio=1234,
                                      repe=10, 
                                      nodesize=2, #Inspirado en el árbol
                                      replace=TRUE,
                                      ntree=150, #Parametrizado con el OOB
                                      mtry=122) #predictoras
        medias_bag_arbol_2$modelo="bag_arbol_2"
        saveRDS(medias_bag_arbol_2, "bag_arbol_2")
        medias_bag_arbol_2 <- readRDS("bag_arbol_2")
        
        medias_bag_arbol_2_OOB<-cruzadarf(data = train_semma, 
                                      vardep="errores",
                                      listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                                  "n",  "est_surv_vote", "prom_general_partido", "prom_general_wing", 
                                                  "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido", 
                                                  "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing", 
                                                  "house_effect_e", "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                                  "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                                  "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                                  "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                                  "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                                  "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                                  "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                                  "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                                  "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                                  "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                                  "party_AP", "party_BNG", "party_CC", "party_CCC", "party_CDS", 
                                                  "party_CIU", "party_CS", "party_CUP", "party_EA", "party_EE", 
                                                  "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", "party_IU", 
                                                  "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", "party_PP", 
                                                  "party_PSOE", "party_UCD", "party_UP", "party_UPYD", "party_VOX", 
                                                  "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                                  "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                                  "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                                  "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                                  "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                                  "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                                  "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                                  "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                                  "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                                  "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                                  "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                                  "gov_pre_UCD"), 
                                      listclass=c(""), 
                                      grupos=4, 
                                      sinicio=1234,
                                      repe=10, 
                                      nodesize=2, #Inspirado en el árbol
                                      replace=TRUE,
                                      ntree=70, #Parametrizado con el OOB
                                      mtry=122) #predictoras
        medias_bag_arbol_2_OOB$modelo="bag_arbol_2_OOB"
        saveRDS(medias_bag_arbol_2_OOB, "bag_arbol_2_OOB")
        medias_bag_arbol_2_OOB <- readRDS("bag_arbol_2_OOB")
        # library(plyr)
        source("cruzada rf continua.R")
        medias_rf_bag_arbol_2<-cruzadarf(data = train_semma,
                                         vardep="errores",
                                         listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                                     "n",  "est_surv_vote", "prom_general_partido", "error_general_partido", 
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
                                                     "party_AP", "party_BNG", "party_CC", "party_CCC", 
                                                     "party_CDS", "party_CIU", "party_CS", "party_CUP", "party_EA", 
                                                     "party_EE", "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", 
                                                     "party_IU", "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", 
                                                     "party_PP", "party_PSOE", "party_UCD", "party_UP", "party_UPYD", 
                                                     "party_VOX", "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                                     "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                                     "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                                     "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                                     "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                                     "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                                     "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                                     "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                                     "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                                     "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                                     "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                                     "gov_pre_UCD"),
                                         listclass= c(""),
                                         grupos = 4,
                                         sinicio = 1234,
                                         repe = 10,
                                         replace = TRUE,
                                         ntree= 600,
                                         sampsize=2100,
                                         nodesize= 21,
                                         mtry = 129)
        medias_rf_bag_arbol_2$modelo="rf_bag_arbol_2"
        saveRDS(medias_rf_bag_arbol_2, "rf_bag_arbol_2")
        medias_rf_bag_arbol_2 <- readRDS("rf_bag_arbol_2")
        
        
        medias_rf_bag_arbol_2_btp<-cruzadarf(data = train_semma,
                                             vardep="errores",
                                             listconti = c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                                           "n",  "est_surv_vote", "prom_general_partido", "error_general_partido", 
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
                                                           "party_AP", "party_BNG", "party_CC", "party_CCC", 
                                                           "party_CDS", "party_CIU", "party_CS", "party_CUP", "party_EA", 
                                                           "party_EE", "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", 
                                                           "party_IU", "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", 
                                                           "party_PP", "party_PSOE", "party_UCD", "party_UP", "party_UPYD", 
                                                           "party_VOX", "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                                           "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                                           "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                                           "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                                           "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                                           "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                                           "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                                           "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                                           "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                                           "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                                           "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                                           "gov_pre_UCD"),
                                             listclass= c(""),
                                             grupos = 4,
                                             sinicio = 1234,
                                             repe = 10,
                                             replace = TRUE,
                                             ntree= 600,
                                             # sampsize= 5510,
                                             nodesize= 70, 
                                             mtry = 90)
        medias_rf_bag_arbol_2_btp$modelo="rf_bag_arbol_2_btp"
        saveRDS(medias_rf_bag_arbol_2_btp, "rf_bag_arbol_2_btp")
        medias_rf_bag_arbol_2_btp <- readRDS("rf_bag_arbol_2_btp")
        
        
    # bag_arbol_3:  ======================================================================
        # HIPERPARÁMETROS y AJUSTE: NTREE
        library(randomForest)
        set.seed(1234)
        bag_arbol_3<-randomForest(errores~., 
                                  data = train_semma, 
                                  mtry=129, #nº predictoras (133 con dummy 74 sin)
                                  ntree=1000, #iteraciones nº de árboles
                                  sampsize= 5510, #(1-1/4)*7348=5510 entre 1280 y 5510
                                  nodesize= 40, #nodos finales max; minbucket del arbol
                                  replace=TRUE)
        
        # OUT OF BAG  (AJUSTE: NTREE)
        plot(bag_arbol_3$mse) # entre 200 y 400
        
        bag_arbol_3_OOB<-randomForest(errores~., 
                                      data = train_semma, 
                                      mtry=129, #nº predictoras (133 con dummy 74 sin)
                                      ntree=400, #iteraciones
                                      sampsize= 5510,
                                      # 5510, #(1-1/4)*7348=5510 entre 1280 y 5510
                                      nodesize=40,#nodos finales max; minbucket del arbol
                                      replace=TRUE)
        plot(bag_arbol_3_OOB$mse)
        # IMPORTANCIA DE VARIABLES
        imp <- as.data.frame(bag_arbol_3_OOB[["importance"]])
        dput(rownames(imp))
        imp_data <- tibble::rownames_to_column(imp, "Variables") %>% arrange(desc(IncNodePurity))
        
        library(forcats)
        bag_arbol_3_OOB_plot<-ggplot(data = imp_data, aes(x = reorder(Variables, IncNodePurity),
                                                          y = IncNodePurity,
                                                          fill = IncNodePurity)) +
          labs(x = "variable", title = "Reducción del IncNodePurity") +
          geom_col() +
          coord_flip() +
          theme_bw() +
          theme(legend.position = "bottom")
        bag_arbol_3_OOB_plot 
        
        # VALIDACIÓN CRUZADA REPETIDA
        detach(package:plyr)
        source("cruzada rf continua.R")
        medias_bag_arbol_3<-cruzadarf(data= train_semma, 
                                      vardep="errores",
                                      listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                                  "n",  "est_surv_vote", "prom_general_partido", "prom_general_wing", 
                                                  "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido", 
                                                  "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing", 
                                                  "house_effect_e", "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                                  "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                                  "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                                  "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                                  "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                                  "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                                  "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                                  "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                                  "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                                  "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                                  "party_AP", "party_BNG", "party_CC", "party_CCC", "party_CDS", 
                                                  "party_CIU", "party_CS", "party_CUP", "party_EA", "party_EE", 
                                                  "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", "party_IU", 
                                                  "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", "party_PP", 
                                                  "party_PSOE", "party_UCD", "party_UP", "party_UPYD", "party_VOX", 
                                                  "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                                  "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                                  "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                                  "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                                  "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                                  "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                                  "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                                  "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                                  "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                                  "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                                  "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                                  "gov_pre_UCD"), 
                                      listclass=c(""), 
                                      grupos=4, 
                                      sinicio=1234,
                                      repe=10, 
                                      nodesize=40, #Inspirado en el árbol
                                      replace=TRUE,
                                      ntree=600, #Parametrizado con el OOB
                                      mtry=129 # 27 #predictoras
                                      ) 
        medias_bag_arbol_3$modelo="bag_arbol_3"
        saveRDS(medias_bag_arbol_3, "bag_arbol_3")
        medias_bag_arbol_3 <- readRDS("bag_arbol_3")
        
        medias_bag_arbol_3_OOB<-cruzadarf(data = train_semma, 
                                          vardep="errores",
                                          listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                                      "n",  "est_surv_vote", "prom_general_partido", "prom_general_wing", 
                                                      "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido", 
                                                      "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing", 
                                                      "house_effect_e", "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                                      "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                                      "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                                      "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                                      "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                                      "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                                      "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                                      "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                                      "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                                      "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                                      "party_AP", "party_BNG", "party_CC", "party_CCC", "party_CDS", 
                                                      "party_CIU", "party_CS", "party_CUP", "party_EA", "party_EE", 
                                                      "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", "party_IU", 
                                                      "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", "party_PP", 
                                                      "party_PSOE", "party_UCD", "party_UP", "party_UPYD", "party_VOX", 
                                                      "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                                      "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                                      "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                                      "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                                      "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                                      "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                                      "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                                      "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                                      "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                                      "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                                      "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                                      "gov_pre_UCD"), 
                                          listclass=c(""), 
                                          grupos=4, 
                                          sinicio=1234,
                                          repe=10, 
                                          nodesize=40, #Inspirado en el árbol
                                          replace=TRUE,
                                          ntree=190, #Parametrizado con el OOB
                                          mtry= 129  # 27 #predictoras
                                          ) 
        medias_bag_arbol_3_OOB$modelo="bag_arbol_3_OOB"
        saveRDS(medias_bag_arbol_3_OOB, "bag_arbol_3_OOB")
        medias_bag_arbol_3_OOB <- readRDS("bag_arbol_3_OOB")
        

        
    # rf_bag_arbol_3:  =========================================================
        # HIPERPARÁMETROS y AJUSTE: NTREE
        library(randomForest)
        # HIPERPARÁMETROS,DEL BUCLE
        set.seed(1234)
        rfgrid<-expand.grid(mtry=c(30: 120)) #Inferior al de bagging para que sea rf
        # DEFINICIÓN DEL ENTRENAMIENTO
        control<-trainControl(method = "cv",number=4, savePredictions = "all")  
        # AJUSTE DEL MODELO
        set.seed(1234)
        rf_bag_arbol_3<- train(errores~.,
                               data = train_semma,
                               method="rf",
                               trControl=control,
                               tuneGrid=rfgrid, # A encontrar
                               linout = FALSE,
                               ntree= 600, #iteraciones => OOB
                               nodesize= 40,#nodos finales max; minbucket del árbol
                               sampsize= 5510,
                               replace=TRUE,
                               importance=TRUE)
        rf_bag_arbol_3
        # BOOTSTRAPPING = mtry
        plot(rf_bag_arbol_3)
        
        # library(plyr)
        source("cruzada rf continua.R")
        medias_bag_arbol_3_vars<-cruzadarf(data = train_semma, 
                                           vardep="errores",
                                           listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                                       "n",  "est_surv_vote", "prom_general_partido", "prom_general_wing", 
                                                       "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido", 
                                                       "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing", 
                                                       "house_effect_e", "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                                       "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                                       "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                                       "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                                       "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                                       "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                                       "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                                       "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                                       "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                                       "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                                       "party_AP", "party_BNG", "party_CC", "party_CCC", "party_CDS", 
                                                       "party_CIU", "party_CS", "party_CUP", "party_EA", "party_EE", 
                                                       "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", "party_IU", 
                                                       "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", "party_PP", 
                                                       "party_PSOE", "party_UCD", "party_UP", "party_UPYD", "party_VOX", 
                                                       "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                                       "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                                       "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                                       "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                                       "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                                       "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                                       "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                                       "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                                       "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                                       "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                                       "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                                       "gov_pre_UCD"), 
                                           listclass=c(""), 
                                           grupos=4, 
                                           sinicio=1234,
                                           repe=10, 
                                           nodesize=40, #Inspirado en el árbol
                                           replace=TRUE,
                                           ntree=600, #Parametrizado con el OOB
                                           mtry= 78
                                           # 27 #predictoras
        )
        medias_bag_arbol_3_vars$modelo="bag_arbol_3_vars"
        saveRDS(medias_bag_arbol_3_vars, "bag_arbol_3_vars")
        medias_bag_arbol_3_vars <- readRDS("bag_arbol_3_vars")
        medias_rf_bag_arbol_3<-cruzadarf(data = train_semma,
                                         vardep="errores",
                                         listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                                                 "n",  "est_surv_vote", "prom_general_partido", "prom_general_wing", 
                                                                 "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido", 
                                                                 "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing", 
                                                                 "house_effect_e", "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                                                 "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                                                 "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                                                 "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                                                 "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                                                 "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                                                 "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                                                 "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                                                 "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                                                 "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                                                 "party_AP", "party_BNG", "party_CC", "party_CCC", "party_CDS", 
                                                                 "party_CIU", "party_CS", "party_CUP", "party_EA", "party_EE", 
                                                                 "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", "party_IU", 
                                                                 "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", "party_PP", 
                                                                 "party_PSOE", "party_UCD", "party_UP", "party_UPYD", "party_VOX", 
                                                                 "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                                                 "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                                                 "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                                                 "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                                                 "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                                                 "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                                                 "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                                                 "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                                                 "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                                                 "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                                                 "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                                                 "gov_pre_UCD"), 
                                    
                                         listclass= c(""),
                                         grupos = 4,
                                         sinicio = 1234,
                                         repe = 10,
                                         replace = TRUE,
                                         ntree= 600,
                                         sampsize=5510,
                                         nodesize= 40,
                                         mtry = 126)
        medias_rf_bag_arbol_3$modelo="rf_bag_arbol_3"
        saveRDS(medias_rf_bag_arbol_3, "rf_bag_arbol_3")
        medias_rf_bag_arbol_3 <- readRDS("rf_bag_arbol_3")
        
        
        medias_rf_bag_arbol_3_btp<-cruzadarf(data = train_semma,
                                             vardep="errores",
                                             listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                                         "n",  "est_surv_vote", "prom_general_partido", "prom_general_wing", 
                                                         "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido", 
                                                         "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing", 
                                                         "house_effect_e", "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                                         "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                                         "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                                         "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                                         "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                                         "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                                         "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                                         "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                                         "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                                         "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                                         "party_AP", "party_BNG", "party_CC", "party_CCC", "party_CDS", 
                                                         "party_CIU", "party_CS", "party_CUP", "party_EA", "party_EE", 
                                                         "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", "party_IU", 
                                                         "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", "party_PP", 
                                                         "party_PSOE", "party_UCD", "party_UP", "party_UPYD", "party_VOX", 
                                                         "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                                         "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                                         "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                                         "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                                         "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                                         "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                                         "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                                         "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                                         "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                                         "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                                         "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                                         "gov_pre_UCD"), 
                                             listclass= c(""),
                                             grupos = 4,
                                             sinicio = 1234,
                                             repe = 10,
                                             replace = TRUE,
                                             ntree= 600,
                                             # sampsize= 5510,
                                             nodesize= 40, 
                                             mtry = 92)
        medias_rf_bag_arbol_3_btp$modelo="rf_bag_arbol_3_btp"
        saveRDS(medias_rf_bag_arbol_3_btp, "rf_bag_arbol_3_btp")
        medias_rf_bag_arbol_3_btp <- readRDS("rf_bag_arbol_3_btp")
        

        
   
        
        
  # EVALUAMOS EL MEJOR bagging =====================================================
    # Evaluamos mejor BAGGING
        medias_bag_arbol_1 <- readRDS("bag_arbol_1")
        medias_bag_arbol_1_OOB <- readRDS("bag_arbol_1_OBB")
        medias_rf_bag_arbol_1 <- readRDS("rf_bag_arbol_1")
        medias_rf_bag_arbol_1_btp <- readRDS("rf_bag_arbol_1_btp")
       
        medias_bag_arbol_3 <- readRDS("bag_arbol_3")
        medias_bag_arbol_3_OOB <- readRDS("bag_arbol_3_OBB")
        medias_rf_bag_arbol_3 <- readRDS("rf_bag_arbol_3")
        medias_rf_bag_arbol_3_btp <- readRDS("rf_bag_arbol_3_btp")
        
        
        medias_bag_arbol_2_OOB <- readRDS("bag_arbol_2_OBB")
        medias_rf_bag_arbol_2 <- readRDS("rf_bag_arbol_2")
        medias_rf_bag_arbol_2_btp <- readRDS("rf_bag_arbol_2_btp")
        
      
        medias_arbol_1 <- readRDS("arbol_1") #minbucket = 70
        medias_arbol_2 <- readRDS("arbol_2") #minbucket = 199 cp 0,00012
        medias_arbol_3 <- readRDS("arbol_3") #minbucket = 36 cp 0,00012
        medias_arbol_4 <- readRDS("arbol_4") #minbucket = 1020
        medias_arbol_5 <- readRDS("arbol_5") #minbucket = 150
        
        
    union<-rbind(medias_bag_arbol_1, medias_bag_arbol_1_OOB, medias_rf_bag_arbol_1, medias_rf_bag_arbol_1_btp,
                 medias_bag_arbol_2_OOB, medias_rf_bag_arbol_2, medias_rf_bag_arbol_2_btp,
                 medias_bag_arbol_3, medias_bag_arbol_3_OOB, medias_rf_bag_arbol_3, medias_rf_bag_arbol_3_btp,
                 medias_arbol_1, medias_arbol_3
                 )
    
    par(cex.axis=1)
    boxplot(data=union, error~modelo) 
      
    ggplot(union, aes(x=modelo, y=error, fill=modelo)) +
      geom_boxplot(outlier.colour="black", outlier.shape=1,
                   outlier.size=2) +
      labs(x = "Modelos de bagging de árboles", y = 'MAE', title = "Boxplot vc repetida bagging") 
    #Las observaciones fuera del boxplot (circulos) son errores outlier, o mejor dicho, errores muy anómalos para el modelo.  
    
  # PREDICCIONES EN TEST BAGGING =========================================================
    set.seed(1234)
    bag_ganador <- randomForest(errores~.,
                                 data= train_semma,
                                 mtry= 129, #nº predictoras (133 con dummy 73 sin) del árbol
                                 ntree= 120, #iteraciones => OOB
                                 nodesize= 70,#nodos finales max; minbucket del árbol
                                 sampsize= 5510,
                                 replace=TRUE)
    bag_ganador
    ggplot(data = train_semma,
           mapping = aes(x = errores, y= bag_ganador[["predicted"]])) +
      geom_point(color = "#111CA5", alpha = 0.5, size = 4) +
      # Diagonal
      geom_smooth(method=lm, se=FALSE, color = "orange", size = 1.5) +
      labs(title = "Bagging de Árboles",
           subtitle =
             "Error real vs Predicción de error",
           x = "Error real",
           y = "Predicción de error")
    
    # Ilustramos Importancia de las variables
    imp <- as.data.frame(bag_ganador[["importance"]])
    imp_bag <- imp %>% filter(IncNodePurity > 0.000001)
    dput(rownames(imp_bag))
    length(imp_bag)
    imp_data <- tibble::rownames_to_column(imp_bag, "Variables") %>% arrange(desc(IncNodePurity))
    
    library(forcats)
    bag_ganador_plot<-ggplot(data = imp_data %>% filter(IncNodePurity > 20), aes(x = reorder(Variables, IncNodePurity),
                                                  y = IncNodePurity,
                                                  fill = IncNodePurity)) +
      labs(x = "variable", title = "Reducción del IncNodePurity") +
      geom_col() +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "bottom")
    bag_ganador_plot
    
    
    # Predecimos en test con el modelo seleccionado
    prediccion <- predict(bag_ganador, newdata = test_semma) #hacemos las predicciones sobre test; recordemos que en test no hay dummies. 
    prediccion <- as.data.frame(prediccion)#guardamos las predicciones en test
    # saveRDS(prediccion, "prediccion_bagging")

    # Al no tener el id_semma añadimos un id por el row name que R define por defecto
    obs_test<-tibble::rowid_to_column(test_semma, "ID")
    
    # Al no tener el id_semma añadimos un id por el row name que R define por defecto
    pred_test<-tibble::rowid_to_column(prediccion, "ID")
    
    # Juntamos nuestras predicciones con el conjunto de test mediante el row name que R define por defecto
    eval_test_bag <- left_join(obs_test, pred_test, by = "ID") %>%
      mutate(error = prediccion - errores ) %>% # Error del modelo
      mutate(real_vote = est_surv_vote + errores ) %>% # Error de las encuestas (error real)
      mutate(est_real_vote = est_surv_vote + prediccion ) %>% # Estimación de voto del modelo o corrección del modelo aplicada a la encuesta
      mutate(mae_bag = mean(abs(prediccion - errores)) ) %>%
      mutate(rmse_bag =  sqrt(mean((prediccion - errores)^2)) ) %>% 
      mutate(r_cua_bag = 1 - sum(error^2)/sum((errores - mean(errores))^2)) 
    
    # gráfico de error real y error del modelo
    ggplot(data = eval_test_bag,
           mapping = aes(x = prediccion, y = errores)) +
      geom_point(color = "#CC6666", alpha = 0.5, size = 4) +
      # Diagonal
      geom_abline(intercept = 0, slope = 1,
                  color = "orange", size = 1.5) +
      labs(title = "Resultados del bag en test. MAE = 0.47",
           subtitle =
             "Valores deberían estar cercanos a la diagonal. Modelo: Bagging mtry = 83, nodesize = 40, ntree = 600",
           caption =
             "Autor: Enric Palau Payeras | Datos: Spanish electoral dataset",
           x = "Predicciones",
           y = "Valores Reales")+
      theme_grey()
    
  # CONCLUSIÓN SOBRE HISTÓRICO  ====================================
    eval_test_bag_party <- eval_test_bag %>% #reagrupar partits y casas 
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
      mutate(lead_party =
               case_when(str_detect(lead_party_CS, "1") ~ "CS",
                         str_detect(lead_party_PODEMOS, "1") ~ "PODEMOS",
                         str_detect(lead_party_PP, "1") ~"PP",
                         str_detect(lead_party_PSOE, "1") ~"PSOE",
                         str_detect(lead_party_UCD, "1") ~"UCD",
                         TRUE ~ "OTRAS")) %>% 
      mutate(lead2_party =
               case_when(str_detect(lead2_party_AP, "1") ~ "AP",
                         str_detect(lead2_party_ARM, "1") ~ "ARM",
                         str_detect(lead2_party_CS, "1") ~"CS",
                         str_detect(lead2_party_EA, "1") ~"EA",
                         str_detect(lead2_party_PODEMOS, "1") ~"PODEMOS",
                         str_detect(lead2_party_PP, "1") ~ "PP",
                         str_detect(lead2_party_PSOE, "1") ~ "PSOE",
                         str_detect(lead2_party_UCD, "1") ~"UCD",
                         str_detect(lead2_party_UP, "1") ~"UP",
                         str_detect(lead2_party_VOX, "1") ~"VOX",
                         TRUE ~ "OTRAS")) %>% 
      mutate(gov_pre =
               case_when(str_detect(gov_pre_PP, "1") ~ "PP",
                         str_detect(gov_pre_PSOE, "1") ~ "PSOE",
                         str_detect(gov_pre_UCD, "1") ~"UCD",
                         TRUE ~ "OTRAS"))
    
    
    eval_test_bag_party2 <- eval_test_bag_party %>% select("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                                               "n",  "est_surv_vote", "prom_general_partido", "prom_general_wing", 
                                                               "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido", 
                                                               "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing", 
                                                               "house_effect_e", "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                                               "urna_60", "urna_365", 
                                                               "errores", "party", "poll_firm", 
                                                               "lead_party", "lead2_party", "gov_pre", "error", "real_vote", "est_real_vote") 
    
    semma_id <-
      semma %>% 
      mutate(id_fin =
               glue("{year_elec}_{n_days_field}_{days_to_elec}_{n}_{party}_{poll_firm}")) 
    semma_id <-
      semma_id %>% 
      mutate(id_fin = as.character(id_fin))
    
    eval_test_bag_party2 <-
      eval_test_bag_party2 %>% 
      mutate(id_fin =
               glue("{year_elec}_{n_days_field}_{days_to_elec}_{n}_{party}_{poll_firm}")) 
    
    eval_test_bag_party2 <-
      eval_test_bag_party2 %>% 
      mutate(id_fin = as.character(id_fin))
    
    eval_test_bag_party3 <- sqldf('
      SELECT a.* 
           , b.wing
           , b.date_elec
           , b.id_semma
      FROM eval_test_bag_party2  AS a
      LEFT JOIN (
                SELECT *
                FROM semma_id ) AS b
            ON   (a.id_fin = b.id_fin)
            ')
    
    eval_test_bag_party3 <-
      eval_test_bag_party3 %>% 
      mutate(wing =
               case_when(str_detect(party, "VOX")|                                                                            
                           str_detect(party, "UCD")|
                           str_detect(party, "FN")|
                           str_detect(party, "CIU")|
                           str_detect(party, "CDS")|
                           str_detect(party, "CC")|
                           str_detect(party, "AP")|
                           str_detect(party, "cs")|
                           str_detect(party, "PP") ~ "RIGHT",
                         TRUE ~ "LEFT"))
    
    eval_test_bag_party3 <-
      eval_test_bag_party3 %>% 
      mutate(date_elec =
               case_when(str_detect(year_elec, "1982") ~ "1982-10-28",                                                                        
                         str_detect(year_elec, "1986") ~ "1986-06-22",
                         str_detect(year_elec, "1989") ~ "1989-10-29",
                         str_detect(year_elec, "1993") ~ "1993-06-06",
                         str_detect(year_elec, "1996") ~ "1996-03-03",
                         str_detect(year_elec, "2000") ~ "2000-03-12",
                         str_detect(year_elec, "2004") ~ "2004-03-14",
                         str_detect(year_elec, "2008") ~ "2008-03-09",
                         str_detect(year_elec, "2011") ~ "2011-11-20",
                         str_detect(year_elec, "2015") ~ "2015-12-20",
                         str_detect(year_elec, "2016") ~ "2016-06-26",
                         str_detect(date_elec, "2019-04-28 02:00:00") ~ "2019-04-28",
                         str_detect(date_elec, "2019-11-10 01:00:00") ~ "2019-11-10",
                         TRUE ~ "NA"))
    
    
    # MEDIA POR PARTY Y CARRERA: ¿Medias de las predicciones = predicción del voto real?
    eval_test_bag_party <- group_by(eval_test_bag_party3, date_elec, party) #OJO con 2019 tenemos que recuprara el date elec
    eval_test_bag_party <- summarise(eval_test_bag_party, prediccion_de_partido = mean(est_real_vote, na.rm = TRUE))
    eval_test_bag_party <- eval_test_bag_party %>% filter(!(date_elec == "NA"),)
    # Falta añadir con un join el valor real para hacer la comparativa. 
    eval_test_bag_party
    semma_dos <- semma %>% mutate(date_elec2 = as.character(date_elec))
    
    eval_test_bag_party <- sqldf('
      SELECT a.* 
           , b.real_vote
           , b.prom_carrera_partido
      FROM eval_test_bag_party  AS a
      LEFT JOIN (
                SELECT *
                FROM semma_dos ) AS b
            ON (a.date_elec = b.date_elec2)
            AND (a.party = b.party)
            ')
    eval_test_bag_party<-eval_test_bag_party[!duplicated(eval_test_bag_party), ]
   
    
    # install.packages("ggplot2")
    library(ggplot2)
    carreras <- split(eval_test_bag_party, eval_test_bag_party$date_elec)
    eval_test_bag_party_2019_11 <-carreras[["2019-11-10"]]
    
    a<-ggplot(eval_test_bag_party_2019_11) +
      geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                       y = party, yend = party)) +
      geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
      geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
      theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2019-11-10)")+
      theme(legend.position = "bottom")
    
    eval_test_bag_party_2019_04 <-carreras[["2019-04-28"]]
    
    b<-ggplot(eval_test_bag_party_2019_04) +
      geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                       y = party, yend = party)) +
      geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
      geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
      theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2019-04-28)")+
      theme(legend.position = "bottom")
    
    eval_test_bag_party_2016 <-carreras[["2016-06-26"]]
    
    c<-ggplot(eval_test_bag_party_2016) +
      geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                       y = party, yend = party)) +
      geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
      geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
      theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2016-06-26)")+
      theme(legend.position = "bottom")
    
    eval_test_bag_party_2015 <-carreras[["2015-12-20"]]
    
    d<-ggplot(eval_test_bag_party_2015) +
      geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                       y = party, yend = party)) +
      geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
      geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
      theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2015-12-20)")+
      theme(legend.position = "bottom")
    
    comparativa_test_bag <- ggarrange(a, b, c, d,
                                        ncol = 2, nrow = 2)
    comparativa_test_bag
    
    library(Metrics)
    mae(eval_test_bag_party$real_vote, eval_test_bag_party$prediccion_de_partido)
    
  # PREDICCIONES EN TEST RF =========================================================
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
  
  # CONCLUSIÓN SOBRE HISTÓRICO, MEJOR RF ====================================
  eval_test_rf_party <- eval_test_rf %>% #reagrupar partits y casas 
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
  
  
  semma_id <- semma %>% select(party, poll_firm, year_elec, n_days_field, days_to_elec, n, est_surv_vote, date_elec) #semma lo necesario
  eval_test_rf_party <- left_join(eval_test_rf_party, semma_id, by=c('party'='party', 
                                                                       'poll_firm'='poll_firm',
                                                                       "n_days_field"="n_days_field",
                                                                       "days_to_elec"="days_to_elec",
                                                                       "n"="n",
                                                                       "year_elec"="year_elec",
                                                                       "est_surv_vote"="est_surv_vote"))
  # MEDIA POR PARTY Y CARRERA: ¿Medias de las predicciones = predicción del voto real?
  eval_test_rf_party <- group_by(eval_test_rf_party, year_elec, party) #OJO con 2019 tenemos que recuprara el date elec
  eval_test_rf_party <- summarise(eval_test_rf_party, prediccion_de_partido = mean(est_real_vote, na.rm = TRUE))
  # Falta añadir con un join el valor real para hacer la comparativa. 
  eval_test_rf_party
  # PREDICCIONES EN TEST 2023 =========================================================
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
#### 1.1.4. GBM: ####
  set.seed(1234)
  # HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
  gbmgrid<-expand.grid(shrinkage=c(0.1, 0.05, 0.03, 0.01, 1), # influencia que tiene cada iteraciÃ³n (modelo). BAJO = ^ n.trees = SOBREAJUSTA
                       n.minobsinnode=c(2, 40, 70, 119, 230, 420, 600),# (minbucket arbol; nodesize rf y bagging) BAJO = SOBREAJUSTA
                       n.trees=c(50,100,300,800,1200),# (ntree bagging) nÃºmero de modelos que forman el essemble. SOBREAJUSTA SI ^
                       interaction.depth=c(2)) # nÃºmero total de divisiones que tiene el Ã¡rbol
  
  # DEFINICIÃ“N DEL ENTRENAMIENTO
  control<-trainControl(method = "cv", number=4, savePredictions = "all")
  
  # AJUSTE DEL MODELO
  set.seed(1234)
  gbm<- train(errores~.,
              data = train_semma,
              method="gbm",
              trControl=control,
              tuneGrid=gbmgrid,
              distribution= "gaussian",
              bag.fraction=1,# 1=GBM; 0.5=SGB
              verbose=FALSE)
  
  gbm
  plot(gbm)
  summary(gbm)

    # gbm_1: =====================================================================
  detach(package:plyr)
  source("cruzada gbm continua.R")
  medias_gbm_1<-cruzadagbm(data= train_semma,
                           vardep="errores",
                           listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                       "n",  "est_surv_vote", "prom_general_partido",
                                       "prom_general_wing","prom_casa_partido", 
                                       "prom_casa_wing", "prom_carrera_partido", 
                                       "prom_carrera_wing",
                                       "prom_carrera_casa_partido","prom_carrera_casa_wing", 
                                       "house_effect_e", 
                                       "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                       "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                       "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                       "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                       "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                       "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                       "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                       "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                       "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                       "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                       "party_AP", "party_BNG", "party_CC", "party_CCC", 
                                       "party_CDS", "party_CIU", "party_CS", "party_CUP", "party_EA", 
                                       "party_EE", "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", 
                                       "party_IU", "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", 
                                       "party_PP", "party_PSOE", "party_UCD", "party_UP", "party_UPYD", 
                                       "party_VOX", "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                       "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                       "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                       "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                       "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                       "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                       "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                       "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                       "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                       "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                       "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                       "gov_pre_UCD"),
                           listclass=c(""),
                           grupos=4,
                           sinicio=1234,
                           repe=10,
                           n.minobsinnode=82,
                           shrinkage=0.1,
                           n.trees=1000,
                           interaction.depth=2)
  medias_gbm_1$modelo="gbm_1"
  saveRDS(medias_gbm_1, "gbm_1")
  medias_gbm_1 <- readRDS("gbm_1")
  
    # gbm_2: =====================================================================
  medias_gbm_2<-cruzadagbm(data= train_semma,
                           vardep="errores",
                           listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                       "n",  "est_surv_vote", "prom_general_partido",
                                       "prom_general_wing","prom_casa_partido", 
                                       "prom_casa_wing", "prom_carrera_partido", 
                                       "prom_carrera_wing",
                                       "prom_carrera_casa_partido","prom_carrera_casa_wing", 
                                       "house_effect_e", 
                                       "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                       "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                       "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                       "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                       "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                       "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                       "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                       "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                       "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                       "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                       "party_AP", "party_BNG", "party_CC", "party_CCC", 
                                       "party_CDS", "party_CIU", "party_CS", "party_CUP", "party_EA", 
                                       "party_EE", "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", 
                                       "party_IU", "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", 
                                       "party_PP", "party_PSOE", "party_UCD", "party_UP", "party_UPYD", 
                                       "party_VOX", "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                       "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                       "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                       "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                       "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                       "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                       "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                       "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                       "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                       "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                       "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                       "gov_pre_UCD"),
                           listclass=c(""),
                           grupos=4,
                           sinicio=1234,
                           repe=10,
                           n.minobsinnode=10,
                           shrinkage=0.05,
                           n.trees=750,
                           interaction.depth=2)
  medias_gbm_2$modelo="gbm_2"
  saveRDS(medias_gbm_2, "gbm_2")
  medias_gbm_2 <- readRDS("gbm_2")
    # gbm_3: =====================================================================
  medias_gbm_3<-cruzadagbm(data= train_semma,
                           vardep="errores",
                           listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                       "n",  "est_surv_vote", "prom_general_partido",
                                       "prom_general_wing","prom_casa_partido", 
                                       "prom_casa_wing", "prom_carrera_partido", 
                                       "prom_carrera_wing",
                                       "prom_carrera_casa_partido","prom_carrera_casa_wing", 
                                       "house_effect_e", 
                                       "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                       "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                       "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                       "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                       "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                       "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                       "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                       "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                       "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                       "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                       "party_AP", "party_BNG", "party_CC", "party_CCC", 
                                       "party_CDS", "party_CIU", "party_CS", "party_CUP", "party_EA", 
                                       "party_EE", "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", 
                                       "party_IU", "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", 
                                       "party_PP", "party_PSOE", "party_UCD", "party_UP", "party_UPYD", 
                                       "party_VOX", "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                       "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                       "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                       "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                       "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                       "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                       "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                       "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                       "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                       "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                       "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                       "gov_pre_UCD"),
                           listclass=c(""),
                           grupos=4,
                           sinicio=1234,
                           repe=10,
                           n.minobsinnode=2,
                           shrinkage=0.1,
                           n.trees=1200,
                           interaction.depth=2)
  medias_gbm_3$modelo="gbm_3"
  saveRDS(medias_gbm_3, "gbm_3")
  medias_gbm_3 <- readRDS("gbm_3")
    # gbm_4: =====================================================================
  medias_gbm_4<-cruzadagbm(data= train_semma,
                           vardep="errores",
                           listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                       "n",  "est_surv_vote", "prom_general_partido",
                                       "prom_general_wing","prom_casa_partido", 
                                       "prom_casa_wing", "prom_carrera_partido", 
                                       "prom_carrera_wing",
                                       "prom_carrera_casa_partido","prom_carrera_casa_wing", 
                                       "house_effect_e", 
                                       "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                       "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                       "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                       "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                       "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                       "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                       "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                       "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                       "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                       "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                       "party_AP", "party_BNG", "party_CC", "party_CCC", 
                                       "party_CDS", "party_CIU", "party_CS", "party_CUP", "party_EA", 
                                       "party_EE", "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", 
                                       "party_IU", "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", 
                                       "party_PP", "party_PSOE", "party_UCD", "party_UP", "party_UPYD", 
                                       "party_VOX", "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                       "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                       "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                       "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                       "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                       "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                       "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                       "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                       "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                       "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                       "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                       "gov_pre_UCD"),
                           listclass=c(""),
                           grupos=4,
                           sinicio=1234,
                           repe=10,
                           n.minobsinnode=70,
                           shrinkage=1,
                           n.trees=800,
                           interaction.depth=2)
  medias_gbm_4$modelo="gbm_4"
  saveRDS(medias_gbm_4, "gbm_4")
  medias_gbm_4 <- readRDS("gbm_4")
  # EVALUAMOS EL MEJOR GBM =====================================================
  # Evaluamos mejor GBM
  medias_gbm_1 <- readRDS("gbm_1")
  medias_gbm_2 <- readRDS("gbm_2")
  medias_gbm_3 <- readRDS("gbm_3")
  medias_gbm_4 <- readRDS("gbm_4")
  medias_gbm_5 <- readRDS("gbm_5")
  union<-rbind(  #gbm
    medias_gbm_1, medias_gbm_2, medias_gbm_3, medias_gbm_4, medias_gbm_5
  )
  
  par(cex.axis=1)
  boxplot(data=union, error~modelo) 
  par(cex.axis=0.5)
  ggplot(union, aes(x=modelo, y=error, fill=modelo)) +
    geom_boxplot(outlier.colour="black", outlier.shape=1,
                 outlier.size=2) +
    labs(x = "Modelos de GBM", y = 'MAE', title = "Boxplot vc repetida gbm")+
    theme_grey()
  #Las observaciones fuera del boxplot (circulos) son errores outlier, o mejor dicho, errores muy anómalos para el modelo.  
    
  union<-rbind(medias_bag_arbol_1, medias_bag_arbol_1_OOB, medias_rf_bag_arbol_1, medias_rf_bag_arbol_1_btp,
               medias_bag_arbol_2_OOB, medias_rf_bag_arbol_2, medias_rf_bag_arbol_2_btp,
               medias_bag_arbol_3, medias_bag_arbol_3_OOB, medias_rf_bag_arbol_3, medias_rf_bag_arbol_3_btp,
               medias_arbol_1, medias_arbol_2, medias_arbol_3, medias_arbol_4, medias_arbol_5,
               medias_gbm_1, medias_gbm_2, medias_gbm_3, medias_gbm_4, medias_gbm_5)
  
  par(cex.axis=1)
  boxplot(data=union, error~modelo) 
 
  par(cex.axis=)
  ggplot(union, aes(x=modelo, y=error, fill=modelo)) +
    geom_boxplot(outlier.colour="black", outlier.shape=1,
                 outlier.size=2) +
    labs(x = "Modelos de bagging de árboles", y = 'MAE', title = "Boxplot vc repetida bagging") 
  
  # PREDICCIONES EN TEST =========================================================
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
  
  # CONCLUSIÓN SOBRE HISTÓRICO, MEJOR gbm ====================================
  eval_test_gbm_party <- eval_test_gbm %>% #reagrupar partits y casas 
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
    mutate(lead_party =
             case_when(str_detect(lead_party_CS, "1") ~ "CS",
                       str_detect(lead_party_PODEMOS, "1") ~ "PODEMOS",
                       str_detect(lead_party_PP, "1") ~"PP",
                       str_detect(lead_party_PSOE, "1") ~"PSOE",
                       str_detect(lead_party_UCD, "1") ~"UCD",
                       TRUE ~ "OTRAS")) %>% 
    mutate(lead2_party =
             case_when(str_detect(lead2_party_AP, "1") ~ "AP",
                       str_detect(lead2_party_ARM, "1") ~ "ARM",
                       str_detect(lead2_party_CS, "1") ~"CS",
                       str_detect(lead2_party_EA, "1") ~"EA",
                       str_detect(lead2_party_PODEMOS, "1") ~"PODEMOS",
                       str_detect(lead2_party_PP, "1") ~ "PP",
                       str_detect(lead2_party_PSOE, "1") ~ "PSOE",
                       str_detect(lead2_party_UCD, "1") ~"UCD",
                       str_detect(lead2_party_UP, "1") ~"UP",
                       str_detect(lead2_party_VOX, "1") ~"VOX",
                       TRUE ~ "OTRAS")) %>% 
    mutate(gov_pre =
             case_when(str_detect(gov_pre_PP, "1") ~ "PP",
                       str_detect(gov_pre_PSOE, "1") ~ "PSOE",
                       str_detect(gov_pre_UCD, "1") ~"UCD",
                       TRUE ~ "OTRAS"))
  
  
  eval_test_gbm_party2 <- eval_test_gbm_party %>% select("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                                         "n",  "est_surv_vote", "prom_general_partido", "prom_general_wing", 
                                                         "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido", 
                                                         "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing", 
                                                         "house_effect_e", "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                                         "urna_60", "urna_365", 
                                                         "errores", "party", "poll_firm", 
                                                         "lead_party", "lead2_party", "gov_pre", "error", "real_vote", "est_real_vote") 
  
  semma_id <-
    semma %>% 
    mutate(id_fin =
             glue("{year_elec}_{n_days_field}_{days_to_elec}_{n}_{party}_{poll_firm}")) 
  semma_id <-
    semma_id %>% 
    mutate(id_fin = as.character(id_fin))
  
  eval_test_gbm_party2 <-
    eval_test_gbm_party2 %>% 
    mutate(id_fin =
             glue("{year_elec}_{n_days_field}_{days_to_elec}_{n}_{party}_{poll_firm}")) 
  
  eval_test_gbm_party2 <-
    eval_test_gbm_party2 %>% 
    mutate(id_fin = as.character(id_fin))
  
  eval_test_gbm_party3 <- sqldf('
      SELECT a.* 
           , b.wing
           , b.date_elec
           , b.id_semma
      FROM eval_test_gbm_party2  AS a
      LEFT JOIN (
                SELECT *
                FROM semma_id ) AS b
            ON   (a.id_fin = b.id_fin)
            ')
  
  eval_test_gbm_party3 <-
    eval_test_gbm_party3 %>% 
    mutate(wing =
             case_when(str_detect(party, "VOX")|                                                                            
                         str_detect(party, "UCD")|
                         str_detect(party, "FN")|
                         str_detect(party, "CIU")|
                         str_detect(party, "CDS")|
                         str_detect(party, "CC")|
                         str_detect(party, "AP")|
                         str_detect(party, "cs")|
                         str_detect(party, "PP") ~ "RIGHT",
                       TRUE ~ "LEFT"))
  
  eval_test_gbm_party3 <-
    eval_test_gbm_party3 %>% 
    mutate(date_elec =
             case_when(str_detect(year_elec, "1982") ~ "1982-10-28",                                                                        
                       str_detect(year_elec, "1986") ~ "1986-06-22",
                       str_detect(year_elec, "1989") ~ "1989-10-29",
                       str_detect(year_elec, "1993") ~ "1993-06-06",
                       str_detect(year_elec, "1996") ~ "1996-03-03",
                       str_detect(year_elec, "2000") ~ "2000-03-12",
                       str_detect(year_elec, "2004") ~ "2004-03-14",
                       str_detect(year_elec, "2008") ~ "2008-03-09",
                       str_detect(year_elec, "2011") ~ "2011-11-20",
                       str_detect(year_elec, "2015") ~ "2015-12-20",
                       str_detect(year_elec, "2016") ~ "2016-06-26",
                       str_detect(date_elec, "2019-04-28 02:00:00") ~ "2019-04-28",
                       str_detect(date_elec, "2019-11-10 01:00:00") ~ "2019-11-10",
                       TRUE ~ "NA"))
  
  
  # MEDIA POR PARTY Y CARRERA: ¿Medias de las predicciones = predicción del voto real?
  eval_test_gbm_party <- group_by(eval_test_gbm_party3, date_elec, party) #OJO con 2019 tenemos que recuprara el date elec
  eval_test_gbm_party <- summarise(eval_test_gbm_party, prediccion_de_partido = mean(est_real_vote, na.rm = TRUE))
  eval_test_gbm_party <- eval_test_gbm_party %>% filter(!(date_elec == "NA"),)
  # Falta añadir con un join el valor real para hacer la comparativa. 
  eval_test_gbm_party
  semma_dos <- semma %>% mutate(date_elec2 = as.character(date_elec))
  
  eval_test_gbm_party <- sqldf('
      SELECT a.* 
           , b.real_vote
           , b.prom_carrera_partido
      FROM eval_test_gbm_party  AS a
      LEFT JOIN (
                SELECT *
                FROM semma_dos ) AS b
            ON (a.date_elec = b.date_elec2)
            AND (a.party = b.party)
            ')
  eval_test_gbm_party<-eval_test_gbm_party[!duplicated(eval_test_gbm_party), ]
  # install.packages("CGPfunctions")
  library(CGPfunctions)
  # install.packages("ggplot2")
  library(ggplot2)
  
  newggslopegraph(eval_test_gbm_party, date_elec, real_vote, party,
                  Title = "Evolución del PIB",
                  SubTitle = "1970-1979",
                  Caption =  "Autor: Enric Palau Payeras | Datos: Spanish elections dataset") +
    theme_gray() +
    theme(legend.position = "none")
  
  # install.packages("ggplot2")
 
  carreras <- split(eval_test_gbm_party, eval_test_gbm_party$date_elec)
  eval_test_gbm_party_2019_11 <-carreras[["2019-11-10"]]
  
  a<-ggplot(eval_test_gbm_party_2019_11) +
    geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                     y = party, yend = party)) +
    geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
    geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
    theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2019-11-10)")+
    theme(legend.position = "bottom")
  
  eval_test_gbm_party_2019_04 <-carreras[["2019-04-28"]]
  
  b<-ggplot(eval_test_gbm_party_2019_04) +
    geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                     y = party, yend = party)) +
    geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
    geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
    theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2019-04-28)")+
    theme(legend.position = "bottom")
  
  eval_test_gbm_party_2016 <-carreras[["2016-06-26"]]
  
  c<-ggplot(eval_test_gbm_party_2016) +
    geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                     y = party, yend = party)) +
    geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
    geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
    theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2016-06-26)")+
    theme(legend.position = "bottom")
  
  eval_test_gbm_party_2015 <-carreras[["2015-12-20"]]
  
  d<-ggplot(eval_test_gbm_party_2015) +
    geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                     y = party, yend = party)) +
    geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
    geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
    theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2015-12-20)")+
    theme(legend.position = "bottom")
  
  comparativa_test_gbm <- ggarrange(a, b, c, d,
                                    ncol = 2, nrow = 2)
  comparativa_test_gbm
  
  # PREDICCIONES EN TEST 2023 =========================================================
  # Predecimos en test con el modelo seleccionado
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
  
  
#### 1.2. Modelos de regresión (WRAPPERS): ####
   #### 1.2.1. AIC: ####
  set.seed(1234)
  full<-glm(errores~.,data = train_semma, family = gaussian(link = "identity"))
  null<-glm(errores~1,data = train_semma, family = gaussian(link = "identity"))
  selec1<-stepAIC(null,scope=list(upper=full),
                  direction="both",family = gaussian(link = "identity"),trace=FALSE)
  vec<-(names(selec1[[1]]))
  length(vec)
  dput(vec)
  detach(package:plyr)
  source("cruzadas avnnet y lin.R")
  medias_reg_AIC_1<-cruzadalin(data = train_semma,
                               vardep="errores",
                               listconti= c(
                                 "house_effect_e", "party_PODEMOS", "party_UP", 
                                 "est_surv_vote", "prom_carrera_partido", "prom_carrera_wing", 
                                 "party_VOX", "party_PSOE", "party_EA", "gov_pre_PP", "party_CIU", 
                                 "party_IU", "party_CC", "party_AP", "pobl_pobreza_rate", "gov_cor_rate", 
                                 "party_UPYD", "eco_pib_var", "prom_general_wing", "prom_casa_partido", 
                                 "prom_general_partido", "party_UCD", "party_PCE", "pobl_suicide", 
                                 "eco_unployement", "party_HB", "eco_fisc_ing_percap", "pobl_life_expectancy", 
                                 "pobl_densidad", "days_to_elec", "party_BNG", "poll_firm_ASEP", 
                                 "poll_firm_METROSCOPIA", "party_PA", "party_PNV", "party_EE", 
                                 "lead2_party_UP", "poll_firm_SOCIOMÉTRICA", "poll_firm_ELECTOPANEL", 
                                 "lead2_party_UCD", "poll_firm_GESOP", "poll_firm_TNS_DEMOSCOPIA", 
                                 "poll_firm_NOXA", "lead_party_PODEMOS"
                                  ),
                               listclass= c(""),
                               grupos=4,
                               sinicio=1234,
                               repe=10)
  medias_reg_AIC_1$modelo="AIC_1"
  saveRDS(medias_reg_AIC_1, "AIC_1")
  medias_reg_AIC_1 <- readRDS("AIC_1")
  
  
    #### 1.2.2. AIC Repetido: ####
  
  source("funcion steprepetido.R")
  
  lista<-steprepetido(data = train_semma,
                      vardep=c("errores"),
                      listconti=c("error_carrera_casa_partido", "est_surv_vote", 
                                  "prom_carrera_casa_partido", "error_carrera_wing", "gov_pre_PSOE", 
                                  "party_EH.BILDU", "poll_firm_SOCIOMÉTRICA", "n", "poll_firm_GESOP", 
                                  "poll_firm_VOX_PÚBLICA", "lead2_party_PSOE", "urna_7"),
                      sinicio=1234,
                      sfinal=1238,
                      porcen=0.8,
                      criterio="AIC")
  
  tabla<-lista[[1]]
  dput(lista[[2]][[1]])
  
  # medias2<-cruzadalin(data = train_semma,
  #                      vardep="error",
  #                      listconti= c("party_UP", "party_PODEMOS", "est_surv_vote", 
  #                                   "prom_carrera_partido", "party_VOX", "party_EA", "party_IU", 
  #                                   "party_UPYD", "lead2_party_ARM", "party_UCD", 
  #                                   "party_CS", "gov_exp_san_percap", "party_BNG", 
  #                                   "party_CCC", "party_FN", "party_CIU", "poll_firm_OBRADOIRO_SOCIO", 
  #                                   "lead2_party_EA", "lead2_party_PSOE", "poll_firm_METROSCOPIA", 
  #                                   "poll_firm_MYWORD", "poll_firm_SIMPLE_LÓGICA", "poll_firm_NC_REPORT", 
  #                                   "porc_surveys_firm", "party_PNV", "party_CDS"),
  #                      listclass=c(""),
  #                      grupos=4,
  #                      sinicio=1234,
  #                      repe=10)
  #  medias2$modelo="repAIC"
  #  saveRDS(medias2, "repAIC")
  medias_step_rep_AIC <- readRDS("repAIC")
  
  #### 1.2.3. BIC: ####
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
  
  detach(package:plyr)
  # library(dplyr)
  source("cruzadas avnnet y lin.R")
  medias_reg_BIC_1<-cruzadalin(data = train_semma,
                       vardep="errores",
                       listconti= c("house_effect_e", "party_PODEMOS", "party_UP", 
                                    "est_surv_vote", "prom_carrera_partido", "prom_carrera_wing", 
                                    "party_VOX", "party_PSOE", "party_EA", "gov_pre_PP", "party_CIU", 
                                    "party_IU", "party_CC", "party_AP", "pobl_pobreza_rate", "gov_cor_rate", 
                                    "party_UPYD", "eco_pib_var", "prom_general_wing", "prom_casa_partido", 
                                    "prom_general_partido", "party_UCD", "party_PCE", "pobl_suicide", 
                                    "eco_unployement", "party_HB", "eco_fisc_ing_percap", "pobl_life_expectancy", 
                                    "pobl_densidad", "days_to_elec", "party_BNG", "poll_firm_ASEP", 
                                    "poll_firm_METROSCOPIA", "party_PA", "party_PNV", "party_EE", 
                                    "lead2_party_UP", "poll_firm_SOCIOMÉTRICA", "poll_firm_ELECTOPANEL"),
                       listclass=c(""),
                       grupos=4,
                       sinicio=1234,
                       repe=10)
  medias_reg_BIC_1$modelo="BIC_1"
  saveRDS(medias_reg_BIC_1, "BIC_1")
  medias_reg_BIC_1 <- readRDS("BIC_1")
  union<-rbind(medias_reg_BIC_1, medias_reg_AIC_1)
  par(cex.axis=1)
  boxplot(data=union, error~modelo) 
 
  library(viridis)
  boxplot(data=union, error~modelo, col=plasma(5))
  ggplot(union, aes(x=modelo, y=error, fill=modelo)) +
    geom_boxplot(outlier.colour="black", outlier.shape=1,
                 outlier.size=2) +
    labs(x = "Modelos de regresión", y = 'MAE', title = "Boxplot vc repetida WRAPPERS")
  
    #### 1.2.4. BIC Repetido: ####
  source("funcion steprepetido.R")
  
  lista<-steprepetido(data = train_semma,
                      vardep=c("errores"),
                      listconti=c("error_carrera_casa_partido", "est_surv_vote", 
                                  "prom_carrera_casa_partido", "error_carrera_wing", "gov_pre_PSOE", 
                                  "party_EH.BILDU", "poll_firm_SOCIOMÉTRICA", "n", "poll_firm_GESOP", 
                                  "poll_firm_VOX_PÚBLICA", "lead2_party_PSOE"),
                      sinicio=12345,
                      sfinal=12385,
                      porcen=0.8,
                      criterio="BIC")
  
  #repBIC
  tabla<-lista[[1]]
  dput(lista[[2]][[1]])
  
  
  #### 1.2.5. Boruta: ####
  out.boruta <- Boruta(errores~., data = train_semma)
  
  print(out.boruta)
  
  summary(out.boruta)
  
  sal<-data.frame(out.boruta$finalDecision)
  
  sal2<-sal[which(sal$out.boruta.finalDecision=="Confirmed"),,drop=FALSE]
  dput(row.names(sal2))
  
  length(dput(row.names(sal2)))

  # detach(package:plyr)
  # library(dplyr)
  source("cruzadas avnnet y lin.R")
  medias_reg_Boruta <-cruzadalin(data = train_semma,
                               vardep="errores",
                               listconti= c(
                                 c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm",
                                   "n", "est_surv_vote", "prom_general_partido", "prom_general_wing",
                                   "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido",
                                   "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing",
                                   "house_effect_e", "wing_effect_e", "urna_7", "urna_15", "urna_60",
                                   "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", "pobl_kill",
                                   "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil",
                                   "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate",
                                   "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing",
                                   "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var",
                                   "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap",
                                   "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap",
                                   "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap",
                                   "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap",
                                   "party_AP", "party_BNG", "party_CC", "party_CCC", "party_CIU",
                                   "party_CS", "party_EA", "party_EH.BILDU", "party_ERC", "party_IU",
                                   "party_PNV", "party_PODEMOS", "party_PP", "party_PSOE", "party_UCD",
                                   "party_UP", "party_UPYD", "party_VOX", "wing_LEFT", "wing_RIGHT",
                                   "poll_firm_ASEP", "poll_firm_CELESTE.TEL", "poll_firm_CIS", "poll_firm_ELECTOPANEL",
                                   "poll_firm_GAD3", "poll_firm_METROSCOPIA", "poll_firm_NC_REPORT",
                                   "poll_firm_SIMPLE_LÓGICA", "poll_firm_TNS_DEMOSCOPIA", "lead_party_CS",
                                   "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", "lead2_party_CS",
                                   "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE",
                                   "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE")
                               ),
                               listclass= c(""),
                               grupos=4,
                               sinicio=1234,
                               repe=10)
  medias_reg_Boruta$modelo="boruta"
  saveRDS(medias_reg_Boruta, "boruta")
  medias_reg_Boruta <- readRDS("boruta")
  

# ----- MODELOS: 2. Redes y SVM: -----
#### 2.1. Modelos de redes: ####
  # HIPERPARÁMETROS,iteraciones decay y nodos
  nnetgrid <-  expand.grid(
    size=c(5, 50, 150, 500),
    decay=c(1, 0.1, 0.01, 0.001),
    bag=F)
  # DEFINICIÓN DEL ENTRENAMIENTO
  set.seed(1234)
  control<-trainControl(method = "repeatedcv",
                        number=4, repeats=3, savePredictions = "all")
  completo<-data.frame()
  listaiter<-c(10,50,100,500,1000)

  for (iter in listaiter)
  {rednnet<- train( errores~house_effect_e+ party_PODEMOS+ party_UP+
                    est_surv_vote+ prom_carrera_partido+ prom_carrera_wing+
                    party_VOX+ party_PSOE+ party_EA+ gov_pre_PP+ party_CIU+
                    party_IU+ party_CC+ party_AP+ pobl_pobreza_rate+ party_UPYD+
                    prom_general_wing+ prom_casa_partido+ prom_general_partido+
                    party_UCD+ eco_fisc_ing_percap+ gov_cor_rate+ gov_exp_edu+
                    party_PCE+ gov_pre_PSOE+ party_HB+ party_BNG+ poll_firm_ASEP+
                    eco_pib_var+ pobl_idh+ poll_firm_METROSCOPIA+ party_PA+
                    party_PNV+ party_EE+ pobl+ poll_firm_SOCIOMÉTRICA+ days_to_elec+
                    poll_firm_ELECTOPANEL+ lead2_party_UP+ poll_firm_GESOP,
                    data = train_semma_lineal,
                    method="avNNet",linout = TRUE,maxit=iter,
                    trControl=control,repeats=3,tuneGrid=nnetgrid,trace=F)
    # Añado la columna del parametro de iteraciones
    rednnet$results$itera<-iter
    # Voy incorporando los resultados a completo
    completo<-rbind(completo,rednnet$results)}

  completo<-completo[order(completo$MAE),]
  # PLOT (AJUSTE: decay, size e iteraciones)
  ggplot(completo, aes(x=factor(itera), y=MAE, 
                       color=factor(decay),pch=factor(size))) +
    geom_point(position=position_dodge(width=0.5),size=3)+
    geom_smooth(method="lm")
  
  #### Red basada en el primer set de variables (proceso de seleccción en SAS y documento de TFM) #####
  # HIPERPARÁMETROS,iteraciones decay y nodos
  nnetgrid <-  expand.grid(
    size=c(1, 3, 6, 12),
    decay=c(2, 1, 0.5, 0.1, 0.01),
    bag=F)
  # DEFINICIÓN DEL ENTRENAMIENTO-> set GBM_3
  set.seed(1234)
  control<-trainControl(method = "repeatedcv",
                        number=4, repeats=3, savePredictions = "all")
  completo<-data.frame()
  listaiter<-c(10,50,100,500,1000)
  
  for (iter in listaiter)
  {rednnet<- train( errores~year_elec+n_days_field+days_to_elec+porc_surveys_firm+ 
                    n+est_surv_vote+prom_general_partido+
                    prom_general_wing+prom_casa_partido+ 
                    prom_casa_wing+prom_carrera_partido+ 
                    prom_carrera_wing+
                    prom_carrera_casa_partido+prom_carrera_casa_wing+ 
                    house_effect_e+ 
                    wing_effect_e+urna_0+urna_7+urna_15+ 
                    urna_60+urna_365+pobl_densidad+pobl_fem_porc+pobl+ 
                    pobl_kill+pobl_kill_percienmil+pobl_suicide+pobl_suicide_percienmil+ 
                    pobl_life_expectancy+pobl_idh+pobl_im_rate+pobl_em_rate+ 
                    pobl_pobreza_rate+eco_smi+eco_rate_avg+eco_fisc_ing+ 
                    eco_fisc_ing_percap+eco_debt_percap+eco_deficit+eco_pib_var+ 
                    env_gwh_prod+env_gwh_prod_renovable+env_gwh_consum+env_kwh_consum_percap+ 
                    env_co2+env_co2_percap+eco_unployement+eco_pib_percap+ 
                    gov_exp_pib+gov_cor_rate+gov_exp_war+gov_exp_war_percap+ 
                    gov_exp_san+gov_exp_san_percap+gov_exp_edu+gov_exp_edu_percap+ 
                    party_AP+party_BNG+party_CC+party_CCC+ 
                    party_CDS+party_CIU+party_CS+party_CUP+party_EA+ 
                    party_EE+party_EH.BILDU+party_ERC+party_FN+party_HB+ 
                    party_IU+party_PA+party_PCE+party_PNV+party_PODEMOS+ 
                    party_PP+party_PSOE+party_UCD+party_UP+party_UPYD+ 
                    party_VOX+wing_LEFT+wing_RIGHT+poll_firm_ASEP+poll_firm_CELESTE.TEL+ 
                    poll_firm_CIS+poll_firm_DYM+poll_firm_ELECTOPANEL+poll_firm_GAD3+ 
                    poll_firm_GALLUP+poll_firm_GESOP+poll_firm_HAMALGAMA_MÉTRICA+ 
                    poll_firm_IMOP+poll_firm_METROSCOPIA+poll_firm_MYWORD+ 
                    poll_firm_NC_REPORT+poll_firm_NOXA+poll_firm_OBRADOIRO_SOCIO+ 
                    poll_firm_OPINA+poll_firm_SIGMA_DOS+poll_firm_SIMPLE_LÓGICA+ 
                    poll_firm_SOCIOMÉTRICA+poll_firm_TNS_DEMOSCOPIA+poll_firm_VOX_PÚBLICA+ 
                    lead_party_CS+lead_party_PODEMOS+lead_party_PP+lead_party_PSOE+ 
                    lead_party_UCD+lead2_party_AP+lead2_party_ARM+lead2_party_CS+ 
                    lead2_party_EA+lead2_party_PODEMOS+lead2_party_PP+lead2_party_PSOE+ 
                    lead2_party_UCD+lead2_party_UP+gov_pre_PP+ gov_pre_PSOE+ 
                    gov_pre_UCD,
                    data = train_semma,
                    method="avNNet",linout = TRUE,maxit=iter,
                    trControl=control,repeats=3,tuneGrid=nnetgrid,trace=F)
  # Añado la columna del parametro de iteraciones
  rednnet$results$itera<-iter
  # Voy incorporando los resultados a completo
  completo<-rbind(completo,rednnet$results)}
  
  completo<-completo[order(completo$MAE),]
  ggplot(completo, aes(x=factor(itera), y=MAE, 
                       color=factor(decay),pch=factor(size))) +
    geom_point(position=position_dodge(width=0.5),size=3)+
    geom_smooth(method="lm")
  # 500 0.5 1
  #1000 0.1 3
  
  # VALIDACIÓN CRUZADA REPETIDA
  detach(package:plyr)
  source("cruzadas avnnet y lin.R")
  medias_red_arbol_a<-cruzadaavnnet(data = train_semma,
                          vardep="errores",
                          listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                        "n",  "est_surv_vote", "prom_general_partido",
                                        "prom_general_wing","prom_casa_partido", 
                                        "prom_casa_wing", "prom_carrera_partido", 
                                        "prom_carrera_wing",
                                        "prom_carrera_casa_partido","prom_carrera_casa_wing", 
                                        "house_effect_e", 
                                        "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                        "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                        "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                        "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                        "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                        "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                        "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                        "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                        "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                        "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                        "party_AP", "party_BNG", "party_CC", "party_CCC", 
                                        "party_CDS", "party_CIU", "party_CS", "party_CUP", "party_EA", 
                                        "party_EE", "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", 
                                        "party_IU", "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", 
                                        "party_PP", "party_PSOE", "party_UCD", "party_UP", "party_UPYD", 
                                        "party_VOX", "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                        "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                        "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                        "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                        "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                        "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                        "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                        "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                        "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                        "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                        "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                        "gov_pre_UCD"),                        
                          listclass=c(""),
                          grupos=4,
                          sinicio=1234,
                          repe=10,
                          size=c(1),
                          decay=c(0.5),
                          repeticiones=5,
                          itera=1000)
  medias_red_arbol_a
  medias_red_arbol_a$modelo="red_arbol_a"
  saveRDS(medias_red_arbol_a, "red_arbol_a")
  medias_red_arbol_a <- readRDS("red_arbol_a")
  
  medias_red_arbol_b<-cruzadaavnnet(data = train_semma,
                                  vardep="errores",
                                  listconti=c("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                                "n",  "est_surv_vote", "prom_general_partido",
                                                "prom_general_wing","prom_casa_partido", 
                                                "prom_casa_wing", "prom_carrera_partido", 
                                                "prom_carrera_wing",
                                                "prom_carrera_casa_partido","prom_carrera_casa_wing", 
                                                "house_effect_e", 
                                                "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                                "urna_60", "urna_365", "pobl_densidad", "pobl_fem_porc", "pobl", 
                                                "pobl_kill", "pobl_kill_percienmil", "pobl_suicide", "pobl_suicide_percienmil", 
                                                "pobl_life_expectancy", "pobl_idh", "pobl_im_rate", "pobl_em_rate", 
                                                "pobl_pobreza_rate", "eco_smi", "eco_rate_avg", "eco_fisc_ing", 
                                                "eco_fisc_ing_percap", "eco_debt_percap", "eco_deficit", "eco_pib_var", 
                                                "env_gwh_prod", "env_gwh_prod_renovable", "env_gwh_consum", "env_kwh_consum_percap", 
                                                "env_co2", "env_co2_percap", "eco_unployement", "eco_pib_percap", 
                                                "gov_exp_pib", "gov_cor_rate", "gov_exp_war", "gov_exp_war_percap", 
                                                "gov_exp_san", "gov_exp_san_percap", "gov_exp_edu", "gov_exp_edu_percap", 
                                                "party_AP", "party_BNG", "party_CC", "party_CCC", 
                                                "party_CDS", "party_CIU", "party_CS", "party_CUP", "party_EA", 
                                                "party_EE", "party_EH.BILDU", "party_ERC", "party_FN", "party_HB", 
                                                "party_IU", "party_PA", "party_PCE", "party_PNV", "party_PODEMOS", 
                                                "party_PP", "party_PSOE", "party_UCD", "party_UP", "party_UPYD", 
                                                "party_VOX", "wing_LEFT", "wing_RIGHT", "poll_firm_ASEP", "poll_firm_CELESTE.TEL", 
                                                "poll_firm_CIS", "poll_firm_DYM", "poll_firm_ELECTOPANEL", "poll_firm_GAD3", 
                                                "poll_firm_GALLUP", "poll_firm_GESOP", "poll_firm_HAMALGAMA_MÉTRICA", 
                                                "poll_firm_IMOP", "poll_firm_METROSCOPIA", "poll_firm_MYWORD", 
                                                "poll_firm_NC_REPORT", "poll_firm_NOXA", "poll_firm_OBRADOIRO_SOCIO", 
                                                "poll_firm_OPINA", "poll_firm_SIGMA_DOS", "poll_firm_SIMPLE_LÓGICA", 
                                                "poll_firm_SOCIOMÉTRICA", "poll_firm_TNS_DEMOSCOPIA", "poll_firm_VOX_PÚBLICA", 
                                                "lead_party_CS", "lead_party_PODEMOS", "lead_party_PP", "lead_party_PSOE", 
                                                "lead_party_UCD", "lead2_party_AP", "lead2_party_ARM", "lead2_party_CS", 
                                                "lead2_party_EA", "lead2_party_PODEMOS", "lead2_party_PP", "lead2_party_PSOE", 
                                                "lead2_party_UCD", "lead2_party_UP", "gov_pre_PP", "gov_pre_PSOE", 
                                                "gov_pre_UCD"),                        
                                  listclass=c(""),
                                  grupos=4,
                                  sinicio=1234,
                                  repe=10,
                                  size=c(3),
                                  decay=c(0.1),
                                  repeticiones=5,
                                  itera=500)
  medias_red_arbol_b
  medias_red_arbol_b$modelo="red_arbol_b"
  saveRDS(medias_red_arbol_b, "red_arbol_b")
  medias_red_arbol_b <- readRDS("red_arbol_b")
  
  union<-rbind(medias_red_arbol_b, medias_red_arbol_a)
  par(cex.axis=1)
  boxplot(data=union, error~modelo) 
 
  library(viridis)
  boxplot(data=union, error~modelo, col=plasma(5))
  ggplot(union, aes(x=modelo, y=error, fill=modelo)) +
    geom_boxplot(outlier.colour="black", outlier.shape=1,
                 outlier.size=2) +
    labs(x = "Modelos de red", y = 'MAE', title = "Boxplot vc repetida redes") 
  #Las observaciones fuera del boxplot (circulos) son errores outlier, o mejor dicho, errores muy anómalos para el modelo.
  nnetgrid <- expand.grid(size=3, decay=0.1, bag=F)
  nnetgrid <- expand.grid(size=1, decay=0.5, bag=F)
  nnetgrid <- expand.grid(size=6, decay=0.5, bag=F)
  red_ganador <- train(errores~year_elec+n_days_field+days_to_elec+porc_surveys_firm+ 
                         n+est_surv_vote+prom_general_partido+
                         prom_general_wing+prom_casa_partido+ 
                         prom_casa_wing+prom_carrera_partido+ 
                         prom_carrera_wing+
                         prom_carrera_casa_partido+prom_carrera_casa_wing+ 
                         house_effect_e+ 
                         wing_effect_e+urna_0+urna_7+urna_15+ 
                         urna_60+urna_365+pobl_densidad+pobl_fem_porc+pobl+ 
                         pobl_kill+pobl_kill_percienmil+pobl_suicide+pobl_suicide_percienmil+ 
                         pobl_life_expectancy+pobl_idh+pobl_im_rate+pobl_em_rate+ 
                         pobl_pobreza_rate+eco_smi+eco_rate_avg+eco_fisc_ing+ 
                         eco_fisc_ing_percap+eco_debt_percap+eco_deficit+eco_pib_var+ 
                         env_gwh_prod+env_gwh_prod_renovable+env_gwh_consum+env_kwh_consum_percap+ 
                         env_co2+env_co2_percap+eco_unployement+eco_pib_percap+ 
                         gov_exp_pib+gov_cor_rate+gov_exp_war+gov_exp_war_percap+ 
                         gov_exp_san+gov_exp_san_percap+gov_exp_edu+gov_exp_edu_percap+ 
                         party_AP+party_BNG+party_CC+party_CCC+ 
                         party_CDS+party_CIU+party_CS+party_CUP+party_EA+ 
                         party_EE+party_EH.BILDU+party_ERC+party_FN+party_HB+ 
                         party_IU+party_PA+party_PCE+party_PNV+party_PODEMOS+ 
                         party_PP+party_PSOE+party_UCD+party_UP+party_UPYD+ 
                         party_VOX+wing_LEFT+wing_RIGHT+poll_firm_ASEP+poll_firm_CELESTE.TEL+ 
                         poll_firm_CIS+poll_firm_DYM+poll_firm_ELECTOPANEL+poll_firm_GAD3+ 
                         poll_firm_GALLUP+poll_firm_GESOP+poll_firm_HAMALGAMA_MÉTRICA+ 
                         poll_firm_IMOP+poll_firm_METROSCOPIA+poll_firm_MYWORD+ 
                         poll_firm_NC_REPORT+poll_firm_NOXA+poll_firm_OBRADOIRO_SOCIO+ 
                         poll_firm_OPINA+poll_firm_SIGMA_DOS+poll_firm_SIMPLE_LÓGICA+ 
                         poll_firm_SOCIOMÉTRICA+poll_firm_TNS_DEMOSCOPIA+poll_firm_VOX_PÚBLICA+ 
                         lead_party_CS+lead_party_PODEMOS+lead_party_PP+lead_party_PSOE+ 
                         lead_party_UCD+lead2_party_AP+lead2_party_ARM+lead2_party_CS+ 
                         lead2_party_EA+lead2_party_PODEMOS+lead2_party_PP+lead2_party_PSOE+ 
                         lead2_party_UCD+lead2_party_UP+gov_pre_PP+ gov_pre_PSOE+ 
                         gov_pre_UCD,
                       data=train_semma,
                       method="avNNet",
                       linout = TRUE,
                       maxit=1000,
                       tunegrid=nnetgrid,
                       trace=TRUE)
  red_ganador
  saveRDS(red_ganador, "red_ganador")
  
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
    mutate(est_real_vote = est_surv_vote + prediccion ) # Estimación de voto del modelo o corrección del modelo aplicada a la encuesta
  # write_csv(eval_test_red, file = "./EXPORTADO/eval_test_red.csv")
  
  # gráfico de error real y error del modelo
  ggplot(data = eval_test_red,
         mapping = aes(x = prediccion, y = errores)) +
    geom_point(color = "#006EA1", alpha = 0.5, size = 4) +
    # Diagonal
    geom_abline(intercept = 0, slope = 1,
                color = "orange", size = 1.5) +
    labs(title = "Resultados del red en test",
         subtitle =
           "Valores deberían estar cercanos a la diagonal. Modelo: size=6, decay=0.5, maxite=500",
         caption =
           "Autor: Enric Palau Payeras | Datos: Spanish electoral dataset",
         x = "Predicciones",
         y = "Valores Reales")
  
  # gráfico de voto real y voto según el modelo  
  Nuestro_modelo<-ggplot(data = eval_test_red,
                         mapping = aes(x = est_real_vote, y = real_vote)) +
    geom_point(color = "#006EA1", alpha = 0.5, size = 4) +
    # Diagonal
    geom_abline(intercept = 0, slope = 1,
                color = "orange", size = 1.5) +
    labs(title = "Voto real según nuestro modelo Red (size=6, decay=0.5, maxite=500)",
         subtitle =
           "Valores deberían estar cercanos a la diagonal",
         x = "Voto estimado por el modelo",
         y = "Valores Reales")
  
  # gráfico de voto real y voto según encuestas 
  Encuestas <-ggplot(data = eval_test_red,
                     mapping = aes(x = est_surv_vote, y = real_vote)) +
    geom_point(color = "#006EA1", alpha = 0.5, size = 4) +
    # Diagonal
    geom_abline(intercept = 0, slope = 1,
                color = "orange", size = 1.5) +
    labs(title = "Voto real según encuestadoras",
         caption =
           "Autor: Enric Palau Payeras | Datos: Spanish electoral dataset",
         x = "Voto estimado por las encuestas",
         y = "Valores Reales")
  
  comparativa_red <- ggarrange(Nuestro_modelo, Encuestas,
                               ncol = 1, nrow = 2)
  comparativa_red
  

  
  #### red_BIC; Red basada en el set de variables del conjunto BIC #####
  # HIPERPARÁMETROS,iteraciones decay y nodos
  nnetgrid <-  expand.grid(
    size=c(1, 3, 6, 12),
    decay=c(0.9, 0.5, 0.1, 0.01, 0.001),
    bag=F)
  # DEFINICIÓN DEL ENTRENAMIENTO-> set BIC
  set.seed(1234)
  control<-trainControl(method = "repeatedcv",
                        number=4, repeats=3, savePredictions = "all")
  completo<-data.frame()
  listaiter<-c(10,50,100,500,1000)
  
  for (iter in listaiter)
  {rednnet<- train( errores~house_effect_e+ party_PODEMOS+ party_UP+ 
                      est_surv_vote+ prom_carrera_partido+ prom_carrera_wing+ 
                      party_VOX+ party_PSOE+ party_EA+ gov_pre_PP+ party_CIU+ 
                      party_IU+ party_CC+ party_AP+ pobl_pobreza_rate+ gov_cor_rate+ 
                      party_UPYD+ eco_pib_var+ prom_general_wing+ prom_casa_partido+ 
                      prom_general_partido+ party_UCD+ party_PCE+ pobl_suicide+ 
                      eco_unployement+ party_HB+ eco_fisc_ing_percap+ pobl_life_expectancy+ 
                      pobl_densidad+ days_to_elec+ party_BNG+ poll_firm_ASEP+ 
                      poll_firm_METROSCOPIA+ party_PA+ party_PNV+ party_EE+ 
                      lead2_party_UP+ poll_firm_SOCIOMÉTRICA+ poll_firm_ELECTOPANEL,
                    data = train_semma,
                    method="avNNet",linout = TRUE,maxit=iter,
                    trControl=control,repeats=3,tuneGrid=nnetgrid,trace=F)
  # Añado la columna del parametro de iteraciones
  rednnet$results$itera<-iter
  # Voy incorporando los resultados a completo
  completo<-rbind(completo,rednnet$results)}
  # saveRDS(completo, "completo")
  completo<-completo[order(completo$MAE),]
  ggplot(completo, aes(x=factor(itera), y=MAE, 
                       color=factor(decay),pch=factor(size))) +
    geom_point(position=position_dodge(width=0.5),size=3)+
    geom_smooth(method="lm")
  
  # VALIDACIÓN CRUZADA REPETIDA
  detach(package:plyr)
  source("cruzadas avnnet y lin.R")
  medias_red_BIC_1<-cruzadaavnnet(data=train_semma,
                                vardep="errores",
                                listconti=
                                  c("house_effect_e", "party_PODEMOS", "party_UP", 
                                    "est_surv_vote", "prom_carrera_partido", "prom_carrera_wing", 
                                    "party_VOX", "party_PSOE", "party_EA", "gov_pre_PP", "party_CIU", 
                                    "party_IU", "party_CC", "party_AP", "pobl_pobreza_rate", "gov_cor_rate", 
                                    "party_UPYD", "eco_pib_var", "prom_general_wing", "prom_casa_partido", 
                                    "prom_general_partido", "party_UCD", "party_PCE", "pobl_suicide", 
                                    "eco_unployement", "party_HB", "eco_fisc_ing_percap", "pobl_life_expectancy", 
                                    "pobl_densidad", "days_to_elec", "party_BNG", "poll_firm_ASEP", 
                                    "poll_firm_METROSCOPIA", "party_PA", "party_PNV", "party_EE", 
                                    "lead2_party_UP", "poll_firm_SOCIOMÉTRICA", "poll_firm_ELECTOPANEL"),                        
                                listclass=c(""),
                                grupos=4,
                                sinicio=1234,
                                repe=10,
                                size=c(12),
                                decay=c(2),
                                repeticiones=5,
                                itera=1000)
  medias_red_BIC_1
  medias_red_BIC_1$modelo="red_BIC_1"
  saveRDS(medias_red_BIC_1, "red_BIC_1")
  medias_red_BIC_1 <- readRDS("red_BIC_1")
  
  medias_red_BIC_2<-cruzadaavnnet(data=train_semma,
                                  vardep="errores",
                                  listconti=
                                    c("house_effect_e", "party_PODEMOS", "party_UP", 
                                         "est_surv_vote", "prom_carrera_partido", "prom_carrera_wing", 
                                         "party_VOX", "party_PSOE", "party_EA", "gov_pre_PP", "party_CIU", 
                                         "party_IU", "party_CC", "party_AP", "pobl_pobreza_rate", "gov_cor_rate", 
                                         "party_UPYD", "eco_pib_var", "prom_general_wing", "prom_casa_partido", 
                                         "prom_general_partido", "party_UCD", "party_PCE", "pobl_suicide", 
                                         "eco_unployement", "party_HB", "eco_fisc_ing_percap", "pobl_life_expectancy", 
                                         "pobl_densidad", "days_to_elec", "party_BNG", "poll_firm_ASEP", 
                                         "poll_firm_METROSCOPIA", "party_PA", "party_PNV", "party_EE", 
                                         "lead2_party_UP", "poll_firm_SOCIOMÉTRICA", "poll_firm_ELECTOPANEL"),                        
                                  listclass=c(""),
                                  grupos=4,
                                  sinicio=1234,
                                  repe=10,
                                  size=c(6),
                                  decay=c(0.5),
                                  repeticiones=5,
                                  itera=1000)
  medias_red_BIC_2
  medias_red_BIC_2$modelo="red_BIC_2"
  saveRDS(medias_red_BIC_2, "red_BIC_2")
  medias_red_BIC_2 <- readRDS("red_BIC_2")
  

  # EVALUAMOS LA MEJOR RED =====================================================
  medias_red_BIC_1 <- readRDS("red_BIC_1")
  medias_red_BIC_2 <- readRDS("red_BIC_2") #minbucket = 70
  medias_red_BIC_3 <- readRDS("red_BIC_3")
  medias_red_BIC_4 <- readRDS("red_BIC_4") #minbucket = 70
  medias_red_BIC_5 <- readRDS("red_BIC_5")
  medias_red_arbol_a <- readRDS("red_arbol_a") #minbucket = 2
  medias_red_arbol_b <- readRDS("red_arbol_b") #minbucket = 40

  
  union<-rbind(medias_red_BIC_1, medias_red_BIC_2,medias_red_BIC_3, medias_red_BIC_4, medias_red_BIC_5, medias_red_arbol_b, medias_red_arbol_a)
  par(cex.axis=1)
  boxplot(data=union, error~modelo) 
 
  library(viridis)
  boxplot(data=union, error~modelo, col=plasma(5))
  ggplot(union, aes(x=modelo, y=error, fill=modelo)) +
    geom_boxplot(outlier.colour="black", outlier.shape=1,
                 outlier.size=2) +
    labs(x = "Modelos de red", y = 'MAE', title = "Boxplot vc repetida redes") +
    theme_grey()
  #Las observaciones fuera del boxplot (circulos) son errores outlier, o mejor dicho, errores muy anómalos para el modelo.
  
  # PREDICCIONES EN TEST =========================================================
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
  
  # CONCLUSIÓN SOBRE HISTÓRICO, MEJOR red ====================================
  eval_test_red_party <- eval_test_red %>% #reagrupar partits y casas 
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
    mutate(lead_party =
             case_when(str_detect(lead_party_CS, "1") ~ "CS",
                       str_detect(lead_party_PODEMOS, "1") ~ "PODEMOS",
                       str_detect(lead_party_PP, "1") ~"PP",
                       str_detect(lead_party_PSOE, "1") ~"PSOE",
                       str_detect(lead_party_UCD, "1") ~"UCD",
                       TRUE ~ "OTRAS")) %>% 
    mutate(lead2_party =
             case_when(str_detect(lead2_party_AP, "1") ~ "AP",
                       str_detect(lead2_party_ARM, "1") ~ "ARM",
                       str_detect(lead2_party_CS, "1") ~"CS",
                       str_detect(lead2_party_EA, "1") ~"EA",
                       str_detect(lead2_party_PODEMOS, "1") ~"PODEMOS",
                       str_detect(lead2_party_PP, "1") ~ "PP",
                       str_detect(lead2_party_PSOE, "1") ~ "PSOE",
                       str_detect(lead2_party_UCD, "1") ~"UCD",
                       str_detect(lead2_party_UP, "1") ~"UP",
                       str_detect(lead2_party_VOX, "1") ~"VOX",
                       TRUE ~ "OTRAS")) %>% 
    mutate(gov_pre =
             case_when(str_detect(gov_pre_PP, "1") ~ "PP",
                       str_detect(gov_pre_PSOE, "1") ~ "PSOE",
                       str_detect(gov_pre_UCD, "1") ~"UCD",
                       TRUE ~ "OTRAS"))
  
  
  eval_test_red_party2 <- eval_test_red_party %>% select("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                                         "n",  "est_surv_vote", "prom_general_partido", "prom_general_wing", 
                                                         "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido", 
                                                         "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing", 
                                                         "house_effect_e", "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                                         "urna_60", "urna_365", 
                                                         "errores", "party", "poll_firm", 
                                                         "lead_party", "lead2_party", "gov_pre", "error", "real_vote", "est_real_vote") 
  
  semma_id <-
    semma %>% 
    mutate(id_fin =
             glue("{year_elec}_{n_days_field}_{days_to_elec}_{n}_{party}_{poll_firm}")) 
  semma_id <-
    semma_id %>% 
    mutate(id_fin = as.character(id_fin))
  
  eval_test_red_party2 <-
    eval_test_red_party2 %>% 
    mutate(id_fin =
             glue("{year_elec}_{n_days_field}_{days_to_elec}_{n}_{party}_{poll_firm}")) 
  
  eval_test_red_party2 <-
    eval_test_red_party2 %>% 
    mutate(id_fin = as.character(id_fin))
  
  eval_test_red_party3 <- sqldf('
      SELECT a.* 
           , b.wing
           , b.date_elec
           , b.id_semma
      FROM eval_test_red_party2  AS a
      LEFT JOIN (
                SELECT *
                FROM semma_id ) AS b
            ON   (a.id_fin = b.id_fin)
            ')
  
  eval_test_red_party3 <-
    eval_test_red_party3 %>% 
    mutate(wing =
             case_when(str_detect(party, "VOX")|                                                                            
                         str_detect(party, "UCD")|
                         str_detect(party, "FN")|
                         str_detect(party, "CIU")|
                         str_detect(party, "CDS")|
                         str_detect(party, "CC")|
                         str_detect(party, "AP")|
                         str_detect(party, "cs")|
                         str_detect(party, "PP") ~ "RIGHT",
                       TRUE ~ "LEFT"))
  
  eval_test_red_party3 <-
    eval_test_red_party3 %>% 
    mutate(date_elec =
             case_when(str_detect(year_elec, "1982") ~ "1982-10-28",                                                                        
                       str_detect(year_elec, "1986") ~ "1986-06-22",
                       str_detect(year_elec, "1989") ~ "1989-10-29",
                       str_detect(year_elec, "1993") ~ "1993-06-06",
                       str_detect(year_elec, "1996") ~ "1996-03-03",
                       str_detect(year_elec, "2000") ~ "2000-03-12",
                       str_detect(year_elec, "2004") ~ "2004-03-14",
                       str_detect(year_elec, "2008") ~ "2008-03-09",
                       str_detect(year_elec, "2011") ~ "2011-11-20",
                       str_detect(year_elec, "2015") ~ "2015-12-20",
                       str_detect(year_elec, "2016") ~ "2016-06-26",
                       str_detect(date_elec, "2019-04-28 02:00:00") ~ "2019-04-28",
                       str_detect(date_elec, "2019-11-10 01:00:00") ~ "2019-11-10",
                       TRUE ~ "NA"))
  
  
  # MEDIA POR PARTY Y CARRERA: ¿Medias de las predicciones = predicción del voto real?
  eval_test_red_party <- group_by(eval_test_red_party3, date_elec, party) #OJO con 2019 tenemos que recuprara el date elec
  eval_test_red_party <- summarise(eval_test_red_party, prediccion_de_partido = mean(est_real_vote, na.rm = TRUE))
  eval_test_red_party <- eval_test_red_party %>% filter(!(date_elec == "NA"),)
  # Falta añadir con un join el valor real para hacer la comparativa. 
  eval_test_red_party
  semma_dos <- semma %>% mutate(date_elec2 = as.character(date_elec))
  
  eval_test_red_party <- sqldf('
      SELECT a.* 
           , b.real_vote
           , b.prom_carrera_partido
      FROM eval_test_red_party  AS a
      LEFT JOIN (
                SELECT *
                FROM semma_dos ) AS b
            ON (a.date_elec = b.date_elec2)
            AND (a.party = b.party)
            ')
  eval_test_red_party<-eval_test_red_party[!duplicated(eval_test_red_party), ]
  # install.packages("CGPfunctions")
  library(CGPfunctions)
  # install.packages("ggplot2")
  library(ggplot2)
  
  newggslopegraph(eval_test_red_party, date_elec, real_vote, party,
                  Title = "Evolución del PIB",
                  SubTitle = "1970-1979",
                  Caption =  "Autor: Enric Palau Payeras | Datos: Spanish elections dataset") +
    theme_gray() +
    theme(legend.position = "none")
  
  # install.packages("ggplot2")
  carreras <- split(eval_test_red_party, eval_test_red_party$date_elec)
  eval_test_red_party_2019_11 <-carreras[["2019-11-10"]]
  
  a<-ggplot(eval_test_red_party_2019_11) +
    geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                     y = party, yend = party)) +
    geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
    geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
    theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2019-11-10)")+
    theme(legend.position = "bottom")
  
  eval_test_red_party_2019_04 <-carreras[["2019-04-28"]]
  
  b<-ggplot(eval_test_red_party_2019_04) +
    geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                     y = party, yend = party)) +
    geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
    geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
    theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2019-04-28)")+
    theme(legend.position = "bottom")
  
  eval_test_red_party_2016 <-carreras[["2016-06-26"]]
  
  c<-ggplot(eval_test_red_party_2016) +
    geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                     y = party, yend = party)) +
    geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
    geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
    theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2016-06-26)")+
    theme(legend.position = "bottom")
  
  eval_test_red_party_2015 <-carreras[["2015-12-20"]]
  
  d<-ggplot(eval_test_red_party_2015) +
    geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                     y = party, yend = party)) +
    geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
    geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
    theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2015-12-20)")+
    theme(legend.position = "bottom")
  
  comparativa_test_red <- ggarrange(a, b, c, d,
                                    ncol = 2, nrow = 2)
  comparativa_test_red
  
  library(Metrics)
 
  # PREDICCIONES EN TEST 2023 =========================================================
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
  
  

#### 2.3. Modelos de SVM =======================================================
  # 2.3.1. SVM_lin =============================================================
  # HIPERPARÁMETROS:
  set.seed(1234)
  SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,25,50))
  set.seed(1234)
  SVMgrid<-expand.grid(C=c(0.05,0.1,0.5,1,2))
  
  # DEFINICIÓN DEL ENTRENAMIENTO
  control<-trainControl(method = "cv",
                         number=4,
                         savePredictions = "all") 
  
  # AJUSTE DEL MODELO
  set.seed(1234)
  SVM<- train(data=train_semma,
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
               method="svmLinear",
               trControl=control,
               tuneGrid=SVMgrid,
               verbose=FALSE)
  SVM
  # Support Vector Machines with Linear Kernel 
  # 
  # 7348 samples
  # 39 predictor
  # 
  # No pre-processing
  # Resampling: Cross-Validated (4 fold) 
  # Summary of sample sizes: 5511, 5510, 5511, 5512 
  # Resampling results across tuning parameters:
  #   
  #   C      RMSE      Rsquared   MAE     
  # 0.01  2.825718  0.5919684  1.766938
  # 0.05  2.603875  0.6282868  1.522703
  # 0.10  2.617290  0.6232868  1.520338
  # 0.20  2.630730  0.6195289  1.527233
  # 0.50  2.632220  0.6191702  1.527756
  # 1.00  2.631949  0.6192465  1.527924
  # 2.00  2.632010  0.6192216  1.528430
  # 5.00  2.631798  0.6192753  1.528333
  # 10.00  2.628574  0.6201419  1.526857
  # 25.00  2.642995  0.6165761  1.541029
  # 50.00  3.195304  0.5108930  1.916293
  # 
  # RMSE was used to select the optimal model using the smallest value.
  # The final value used for the model was C = 0.05.
  saveRDS(SVM, "SVM_lin")
  SVM_lin<-readRDS("SVM_lin")
  SVM_lin
  resultados <- SVM_lin[["results"]]
  soluti <- SVM_lin[["pred"]]
  SVM$finalModel
  ggplot(SVM, highlight = TRUE) +
    labs(title = "Evolución del error del modelo SVM") +
    theme_grey()+labs(y = "MAE (Cross-Validation)")
  
  options(scipen=999)
  ggplot(data = resultados,
            mapping = aes(x = C, y= MAE )) +
    geom_point(color = "#006EA1", alpha = 0.5, size = 4) +
    theme_grey()+
    labs(title = "SVM lineal BIC",
         subtitle =
           "C vs MAE",
         x = "C",
         y = "MAE")
  
  b1<-ggplot(data = soluti,
             mapping = aes(x = obs, y= pred )) +
    geom_point(color = "#006EA1", alpha = 0.2, size = 2) +
    # Diagonal
    geom_smooth(method=lm, se=FALSE, color = "blue", size = 1.5) +
    labs(title = "SVM lineal BIC",
         subtitle =
           "Pred vs Obs",
         x = "Price",
         y = "Predicción")
  grid.arrange(a, b1, ncol=2)

  # EVALUAMOS EL MEJOR SVM_lineal ==============================================
  source("cruzada SVM continua lineal.R")
  medias_svm_lin_1<-cruzadaSVM(data=train_semma,
                               vardep="errores", 
                               listconti= 
                                 c("house_effect_e", "party_PODEMOS", "party_UP", 
                                   "est_surv_vote", "prom_carrera_partido", "prom_carrera_wing", 
                                   "party_VOX", "party_PSOE", "party_EA", "gov_pre_PP", "party_CIU",
                                   "party_IU", "party_CC", "party_AP", "pobl_pobreza_rate", "gov_cor_rate",
                                   "party_UPYD", "eco_pib_var", "prom_general_wing", "prom_casa_partido", 
                                   "prom_general_partido", "party_UCD", "party_PCE", "pobl_suicide",
                                   "eco_unployement", "party_HB", "eco_fisc_ing_percap", "pobl_life_expectancy",
                                   "pobl_densidad", "days_to_elec", "party_BNG", "poll_firm_ASEP",
                                   "poll_firm_METROSCOPIA", "party_PA", "party_PNV", "party_EE", 
                                   "lead2_party_UP", "poll_firm_SOCIOMÉTRICA", "poll_firm_ELECTOPANEL"),
                               listclass= c(""), 
                               grupos=4,
                               sinicio=1234,
                               repe=10,
                               C=0.05)
  medias_svm_lin_1$modelo="svm_lin_1"
  saveRDS(medias_svm_lin_1, "svm_lin_1")
  medias_svm_lin_1<-readRDS("svm_lin_1")
  
  medias_svm_lin_2<-cruzadaSVM(data=train_semma,
                               vardep="errores", 
                               listconti= 
                                 c("house_effect_e", "party_PODEMOS", "party_UP", 
                                   "est_surv_vote", "prom_carrera_partido", "prom_carrera_wing", 
                                   "party_VOX", "party_PSOE", "party_EA", "gov_pre_PP", "party_CIU",
                                   "party_IU", "party_CC", "party_AP", "pobl_pobreza_rate", "gov_cor_rate",
                                   "party_UPYD", "eco_pib_var", "prom_general_wing", "prom_casa_partido", 
                                   "prom_general_partido", "party_UCD", "party_PCE", "pobl_suicide",
                                   "eco_unployement", "party_HB", "eco_fisc_ing_percap", "pobl_life_expectancy",
                                   "pobl_densidad", "days_to_elec", "party_BNG", "poll_firm_ASEP",
                                   "poll_firm_METROSCOPIA", "party_PA", "party_PNV", "party_EE", 
                                   "lead2_party_UP", "poll_firm_SOCIOMÉTRICA", "poll_firm_ELECTOPANEL"),
                               listclass= c(""), 
                               grupos=4,
                               sinicio=1234,
                               repe=10,
                               C=0.5)
  medias_svm_lin_2$modelo="svm_lin_2"
  saveRDS(medias_svm_lin_2, "svm_lin_2")
  medias_svm_lin_2<-readRDS("svm_lin_2")
  
  medias_svm_lin_3<-cruzadaSVM(data=train_semma,
                               vardep="errores", 
                               listconti= 
                                 c("house_effect_e", "party_PODEMOS", "party_UP", 
                                   "est_surv_vote", "prom_carrera_partido", "prom_carrera_wing", 
                                   "party_VOX", "party_PSOE", "party_EA", "gov_pre_PP", "party_CIU",
                                   "party_IU", "party_CC", "party_AP", "pobl_pobreza_rate", "gov_cor_rate",
                                   "party_UPYD", "eco_pib_var", "prom_general_wing", "prom_casa_partido", 
                                   "prom_general_partido", "party_UCD", "party_PCE", "pobl_suicide",
                                   "eco_unployement", "party_HB", "eco_fisc_ing_percap", "pobl_life_expectancy",
                                   "pobl_densidad", "days_to_elec", "party_BNG", "poll_firm_ASEP",
                                   "poll_firm_METROSCOPIA", "party_PA", "party_PNV", "party_EE", 
                                   "lead2_party_UP", "poll_firm_SOCIOMÉTRICA", "poll_firm_ELECTOPANEL"),
                               listclass= c(""), 
                               grupos=4,
                               sinicio=1234,
                               repe=10,
                               C=10)
  medias_svm_lin_3$modelo="svm_lin_3"
  saveRDS(medias_svm_lin_3, "svm_lin_3")

  union1<-rbind(medias_svm_lin_1, medias_svm_lin_2, medias_svm_lin_3)
  par(cex.axis=0.8)
  union1$error<-sqrt(union1$error)
  par(cex.axis=1.2)
  boxplot(data=union1,col="turquoise",error~modelo)
  ggplot(union1, aes(x=modelo, y=error, fill=modelo)) +
    geom_boxplot(outlier.colour="black", outlier.shape=1,
                 outlier.size=2) +
    labs(x = "Modelos de SVM_lin", y = 'MAE', title = "Boxplot vc repetida SVM_lin")
  
  # 2.3.1. SVM_pol grado 2 =============================================================
  # # HIPERPARÁMETROS:
  SVMgrid<-expand.grid(C=c(0.05,0.5,1,10),
                      degree=c(2),
                      scale=c(0.01,0.1,1,5))
  # DEFINICIÓN DEL ENTRENAMIENTO
  control<-trainControl(method = "cv",
                       number=4,
                       savePredictions = "all") 
  # AJUSTE DEL MODELO
  set.seed(1234)
  SVM_pol<- train(data=train_semma,
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
  
  SVM_pol
  # Support Vector Machines with Polynomial Kernel 
  # 
  # 7348 samples
  # 39 predictor
  # 
  # No pre-processing
  # Resampling: Cross-Validated (4 fold) 
  # Summary of sample sizes: 5511, 5510, 5511, 5512 
  # Resampling results across tuning parameters:
  #   
  #   C      scale  RMSE       Rsquared   MAE      
  # 0.05  0.01   3.2068100  0.4958499  1.9367796
  # 0.05  0.10   1.2403312  0.9160668  0.6382454
  # 0.05  1.00   0.7988247  0.9552514  0.2917578
  # 0.05  5.00   0.8866632  0.9414356  0.2588986
  # 0.50  0.01   1.7897843  0.8464778  1.0411037
  # 0.50  0.10   0.8394161  0.9609497  0.3896123
  # 0.50  1.00   0.8151849  0.9491350  0.3122355
  # 0.50  5.00   0.8718272  0.9430281  0.2574096
  # 1.00  0.01   1.4307191  0.8921926  0.7726714
  # 1.00  0.10   0.8170166  0.9630974  0.3691345
  # 1.00  1.00   0.8153097  0.9492110  0.3138559
  # 1.00  5.00   0.8718272  0.9430281  0.2574096
  # 10.00  0.01   1.1127480  0.9316583  0.5081767
  # 10.00  0.10   0.7617397  0.9586992  0.3961186
  # 10.00  1.00   0.8147818  0.9492668  0.3133670
  # 10.00  5.00   0.8718272  0.9430281  0.2574096
  # 
  # Tuning parameter 'degree' was held constant at a value of 2
  # RMSE was used to select the optimal model using the smallest value.
  # The final values used for the model were degree = 2, scale = 0.1 and C = 10.
  
  saveRDS(SVM_pol, "svm_pol")
  svm_pol<-readRDS("svm_pol")
  
  dat<-as.data.frame(svm_pol$results)
  # REPRESENTACIÓN GRÁFICA
  ggplot(dat, aes(x=factor(C), 
                  y=MAE, 
                  color=factor(degree),
                  pch=factor(scale))) +
    geom_point(position=position_dodge(width=0.5),size=3)
  
  
  
  
  # 2.3.3. SVM_pol grado 3 =============================================================
  # HIPERPARÁMETROS:
  SVMgrid<-expand.grid(C=c(0.5, 10),
                      degree=c(2,3),
                      scale=c(0.1, 1, 5))
  # DEFINICIÓN DEL ENTRENAMIENTO
  control<-trainControl(method = "cv",
                        number=4,
                        savePredictions = "all") 
  # AJUSTE DEL MODELO
  set.seed(1234)
  SVM_pol_3<- train(data=train_semma,
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
  
  SVM_pol_3
  saveRDS(SVM_pol_3, "SVM_pol_3")
  SVM_pol_3<-readRDS("SVM_pol_3")
  SVM_pol_3$results
  dat<-as.data.frame(SVM_pol_3$results)
  # REPRESENTACIÓN GRÁFICA
  ggplot(dat, aes(x=factor(C), 
                  y=MAE, 
                  color=factor(degree),
                  pch=factor(scale))) +
    geom_point(position=position_dodge(width=0.5),size=3)
  dat<-as.data.frame(SVM3$results)
  # PLOT DE DOS VARIABLES CATEGÓRICAS, UNA CONTINUA
  ggplot(dat, aes(x=factor(C),
                  y=RMSE,
                  color=factor(degree),
                  pch=factor(scale))) +
    geom_point(position=position_dodge(width=0.5),size=3)
  
  # EVALUAMOS EL MEJOR SVM ==============================================
  source("cruzada SVM continua lineal.R")
  medias_svm_lin_1<-cruzadaSVM(data=train_semma,
                               vardep="errores", 
                               listconti= 
                                 c("house_effect_e", "party_PODEMOS", "party_UP", 
                                   "est_surv_vote", "prom_carrera_partido", "prom_carrera_wing", 
                                   "party_VOX", "party_PSOE", "party_EA", "gov_pre_PP", "party_CIU",
                                   "party_IU", "party_CC", "party_AP", "pobl_pobreza_rate", "gov_cor_rate",
                                   "party_UPYD", "eco_pib_var", "prom_general_wing", "prom_casa_partido", 
                                   "prom_general_partido", "party_UCD", "party_PCE", "pobl_suicide",
                                   "eco_unployement", "party_HB", "eco_fisc_ing_percap", "pobl_life_expectancy",
                                   "pobl_densidad", "days_to_elec", "party_BNG", "poll_firm_ASEP",
                                   "poll_firm_METROSCOPIA", "party_PA", "party_PNV", "party_EE", 
                                   "lead2_party_UP", "poll_firm_SOCIOMÉTRICA", "poll_firm_ELECTOPANEL"),
                               listclass= c(""), 
                               grupos=4,
                               sinicio=1234,
                               repe=10,
                               C=0.5)
  medias_svm_lin_1$modelo="svm_lin_1"
  saveRDS(medias_svm_lin_1, "svm_lin_1")
  medias_svm_lin_1<-readRDS("svm_lin_1")
  
  medias_svm_lin_2<-cruzadaSVM(data=train_semma,
                               vardep="errores", 
                               listconti= 
                                 c("house_effect_e", "party_PODEMOS", "party_UP", 
                                   "est_surv_vote", "prom_carrera_partido", "prom_carrera_wing", 
                                   "party_VOX", "party_PSOE", "party_EA", "gov_pre_PP", "party_CIU",
                                   "party_IU", "party_CC", "party_AP", "pobl_pobreza_rate", "gov_cor_rate",
                                   "party_UPYD", "eco_pib_var", "prom_general_wing", "prom_casa_partido", 
                                   "prom_general_partido", "party_UCD", "party_PCE", "pobl_suicide",
                                   "eco_unployement", "party_HB", "eco_fisc_ing_percap", "pobl_life_expectancy",
                                   "pobl_densidad", "days_to_elec", "party_BNG", "poll_firm_ASEP",
                                   "poll_firm_METROSCOPIA", "party_PA", "party_PNV", "party_EE", 
                                   "lead2_party_UP", "poll_firm_SOCIOMÉTRICA", "poll_firm_ELECTOPANEL"),
                               listclass= c(""), 
                               grupos=4,
                               sinicio=1234,
                               repe=10,
                               C=0.5)
  medias_svm_lin_2$modelo="svm_lin_2"
  saveRDS(medias_svm_lin_2, "svm_lin_2")
  medias_svm_lin_2<-readRDS("svm_lin_2")
  
  union1<-rbind(medias_svm_lin_1, medias_svm_lin_2)
  par(cex.axis=0.8)
  union1$error<-sqrt(union1$error)
  par(cex.axis=1.2)
  boxplot(data=union1,col="turquoise",error~modelo)
  
  detach(package:plyr)
  source("cruzada SVM continua polinomial.R")
  medias_svm_pol_1<-cruzadaSVMpoly(data=train_semma,
                               vardep="errores", 
                               listconti=  c("house_effect_e", "party_PODEMOS", "party_UP", 
                                             "est_surv_vote", "prom_carrera_partido", "prom_carrera_wing", 
                                             "party_VOX", "party_PSOE", "party_EA", "gov_pre_PP", "party_CIU",
                                             "party_IU", "party_CC", "party_AP", "pobl_pobreza_rate", "gov_cor_rate",
                                             "party_UPYD", "eco_pib_var", "prom_general_wing", "prom_casa_partido", 
                                             "prom_general_partido", "party_UCD", "party_PCE", "pobl_suicide",
                                             "eco_unployement", "party_HB", "eco_fisc_ing_percap", "pobl_life_expectancy",
                                             "pobl_densidad", "days_to_elec", "party_BNG", "poll_firm_ASEP",
                                             "poll_firm_METROSCOPIA", "party_PA", "party_PNV", "party_EE", 
                                             "lead2_party_UP", "poll_firm_SOCIOMÉTRICA", "poll_firm_ELECTOPANEL"),
                               listclass= c(""), 
                               grupos=4,
                               sinicio=1234,
                               repe=10,
                               C= 0.5,
                               degree=3,
                               scale=1)
  medias_svm_pol_1$modelo="svm_pol_1"
  saveRDS(medias_svm_pol_1, "svm_pol_1")
  medias_svm_pol_1<-readRDS("svm_pol_1")
  
  
  medias_svm_pol_2<-cruzadaSVMpoly(data=train_semma,
                               vardep="errores", 
                               listconti= 
                                 c("house_effect_e", "party_PODEMOS", "party_UP", 
                                   "est_surv_vote", "prom_carrera_partido", "prom_carrera_wing", 
                                   "party_VOX", "party_PSOE", "party_EA", "gov_pre_PP", "party_CIU",
                                   "party_IU", "party_CC", "party_AP", "pobl_pobreza_rate", "gov_cor_rate",
                                   "party_UPYD", "eco_pib_var", "prom_general_wing", "prom_casa_partido", 
                                   "prom_general_partido", "party_UCD", "party_PCE", "pobl_suicide",
                                   "eco_unployement", "party_HB", "eco_fisc_ing_percap", "pobl_life_expectancy",
                                   "pobl_densidad", "days_to_elec", "party_BNG", "poll_firm_ASEP",
                                   "poll_firm_METROSCOPIA", "party_PA", "party_PNV", "party_EE", 
                                   "lead2_party_UP", "poll_firm_SOCIOMÉTRICA", "poll_firm_ELECTOPANEL"),
                               listclass= c(""), 
                               grupos=4,
                               sinicio=1234,
                               repe=10,
                               C=0.5,
                               degree=3,
                               scale=5)
  medias_svm_pol_2$modelo="svm_pol_2"
  saveRDS(medias_svm_pol_2, "svm_pol_2")
  medias_svm_pol_2<-readRDS("svm_pol_2")
  
  medias_svm_pol_3<-cruzadaSVMpoly(data=train_semma,
                               vardep="errores", 
                               listconti= 
                                 c("house_effect_e", "party_PODEMOS", "party_UP", 
                                   "est_surv_vote", "prom_carrera_partido", "prom_carrera_wing", 
                                   "party_VOX", "party_PSOE", "party_EA", "gov_pre_PP", "party_CIU",
                                   "party_IU", "party_CC", "party_AP", "pobl_pobreza_rate", "gov_cor_rate",
                                   "party_UPYD", "eco_pib_var", "prom_general_wing", "prom_casa_partido", 
                                   "prom_general_partido", "party_UCD", "party_PCE", "pobl_suicide",
                                   "eco_unployement", "party_HB", "eco_fisc_ing_percap", "pobl_life_expectancy",
                                   "pobl_densidad", "days_to_elec", "party_BNG", "poll_firm_ASEP",
                                   "poll_firm_METROSCOPIA", "party_PA", "party_PNV", "party_EE", 
                                   "lead2_party_UP", "poll_firm_SOCIOMÉTRICA", "poll_firm_ELECTOPANEL"),
                               listclass= c(""), 
                               grupos=4,
                               sinicio=1234,
                               repe=10,
                               C=10,
                               degree=3,
                               scale=0.1)
  medias_svm_pol_3$modelo="svm_pol_3"
  saveRDS(medias_svm_pol_3, "svm_pol_3")
  medias_svm_pol_3<-readRDS("svm_pol_3")
  
  medias_svm_pol_4<-cruzadaSVMpoly(data=train_semma,
                               vardep="errores", 
                               listconti= 
                                 c("house_effect_e", "party_PODEMOS", "party_UP", 
                                   "est_surv_vote", "prom_carrera_partido", "prom_carrera_wing", 
                                   "party_VOX", "party_PSOE", "party_EA", "gov_pre_PP", "party_CIU",
                                   "party_IU", "party_CC", "party_AP", "pobl_pobreza_rate", "gov_cor_rate",
                                   "party_UPYD", "eco_pib_var", "prom_general_wing", "prom_casa_partido", 
                                   "prom_general_partido", "party_UCD", "party_PCE", "pobl_suicide",
                                   "eco_unployement", "party_HB", "eco_fisc_ing_percap", "pobl_life_expectancy",
                                   "pobl_densidad", "days_to_elec", "party_BNG", "poll_firm_ASEP",
                                   "poll_firm_METROSCOPIA", "party_PA", "party_PNV", "party_EE", 
                                   "lead2_party_UP", "poll_firm_SOCIOMÉTRICA", "poll_firm_ELECTOPANEL"),
                               listclass= c(""), 
                               grupos=4,
                               sinicio=1234,
                               repe=10,
                               C=10,
                               degree=3,
                               scale=1)
  medias_svm_pol_4$modelo="svm_pol_4"
  saveRDS(medias_svm_pol_4, "svm_pol_4")
  medias_svm_pol_4<-readRDS("svm_pol_4")
  
  medias_svm_pol_5<-cruzadaSVMpoly(data=train_semma,
                               vardep="errores", 
                               listconti= 
                                 c("house_effect_e", "party_PODEMOS", "party_UP", 
                                   "est_surv_vote", "prom_carrera_partido", "prom_carrera_wing", 
                                   "party_VOX", "party_PSOE", "party_EA", "gov_pre_PP", "party_CIU",
                                   "party_IU", "party_CC", "party_AP", "pobl_pobreza_rate", "gov_cor_rate",
                                   "party_UPYD", "eco_pib_var", "prom_general_wing", "prom_casa_partido", 
                                   "prom_general_partido", "party_UCD", "party_PCE", "pobl_suicide",
                                   "eco_unployement", "party_HB", "eco_fisc_ing_percap", "pobl_life_expectancy",
                                   "pobl_densidad", "days_to_elec", "party_BNG", "poll_firm_ASEP",
                                   "poll_firm_METROSCOPIA", "party_PA", "party_PNV", "party_EE", 
                                   "lead2_party_UP", "poll_firm_SOCIOMÉTRICA", "poll_firm_ELECTOPANEL"),
                               listclass= c(""), 
                               grupos=4,
                               sinicio=1234,
                               repe=10,
                               C=0.5,
                               degree=2,
                               scale=5)
  medias_svm_pol_5$modelo="svm_pol_5"
  saveRDS(medias_svm_pol_5, "svm_pol_5")
  medias_svm_pol_5<-readRDS("svm_pol_5")
  
  union1<-rbind(medias_svm_pol_1, medias_svm_pol_2, medias_svm_pol_3, medias_svm_pol_4, medias_svm_pol_5)
  par(cex.axis=0.8)
  par(cex.axis=1.2)
  boxplot(data=union1,col="turquoise",error~modelo)
  union1<-rbind(medias_svm_pol_1, medias_svm_pol_2, medias_svm_pol_3, medias_svm_pol_4, medias_svm_pol_5)
  b<-ggplot(union1, aes(x=modelo, y=error, fill=modelo)) +
    geom_boxplot(outlier.colour="black", outlier.shape=1,
                 outlier.size=2) +
    labs(x = "Modelos de SVM polinomial", y = 'MAE', title = "Boxplot vc repetida SVM")+
    theme_grey() + theme(legend.position = "bottom", legend.direction = "horizontal")
  union2<-rbind(medias_svm_lin_1, medias_svm_lin_2, medias_svm_lin_3)
  a<-ggplot(union2, aes(x=modelo, y=error, fill=modelo)) +
    geom_boxplot(outlier.colour="black", outlier.shape=1,
                 outlier.size=2) +
    labs(x = "Modelos de SVM lineal", y = 'MAE', title = "Boxplot vc repetida SVM")+
    theme_grey() + theme(legend.position = "bottom", legend.direction = "horizontal")
  comparativa_gbm <- ggarrange(b, a,
                               ncol = 2, nrow = 1)
  comparativa_gbm


  
  # CONCLUSIÓN MEJOR SVM =======================================================
  # Concluimos con el mejor SVM
  best<-rbind(medias_svm_lin_1, medias_svm_pol_1, medias_svm_pol_3, medias_svm_pol_4)
  par(cex.axis=1)
  boxplot(data=best, error~modelo) #Las observaciones fuera del boxplot (circulos) son errores outlier, o mejor dicho, errores muy anómalos para el modelo.
  best$error<-sqrt(best$error)
  #   cp     RMSE  Rsquared      MAE     RMSESD RsquaredSD      MAESD
  # 1  0 1.896974 0.8015885 1.098803 0.08314654 0.01508187 0.03734614
  ggplot(best, aes(x=modelo, y=error, fill=modelo)) +
    geom_boxplot(outlier.colour="black", outlier.shape=1,
                 outlier.size=2) +
    labs(x = "Modelos de árbol", y = 'MAE', title = "Boxplot vc repetida árboles")
  
  # CONCLUSIÓN MEJOR ARBOL : Predicciones en test
  # PREDICCIONES EN TEST =========================================================
    # HIPERPARÁMETROS:
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
  
  # CONCLUSIÓN SOBRE HISTÓRICO, MEJOR SVM ====================================
  eval_test_SVM_party <- eval_test_SVM %>% #reagrupar partits y casas 
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
    mutate(lead_party =
             case_when(str_detect(lead_party_CS, "1") ~ "CS",
                       str_detect(lead_party_PODEMOS, "1") ~ "PODEMOS",
                       str_detect(lead_party_PP, "1") ~"PP",
                       str_detect(lead_party_PSOE, "1") ~"PSOE",
                       str_detect(lead_party_UCD, "1") ~"UCD",
                       TRUE ~ "OTRAS")) %>% 
    mutate(lead2_party =
             case_when(str_detect(lead2_party_AP, "1") ~ "AP",
                       str_detect(lead2_party_ARM, "1") ~ "ARM",
                       str_detect(lead2_party_CS, "1") ~"CS",
                       str_detect(lead2_party_EA, "1") ~"EA",
                       str_detect(lead2_party_PODEMOS, "1") ~"PODEMOS",
                       str_detect(lead2_party_PP, "1") ~ "PP",
                       str_detect(lead2_party_PSOE, "1") ~ "PSOE",
                       str_detect(lead2_party_UCD, "1") ~"UCD",
                       str_detect(lead2_party_UP, "1") ~"UP",
                       str_detect(lead2_party_VOX, "1") ~"VOX",
                       TRUE ~ "OTRAS")) %>% 
    mutate(gov_pre =
             case_when(str_detect(gov_pre_PP, "1") ~ "PP",
                       str_detect(gov_pre_PSOE, "1") ~ "PSOE",
                       str_detect(gov_pre_UCD, "1") ~"UCD",
                       TRUE ~ "OTRAS"))
  
  
  eval_test_SVM_party2 <- eval_test_SVM_party %>% select("year_elec", "n_days_field", "days_to_elec", "porc_surveys_firm", 
                                                             "n",  "est_surv_vote", "prom_general_partido", "prom_general_wing", 
                                                             "prom_casa_partido", "prom_casa_wing", "prom_carrera_partido", 
                                                             "prom_carrera_wing", "prom_carrera_casa_partido", "prom_carrera_casa_wing", 
                                                             "house_effect_e", "wing_effect_e", "urna_0", "urna_7", "urna_15", 
                                                             "urna_60", "urna_365", 
                                                             "errores", "party", "poll_firm", 
                                                             "lead_party", "lead2_party", "gov_pre", "error", "real_vote", "est_real_vote") 
  
  semma_id <-
    semma %>% 
    mutate(id_fin =
             glue("{year_elec}_{n_days_field}_{days_to_elec}_{n}_{party}_{poll_firm}")) 
  semma_id <-
    semma_id %>% 
    mutate(id_fin = as.character(id_fin))
  
  eval_test_SVM_party2 <-
    eval_test_SVM_party2 %>% 
    mutate(id_fin =
             glue("{year_elec}_{n_days_field}_{days_to_elec}_{n}_{party}_{poll_firm}")) 
  
  eval_test_SVM_party2 <-
    eval_test_SVM_party2 %>% 
    mutate(id_fin = as.character(id_fin))
  
  eval_test_SVM_party3 <- sqldf('
      SELECT a.* 
           , b.wing
           , b.date_elec
           , b.id_semma
      FROM eval_test_SVM_party2  AS a
      LEFT JOIN (
                SELECT *
                FROM semma_id ) AS b
            ON   (a.id_fin = b.id_fin)
            ')
  
  eval_test_SVM_party3 <-
    eval_test_SVM_party3 %>% 
    mutate(wing =
             case_when(str_detect(party, "VOX")|                                                                            
                         str_detect(party, "UCD")|
                         str_detect(party, "FN")|
                         str_detect(party, "CIU")|
                         str_detect(party, "CDS")|
                         str_detect(party, "CC")|
                         str_detect(party, "AP")|
                         str_detect(party, "cs")|
                         str_detect(party, "PP") ~ "RIGHT",
                       TRUE ~ "LEFT"))
  
  eval_test_SVM_party3 <-
    eval_test_SVM_party3 %>% 
    mutate(date_elec =
             case_when(str_detect(year_elec, "1982") ~ "1982-10-28",                                                                        
                       str_detect(year_elec, "1986") ~ "1986-06-22",
                       str_detect(year_elec, "1989") ~ "1989-10-29",
                       str_detect(year_elec, "1993") ~ "1993-06-06",
                       str_detect(year_elec, "1996") ~ "1996-03-03",
                       str_detect(year_elec, "2000") ~ "2000-03-12",
                       str_detect(year_elec, "2004") ~ "2004-03-14",
                       str_detect(year_elec, "2008") ~ "2008-03-09",
                       str_detect(year_elec, "2011") ~ "2011-11-20",
                       str_detect(year_elec, "2015") ~ "2015-12-20",
                       str_detect(year_elec, "2016") ~ "2016-06-26",
                       str_detect(date_elec, "2019-04-28 02:00:00") ~ "2019-04-28",
                       str_detect(date_elec, "2019-11-10 01:00:00") ~ "2019-11-10",
                       TRUE ~ "NA"))
  
  
  # MEDIA POR PARTY Y CARRERA: ¿Medias de las predicciones = predicción del voto real?
  eval_test_SVM_party <- group_by(eval_test_SVM_party3, date_elec, party)  
  write.csv()
  eval_test_SVM_party <- summarise(eval_test_SVM_party, prediccion_de_partido = mean(est_real_vote, na.rm = TRUE))
  eval_test_SVM_party <- eval_test_SVM_party %>% filter(!(date_elec == "NA"),)
  # Falta añadir con un join el valor real para hacer la comparativa. 
  eval_test_SVM_party
  semma_dos <- semma %>% mutate(date_elec2 = as.character(date_elec))
  
  eval_test_SVM_party <- sqldf('
      SELECT a.* 
           , b.real_vote
           , b.prom_carrera_partido
      FROM eval_test_SVM_party  AS a
      LEFT JOIN (
                SELECT *
                FROM semma_dos ) AS b
            ON (a.date_elec = b.date_elec2)
            AND (a.party = b.party)
            ')
  eval_test_SVM_party<-eval_test_SVM_party[!duplicated(eval_test_SVM_party), ]
  saveRDS(eval_test_SVM_party, "eval_test_SVM_party")
  # install.packages("CGPfunctions")
  library(CGPfunctions)
  # install.packages("ggplot2")
  library(ggplot2)
  
  newggslopegraph(eval_test_SVM_party, date_elec, prediccion_de_partido, party,
                  Title = "Evolución del voto en España",
                  SubTitle = "1982-2023",
                  Caption =  "Autor: Enric Palau Payeras | Datos: Spanish elections dataset") +
    theme_gray() +
    theme(legend.position = "none")
  
  # install.packages("ggplot2")
  carreras <- split(eval_test_SVM_party, eval_test_SVM_party$date_elec)
  eval_test_SVM_party_2019_11 <-carreras[["2019-11-10"]]
  
  a<-ggplot(eval_test_SVM_party_2019_11) +
    geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                     y = party, yend = party)) +
    geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
    geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
    theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2019-11-10)")+
    theme(legend.position = "bottom")
  
  eval_test_SVM_party_2019_04 <-carreras[["2019-04-28"]]
  
  b<-ggplot(eval_test_SVM_party_2019_04) +
    geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                     y = party, yend = party)) +
    geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
    geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
    theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2019-04-28)")+
    theme(legend.position = "bottom")
  
  eval_test_SVM_party_2016 <-carreras[["2016-06-26"]]
  
  c<-ggplot(eval_test_SVM_party_2016) +
    geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                     y = party, yend = party)) +
    geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
    geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
    theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2016-06-26)")+
    theme(legend.position = "bottom")
  
  eval_test_SVM_party_2015 <-carreras[["2015-12-20"]]
  
  d<-ggplot(eval_test_SVM_party_2015) +
    geom_segment(aes(x = prediccion_de_partido, xend = real_vote,
                     y = party, yend = party)) +
    geom_point(aes(x = prediccion_de_partido, y = party), size = 4, color = "indianred3", alpha = 0.7) +
    geom_point(aes(x = real_vote, y = party), size = 4, color = "cornflowerblue", alpha = 0.7)+
    theme_grey()+labs(x = "Predicciones (rojo) vs  Observaciones (azul); (2015-12-20)")+
    theme(legend.position = "bottom")
  
  comparativa_test_SVM <- ggarrange(a, b, c, d,
                                      ncol = 2, nrow = 2)
  comparativa_test_SVM
  
  
  # PREDICCIONES EN TEST 2023 =========================================================
  # Predecimos en test con el modelo seleccionado
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