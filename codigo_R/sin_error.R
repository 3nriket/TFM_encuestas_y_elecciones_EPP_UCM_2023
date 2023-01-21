# Para la fase de modelos =====================================================
# Eliminamos las varibles que puedan causar sobreajuste
test_2023<-test_2023 %>% filter(!(exit_poll == "TRUE"),)
test_2023$exit_poll<-NULL
test_2023$error_general_partido<-NULL
test_2023$error_general_wing<-NULL
test_2023$error_casa_partido<-NULL
test_2023$error_casa_wing<-NULL
test_2023$error_carrera_partido<-NULL
test_2023$error_carrera_wing<-NULL
test_2023$error_carrera_casa_partido<-NULL
test_2023$error_carrera_casa_wing<-NULL
test_2023$house_effect<-NULL
test_2023$wing_effect<-NULL

test_semma<-test_semma %>% filter(!(exit_poll == "TRUE"),)
test_2023$exit_poll<-NULL
test_semma$error_general_partido<-NULL
test_semma$error_general_wing<-NULL
test_semma$error_casa_partido<-NULL
test_semma$error_casa_wing<-NULL
test_semma$error_carrera_partido<-NULL
test_semma$error_carrera_wing<-NULL
test_semma$error_carrera_casa_partido<-NULL
test_semma$error_carrera_casa_wing<-NULL
test_semma$house_effect<-NULL
test_semma$wing_effect<-NULL

train_semma<-train_semma %>% filter(!(exit_poll == "TRUE"),)
train_semma$exit_poll<-NULL
train_semma$error_general_partido<-NULL
train_semma$error_general_wing<-NULL
train_semma$error_casa_partido<-NULL
train_semma$error_casa_wing<-NULL
train_semma$error_carrera_partido<-NULL
train_semma$error_carrera_wing<-NULL
train_semma$error_carrera_casa_partido<-NULL
train_semma$error_carrera_casa_wing<-NULL
train_semma$house_effect<-NULL
train_semma$wing_effect<-NULL

# Para la fase de modificaciones =====================================================
train_estadisticos$error_general_partido<-NULL
train_estadisticos$error_general_wing<-NULL
train_estadisticos$error_casa_partido<-NULL
train_estadisticos$error_casa_wing<-NULL
train_estadisticos$error_carrera_partido<-NULL
train_estadisticos$error_carrera_wing<-NULL
train_estadisticos$error_carrera_casa_partido<-NULL
train_estadisticos$error_carrera_casa_wing<-NULL
train_estadisticos$house_effect<-NULL
train_estadisticos$wing_effect<-NULL

train_arbol$error_general_partido<-NULL
train_arbol$error_general_wing<-NULL
train_arbol$error_casa_partido<-NULL
train_arbol$error_casa_wing<-NULL
train_arbol$error_carrera_partido<-NULL
train_arbol$error_carrera_wing<-NULL
train_arbol$error_carrera_casa_partido<-NULL
train_arbol$error_carrera_casa_wing<-NULL
train_arbol$house_effect<-NULL
train_arbol$wing_effect<-NULL

test_estadisticos$error_general_partido<-NULL
test_estadisticos$error_general_wing<-NULL
test_estadisticos$error_casa_partido<-NULL
test_estadisticos$error_casa_wing<-NULL
test_estadisticos$error_carrera_partido<-NULL
test_estadisticos$error_carrera_wing<-NULL
test_estadisticos$error_carrera_casa_partido<-NULL
test_estadisticos$error_carrera_casa_wing<-NULL
test_estadisticos$house_effect<-NULL
test_estadisticos$wing_effect<-NULL

test_arbol$error_general_partido<-NULL
test_arbol$error_general_wing<-NULL
test_arbol$error_casa_partido<-NULL
test_arbol$error_casa_wing<-NULL
test_arbol$error_carrera_partido<-NULL
test_arbol$error_carrera_wing<-NULL
test_arbol$error_carrera_casa_partido<-NULL
test_arbol$error_carrera_casa_wing<-NULL
test_arbol$house_effect<-NULL
test_arbol$wing_effect<-NULL