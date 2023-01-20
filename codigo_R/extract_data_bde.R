
# ----- SERIES TRIMESTRALES ----- 

# Tasa de desempleados
unemploy_rate <-
  bde_ind_unemployment_rate(series_label =
                              "Unemployment_Rate") %>% 
  rename(date = Date, unemploy_rate = Unemployment_Rate)

# Población
pop <-
  bde_ind_population(series_label = "Population_Spain") %>% 
  rename(date = Date, pop = Population_Spain) %>% 
  # Pasamos a escala normal
  mutate(pop = pop * 1000)

# Producto interior bruto
gdp <-
  bde_ind_gdp_var(series_label = "GDP_YoY") %>% 
  rename(date = Date, gdp = GDP_YoY)

# Juntamos
trimonth_bde_data <- 
  unemploy_rate %>%
  left_join(pop, by = "date") %>% 
  left_join(gdp, by = "date") %>% 
  # Filtramos fechas
  filter(date >= as_date("1980-01-01"))

# ----- SERIES MENSUALES ----- 

# índice de precios al consumidor (IPC)
cpi <-
  bde_ind_cpi_var(series_label = "Consumer_price_index_YoY") %>% 
  rename(date = Date, cpi = Consumer_price_index_YoY)

# Rendimiento medio del IBEX
ibex <-
  bde_ind_ibex(series_label = "IBEX_index_month") %>% 
  rename(date = Date, ibex = IBEX_index_month)

# Juntamos
month_bde_data <- 
  cpi %>%
  left_join(ibex, by = "date") %>% 
  # Filtramos fechas
  filter(date >= as_date("1980-01-01"))

# ----- EXPORTAMOS -----
write_csv(trimonth_bde_data, "./EXPORTADO/trimonth_bde_data.csv")
write_csv(month_bde_data, "./EXPORTADO/month_bde_data.csv")


