####- DATA -####

# prueba con eph
tabla <- eph::get_microdata(year = 2020, trimester = 1:4, vars = c("ANO4", "TRIMESTRE","ESTADO", "PONDERA")) %>%
  unnest() %>% 
  mutate(periodo = paste(ANO4, TRIMESTRE)) %>% 
  eph::organize_labels() %>%
  calculate_tabulates(x = "periodo", 
                      y = "ESTADO",
                      weights = "PONDERA")



####- FUNCIONES -####
# para toda tasa Z = X/Y*100
calculate_rates <- function(numerador, denominador, codigo_aglo, periodo) {
  calculate_cv_rate <- function(cvx,cvy) {sqrt((cvx)^2 + (cvy)^2)} # tal que Z = X/Y 
  # calculo el CV de la tasa z como la raiz cuadrada de la suma de los cuadrados de 
  # los coeficientes de variacion de las estimaciones que componen la tasa z
  # según el manual de error de la EPH.
  
  calculate_ds_rate <- function(z,cv) {z * cv / 100} 
  # tal que cv sea el coeficiente de variacion de Z
  #calculo la DS de la tasa como el producto entre la tasa y su CV
  
  cvx <- calculate_errors(numerador, codigo_aglo, periodo)
  cvy <- calculate_errors(denominador, codigo_aglo, periodo)
  z <- 100 * numerador / denominador
  se <- calculate_ds_rate(z,calculate_cv_rate(cvx, cvy))
  
  tibble(tasa = z, stderror = se)
}

limites <- function(tabulado, puntaje_z) {
  Li = tabulado$tasa - tabulado$stderror*puntaje_z
  Ls = tabulado$tasa + tabulado$stderror*puntaje_z
  
  tibble(tasa = tabulado$tasa, Li, Ls)
}


####- PRUEBAS -####

tabla %>%
  dplyr::mutate(ds_ocupado = calculate_errors(Ocupado, periodo_eph = "2014.03", measure = "ds"),
                ds_desocupado = calculate_errors(Desocupado, measure = "ds"))

tabla %>% 
  cbind(calculate_rates(tabla$Desocupado, tabla$Ocupado + tabla$Desocupado))

tabla %>% 
  select(`periodo/ESTADO`, Ocupado, Desocupado) %>% 
  cbind(limites(calculate_rates(tabla$Desocupado, tabla$Ocupado + tabla$Desocupado),
        puntaje_z =  2)) 



