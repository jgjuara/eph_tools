calculate_error <- function(value, codigo_aglo = "Total", measure = "cv") {
  tabla_referencia <- errores_muestrales %>%
    dplyr::filter(aglomerado == codigo_aglo)  %>%
    select(x,measure)
  
  find_closest <-function(y) {
    tabla_referencia[[measure]][which.min(abs(tabla_referencia[["x"]] - y))]
  }
  
  sapply(value, find_closest)
}

calculate_cv_rate <- function(cvx,cvy) {sqrt((cvx)^2 + (cvy)^2)} # tal que Z = X/Y 
# calculo el CV de la tasa z como la raiz cuadrada de la suma de los cuadrados de 
# los coeficientes de variacion de las estimaciones que componen la tasa
# según el manual de error de la EPH.

calculate_ds_rate <- function(z,cv) {z * cv / 100} # tal que cv sea el coeficiente de variacion de Z
#calculo la DS de la tasa como el producto entre la tasa y su CV

# para toda tasa Z = X/Y*100
calculate_rate <- function(numerador,denominador) {
  cvx <- calculate_error(numerador)
  cvy <- calculate_error(denominador)
  z <- 100 * numerador / denominador
  se <- calculate_ds_rate(z,calculate_cv_rate(cvx, cvy))
  
  list(tasa = z, stderror = se)
}

limites <- function(tasasEPH, puntaje_z) {
  Li = tasasEPH$tasa - tasasEPH$stderror*puntaje_z
  Ls = tasasEPH$tasa + tasasEPH$stderror*puntaje_z
  
  cat(paste(paste("Tasa Estimada =", round(tasasEPH$tasa,2)),
            paste("Lim. Inf. = ",round(Li,2)),
            paste("Lim. Sup. = ",round(Ls,2)),
            sep = " \n "))
}


# prueba con eph
tabulados <- eph::toybase_individual_2016_03 %>%
  eph::organize_labels() %>% 
  eph:: calculate_tabulates(x = "CH03", weights = "PONDERA", add.totals = "row")

calculate_error(tabulados$Freq, measure = "cv")

tabulados %>% 
  mutate(ds = calculate_error(Freq, measure = "ds"))

eph::toybase_individual_2016_03 %>%
   eph::organize_labels() %>%
   filter(AGLOMERADO == 32) %>% 
   eph::calculate_tabulates(x = "CH03",
                          weights = "PONDERA",
                          add.totals = "row") %>% 
   mutate(ds = calculate_error(Freq, measure = "ds", codigo_aglo = "32"))

