#'Calculo del desvío estándar y el coeficiente de variación
#'@description
#'Asigna a una estimación de personas el desvío estándar o el coeficiente de variación
#' correspondientes en base a la tabla de errores muestrales de INDEC para EPH continua desde el tercer trimestre de 2014.
#'@param value Valor o vector de valores de las estimaciones puntuales para las
#' que se desea hallar el desvío estándar o el coeficiente de variación
#'@param codigo_aglo default = "Total". String con el código numerico de aglomerado que
#' corresponde a la estimación o con "Total" para el total de 31 aglomerados urbanos (ver \code{\link{errores_muestrales}})
#'@param measure default = "cv". String con "cv" para obtener el coeficiente de variación
#' correspondiente a las estimaciones o con "ds" para obtener el desvío estándar. 
#'@details disclaimer: El script no es un producto oficial de INDEC.
#'
#'@examples
#'
#' tabla <- eph::toybase_individual_2016_03 %>%
#'   eph::organize_labels() %>%
#'   eph::calculate_tabulates(x = "CH03",
#'                          weights = "PONDERA",
#'                          add.totals = "row")
#'### hallar el coeficiente de variación de una lista de frecuencias
#' calculate_error(tabla$Freq, measure = "cv")
#'
#'### hallar el desvio estándar para la columna de frecuencias
#' tabla %>%
#'   mutate(ds = calculate_error(Freq, measure = "ds"))
#'
#'### usando el parametro codigo_aglo
#'
#'tabla <- eph::toybase_individual_2016_03 %>%
#'   eph::organize_labels() %>%
#'   filter(AGLOMERADO == 32)
#'   eph::calculate_tabulates(x = "CH03",
#'                          weights = "PONDERA",
#'                          add.totals = "row")
#'tabla %>% 
#'   mutate(ds = calculate_error(Freq, measure = "ds", codigo_aglo = "32"))

#'@export


calculate_error <- function(value, codigo_aglo = "Total", measure = "cv") {
  tabla_referencia <- errores_muestrales %>%
    dplyr::filter(aglomerado == codigo_aglo)  %>%
    select(x,measure)
  
  find_closest <-function(y) {
    tabla_referencia[[measure]][which.min(abs(tabla_referencia[["x"]] - y))]
  }
  
  sapply(value, find_closest)
}