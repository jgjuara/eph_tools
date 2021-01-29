closest_error_eph <- function(valor, aglomerado = 1, busqueda = "cv", periodo = 201403) {
    #aglomerado = #lista de aglomerados
    #busqueda = #CV o #DS
    #periodo = #2003-2do trim 2014 ó #3er trim 2014 y posterior
    tablaReferencia <- errores_eph %>% 
        filter(codigo == aglomerado & periodo == periodo & variable == busqueda)
    tablaReferencia[["value"]][which.min(abs(tablaReferencia[["estimacion"]]-valor))]
}

CVz <- function(CVx,CVy) round(sqrt((CVx)^2+(CVy)^2),2) # tal que Z = Y/X 
#calculo el CV de la tasa como la raiz cuadrada de 
# la suma de los cuadrados de los coeficientes de variacion de las estimaciones que componen la tasa
# según el manual de error de la EPH.
DSz <- function(z,cv) round(z*cv/100,2) # tal que cv sea el coeficiente de variacion de Z
#calculo la DS de la tasa como el producto entre la tasa de actividad y su CV, redondeo a 2 digits

# para toda tasa Z = Y/X*100
error_tasasEPH <- function(num,den) {
    cvx <- closest_error_eph(num)
    cvy <- closest_error_eph(den)
    z = 100*num/den
    se <- DSz(z,CVz(cvx, cvy))
    print(paste("Tasa",round(z,2),"\n Error Estandar",se))
}

