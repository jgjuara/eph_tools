#install.packages('tabulizer')

library(tabulizer)
library(tidyverse)
library(stringr)

#####-areas-####
areas <-
  list(
    NULL,
    NULL,
    c(
      top = 224.3687340697,
      left = 165.2863175192,
      bottom = 575.65477862969,
      right = 431.53006005406
    )
    ,
    c(
      top = 177.28916108743,
      left = 159.85277175319,
      bottom = 555.73649775258,
      right = 404.36233122397
    )
    ,
    c(
      top = 180.91066670145,
      left = 150.79686214316,
      bottom = 515.89993599835,
      right = 411.607058912
    )
    ,
    c(
      top = 179.09991389444,
      left = 147.17449829915,
      bottom = 479.68487985815,
      right = 417.04060467802
    )
    ,
    c(
      top = 168.23539705238,
      left = 158.04158983118,
      bottom = 486.92789108619,
      right = 417.04060467802
    )
    ,
    c(
      top = 171.8569026664,
      left = 156.23040790917,
      bottom = 515.89993599835,
      right = 415.22942275601
    )
    ,
    c(
      top = 173.66765547341,
      left = 147.17449829915,
      bottom = 465.19885740207,
      right = 411.607058912
    )
    ,
    c(
      top = 175.47840828042,
      left = 147.17449829915,
      bottom = 479.68487985815,
      right = 411.607058912
    )
    ,
    c(
      top = 177.28916108743,
      left = 150.79686214316,
      bottom = 497.79240792825,
      right = 426.09651428804
    )
    ,
    c(
      top = 182.72141950846,
      left = 136.30740676711,
      bottom = 436.2268124899,
      right = 418.85178660002
    )
    ,
    c(
      top = 179.09991389444,
      left = 138.11858868912,
      bottom = 523.1429472264,
      right = 409.79587698999
    )
    ,
    c(
      top = 171.8569026664,
      left = 150.79686214316,
      bottom = 467.00961020908,
      right = 400.73996737996
    )
    ,
    c(
      top = 175.47840828042,
      left = 143.55213445513,
      bottom = 485.11713827918,
      right = 413.418240834
    )
    ,
    c(
      top = 177.28916108743,
      left = 130.87386100109,
      bottom = 463.38810459506,
      right = 417.04060467802
    )
    ,
    c(
      top = 179.09991389444,
      left = 138.11858868912,
      bottom = 477.87412705114,
      right = 431.53006005406
    )
    ,
    c(
      top = 177.28916108743,
      left = 141.74095253313,
      bottom = 499.60316073526,
      right = 420.66296852203
    )
    ,
    c(
      top = 175.47840828042,
      left = 147.17449829915,
      bottom = 499.60316073526,
      right = 411.607058912
    )
    ,
    c(
      top = 177.28916108743,
      left = 134.4962248451,
      bottom = 510.46767757732,
      right = 406.17351314598
    )
    ,
    c(
      top = 177.28916108743,
      left = 134.4962248451,
      bottom = 409.06552038475,
      right = 418.85178660002
    )
    ,
    c(
      top = 179.09991389444,
      left = 138.11858868912,
      bottom = 505.03541915629,
      right = 413.418240834
    )
    ,
    c(
      top = 180.91066670145,
      left = 145.36331637714,
      bottom = 494.17090231423,
      right = 424.28533236604
    )
    ,
    c(
      top = 180.91066670145,
      left = 147.17449829915,
      bottom = 459.76659898104,
      right = 420.66296852203
    )
    ,
    c(
      top = 182.72141950846,
      left = 147.17449829915,
      bottom = 483.30638547217,
      right = 400.73996737996
    )
    ,
    c(
      top = 177.28916108743,
      left = 134.4962248451,
      bottom = 488.7386438932,
      right = 411.607058912
    )
    ,
    c(
      top = 182.72141950846,
      left = 147.17449829915,
      bottom = 532.19671126145,
      right = 415.22942275601
    )
    ,
    c(
      top = 182.72141950846,
      left = 112.76204178103,
      bottom = 490.54939670021,
      right = 411.607058912
    )
    ,
    c(
      top = 171.8569026664,
      left = 156.23040790917,
      bottom = 439.84831810392,
      right = 406.17351314598
    )
    ,
    c(
      top = 179.09991389444,
      left = 139.92977061112,
      bottom = 506.8461719633,
      right = 409.79587698999
    )
    ,
    c(
      top = 173.66765547341,
      left = 150.79686214316,
      bottom = 581.08703705073,
      right = 418.85178660002
    )
    ,
    c(
      top = 179.09991389444,
      left = 152.60804406516,
      bottom = 499.60316073526,
      right = 404.36233122397
    )
    ,
    c(
      top = 182.72141950846,
      left = 152.60804406516,
      bottom = 463.38810459506,
      right = 397.11760353595
    )
    ,
    c(
      top = 168.23539705238,
      left = 143.55213445513,
      bottom = 448.90208213897,
      right = 407.98469506799
    )
    ,
    c(
      top = 186.34292512248,
      left = 139.92977061112,
      bottom = 430.79455406887,
      right = 415.22942275601
    )
    ,
    c(
      top = 177.28916108743,
      left = 132.6850429231,
      bottom = 340.25691371836,
      right = 418.85178660002
    )
  )




#####

path <- "https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_errores_muestreo.pdf"

raw_tables <- extract_areas(path)

raw_text <- extract_text(path)

aglomerados <- str_extract_all(raw_text, 'Aglomerado.*(?=\n)')[[1]]

aglomerados <- c('Total 31 aglomerados urbanos',aglomerados[2:34])

raw_tables <- areas[3:36]

errores_muestrales <- tibble(aglomerados) %>%
  mutate(table = raw_tables[row_number()])

errores_muestrales_clean <- errores_muestrales

for (i in seq_along(errores_muestrales$table)) {
  errores_muestrales_clean$table[i] <- list(data.frame(errores_muestrales_clean$table[i]))
}


errores_muestrales_clean <- errores_muestrales_clean %>%
  unnest(cols = table)

errores_muestrales_clean <- errores_muestrales_clean %>%
  mutate(x = as.character(X1) %>% as.numeric(),
         ds = as.character(X2) %>% as.numeric(),
         cv = as.character(X3) %>%
           str_replace_all(pattern = ",", replacement = "\\.") %>%
           as.numeric()
  ) %>%
  select(1,5:7)

write_csv(errores_muestrales_clean,  file = "~/errores_muestrales_2003_2014.csv")
