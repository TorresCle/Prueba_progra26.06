######################################################################################################
######################################## EXAMEN PARCIAL ##############################################
######################################################################################################
library(tidyverse)
library(ggplot2)

############################################ PARTE 1 #################################################
######################################################################################################

# PREGUNTA N°1
##############
variable <- function(h, x = 85.4) {
  if (h >= 1000 & h < 3000) {
    p <- x  - 2*(h %/% 500)
  } else if (h >= 3000 & h < 4000) {
    p <- x - 0.5 * (h %/% 500)
  } else if (h >= 4000) {
    p <- x
  } else {
    print("valor no considerado")
  }

  return(p)
}

variable(1000)

# PREGUNTA N°2
##############
mat01 <- matrix(c(3, 2, -2, 2, -1, 3, 1, 4, 2), nrow = 3, ncol = 3, byrow = T)
mat02 <- matrix(c(0, 9, -4), nrow = 3, ncol = 1)

solve(mat01, mat02)
# a=2, b=-2, c=1


############################################ PARTE 2 #################################################
######################################################################################################
setwd("C:/Users/Jose Luis/Desktop/PROGRAMACION II/MATERIAL DOCENTE/ProgramacionR-master/data")

# PREGUNTA N°1  -------------- Travbajaremos con la Cuenca Atico
################################################################

# a) ####################----------------------------------------------------------------
(cuenca <- read_csv("mods_clima_uh.csv") %>%
  filter(uh_name == "Cuenca Tumbes", bh_esc == "Observado") %>%
  summarise( # Tabla resumen
    pp = sum(bh_pc, na.rm = T), # realizamos la pp acumulada
  ))
  
# Los datos extraidos mediante el filtro al conjunto de datos para la cuenta Atico y los escenarios
# observados nos muestran que la data corresponde a un solo año, sin datos faltantes ni missing values.
# Por ende se precide de realizar la comprobacion de la cantidad de datos y existencia de valores NA.

# La precipitación observada anual acumulada para la cuenca atico es de 16mm, ello nos indica
# que se trata de una zona seca.

#SEGUNDO MÉTODO - FIORELLA
cuenca01 <- read_csv("mods_clima_uh.csv")
cuenca01 %>% 
  filter(uh_name == "Cuenca Tumbes") %>%                       
  group_by(bh_esc) %>% 
  summarise(bh_pc = sum(bh_pc, na.rm = T))

# b) ####################----------------------------------------------------------------
grafico <- read_csv("mods_clima_uh.csv") %>%
  filter(uh_name == "Cuenca Tumbes") %>%
  select(bh_month, everything()) %>%
  mutate(bh_month = sprintf("%1$02d", bh_month)) # Convertidos los meses a numeros de dos dígitos


table(grafico$bh_esc)
modelos <- c("ACCESS 1.0", "HadGEM2-ES", "MPI-ESM-LR", "Observado")

tabla <- data.frame(month.abb)

for (x in 1:4) {
  a <- grafico %>%
    filter(bh_esc == modelos[x]) %>%
    select(bh_pc) # %>%rename(oso = bh_pc)
  tabla <- cbind(tabla, a)
}

colnames(tabla) <- c("meses", "ACCESS 1.0", "HadGEM2-ES", "MPI-ESM-LR", "Observado")

# Estouy encontrando el error relativo porque no encuentro la formula del sesgo
tabla %>% mutate(
  ses_ACCESS = abs(Observado - `ACCESS 1.0`) * 100 / Observado,
  SES_HADGEM = abs(Observado - `HadGEM2-ES`) * 100 / Observado,
  ses_MPI = abs(Observado - `MPI-ESM-LR`) * 100 / Observado
)

# install.packages("hydroGOF")
# library(hydroGOF)
# tabla %>% mutate(
#   ses_ACCESS = pbias(`ACCESS 1.0`,Observado,na.rm = T),
#   SES_HADGEM = pbias(`HadGEM2-ES`,Observado,na.rm = T), 
#   ses_MPI = pbias(`MPI-ESM-LR`,Observado,na.rm = T) 
# )

#promedio y mas un gráfico.


# d) ####################----------------------------------------------------------------
grafico <- read_csv("mods_clima_uh.csv") %>%
  filter(uh_name == "Cuenca Tumbes") %>%
  select(bh_month, everything()) %>%
  mutate(bh_month = sprintf("%1$02d", bh_month))
# Convertidos los meses a numeros de dos dígitos


table(grafico$bh_esc)
# Mediante este filtro observamos que tenemos 3 modelos mas la precipitación observada en nuestra cuenca.
# cada una de ella con un año de dato generados.

ggplot(data = grafico) +
  geom_bar(stat = "identity", fill = "#048ABF", aes(x = bh_month, y = bh_pc)) +
  scale_x_discrete(
    labels = month.abb
  ) +
  labs(y = "precipitación (mm)", x = "meses") +
  facet_wrap(~bh_esc, nrow = 2)

#Plotear5 por lineas juntas de los modelos.

############################################ PARTE 3 #################################################
######################################################################################################

# PREGUNTA N°1  -------------- Travbajaremos con la estación qc00000746
#######################################################################
data <-
  read_csv("temperatureDataset.csv") %>%
  select(DATE, all_of("qc00000746")) %>%
  mutate(DATE = as.Date(DATE, format = "%d/%m/%Y")) %>%
  rename(tmp = all_of("qc00000746")) %>%
  mutate(tmp = ifelse(tmp == -99.9, NA, tmp)) %>%
  arrange(DATE) 

tail(data) # con esto podre saber la fecha final(la inicial lo podemos obtener solo llamando al archivo data).
# mediante una Secuencia diaria comprombamos si la cantidadd de elementos en el dataframe estan completos.
seq(as.Date("1928-11-02"), as.Date("2015-10-31"), by = "day") %>%
  length()

# CINCIDE CON LA CANTIDAD DEL DATAFRAME, EXCELENTE ...!!!!

# a) ####################----------------------------------------------------------------
cantidad_na <- function(a, b) {
  a <- as.character(a)
  b <- as.character(b)

  data %>%
    filter(DATE >= a & DATE < b) %>%
    mutate(
      cantidad_NA = sum(is.na(tmp)) # indicrá nos numeros de NA en un nuevo campo llamado missval(en porcentaje). la funcion n me arroja el numero de dias.
    ) %>%
    summarise(
      cantidad_NA = unique(cantidad_NA)
    )
}

cantidad_na("1983-09-01", "1984-08-31")
cantidad_na("1997-09-01", "1998-08-31")

# b) ####################----------------------------------------------------------------
(temp_mensual <-
  data %>%
  group_by(DATE = str_sub(DATE, 1, 7)) %>%
  mutate(
    valores_NA = sum(is.na(tmp)) * 100 / n() # indicrá nos numeros de NA en un nuevo campo llamado missval(en porcentaje). la funcion n me arroja el numero de dias.
  ) %>%
  summarise( # Tabla resumen
    tmp = mean(tmp, na.rm = T), # realizamos la tmp acumulada
    valores_NA = unique(valores_NA), # Genera un unico valor, la funcion unique me devuelve valores únicos
  ) %>%
  mutate(
    tmp = ifelse(valores_NA > 5, NA, tmp),
    DATE = as.Date(sprintf("%1$s-01", DATE)),
  ))
View(temp_mensual)
# Ploteamos los valores obtenidos.
ggplot(temp_mensual, aes(DATE, tmp)) +
  geom_line(color = "darkblue") +
  labs(x = "Años analizados", y = "Temperatura") +
  ggtitle("Temperatura mensual") +
  theme(
    plot.title = element_text(vjust = 2, hjust = 0.5),
    axis.title.x = element_text(vjust = -0.5),
    axis.title.y = element_text(vjust = 2.5)
  )

# Mejor vista de los valores obtenidos.
ggplot(temp_mensual %>% filter(DATE >= "1964-01-01" & DATE < "2015-01-01"), aes(DATE, tmp)) +
  geom_line(color = "darkblue") +
  labs(x = "Años analizados", y = "Temperatura") +
  ggtitle("Temperatura mensual") +
  theme(
    plot.title = element_text(vjust = 2, hjust = 0.5),
    axis.title.x = element_text(vjust = -0.5),
    axis.title.y = element_text(vjust = 2.5)
  )


# c) ####################----------------------------------------------------------------
(NA_count03 <-
  data %>%
  filter(DATE >= "2005-01-01" & DATE < "2010-12-31") %>%
  group_by(DATE = str_sub(DATE, 1, 7)) %>%
  mutate(
    valores_NA = sum(is.na(tmp)) * 100 / n() # indicrá nos numeros de NA en un nuevo campo llamado missval(en porcentaje). la funcion n me arroja el numero de dias.
  ) %>%
  summarise( # Tabla resumen
    tmp = mean(tmp, na.rm = T), # realizamos la tmp acumulada
    valores_NA = unique(valores_NA), # Genera un unico valor, la funcion unique me devuelve valores únicos
  ) %>%
  mutate(
    valores_NA = ifelse(valores_NA > 5, NA, "-")
  ))

# en el ejemplo vemos que ningun mes supera ni el 1% de valores con presencia de missing values.
# Por ende la temperatura calculada resulta confiable

# d) ####################----------------------------------------------------------------
climatologia <- function(x, y) {
  x <- as.character(x)
  y <- as.character(y)

  data %>%
    filter(DATE >= x & DATE < y) %>%
    group_by(DATE = str_sub(DATE, 6, 7)) %>%
    summarise( # Tabla resumen
      tmp = mean(tmp, na.rm = T), # realizamos la tmp acumulada
    ) %>%
    mutate(periodo = sprintf("periodo %1$s-%2$s", x = str_sub(x, 1, 4), y = str_sub(y, 1, 4)))
}

data01 <- climatologia("1980-01-01", "1995-12-31")
data02 <- climatologia("1996-01-01", "2010-12-31")

ploteo <- rbind(data01, data02)

ggplot(data = ploteo) +
  geom_bar(stat = "identity", fill = "#048ABF", aes(x = DATE, y = tmp)) +
  labs(y = "Temperatura", x = "meses") +
  facet_wrap(~periodo, nrow = 2) +
  scale_x_discrete(
    labels = month.abb
  )

#En un solo gráfico de líneas.

# Para esta situación vemos que los valores promedios de temperatura mensual en ambos casos no varía mucho
# distribuyendose en la mayoría de casos entre valores de 28 y 25 grados.

# e) ####################----------------------------------------------------------------
grafico02 <- data %>%
  filter(DATE >= "1980-01-01" & DATE < "2013-12-31") %>%
  group_by(DATE = str_sub(DATE, 6, 7))

ggplot(grafico02, aes(DATE, tmp)) +
  geom_boxplot(fill = rainbow(12)) +
  scale_x_discrete(
    labels = month.abb
  ) +
  ggtitle("Variabilidad de valores mensuales de temperatura - Periodo: 1980-2013") +
  theme(plot.title = element_text(vjust = 2, hjust = 0.5)) +
  labs(x = "meses", y = "Temperatura")

# El boxplot nos muestra la distribucion de datos de temperatura por meses, los cuales se encuantran distribuidos
# entre valores de 25 a 29 grados, así mismo se observan datos que no coinciden con la tendencia como
# puntos que sobresalen de los gráficos de cajas. La mayor varianza se observa en el mes de febrero.
