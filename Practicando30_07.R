nstall.packages("dslabs")
install.packages("reader")
library(dslabs)
library(rvest)
library(stringr)
library(readr)
library(tidyverse)
url <- paste0("https://en.wikipedia.org/w/index.php?title=",
              "Gun_violence_in_the_United_States_by_state",
              "&direction=prev&oldid=810166167")
murders_raw <- read_html(url) %>%
  html_node("table") %>%
  html_table() %>%
  setNames(c("state", "population", "total", "murder_rate"))


data(murders)
murders_raw$population[1:3]
as.numeric(murders_raw$population[1:3])

commas <- function(x) any(str_detect(x, ","))

murders_raw %>% dplyr::summarise_all(commas)

test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)

test_2 <- readr::parse_number(murders_raw$population)
identical(test_1, test_2)
murders_new <- murders_raw %>% mutate_at(2:3, parse_number)






###############################################################################

data(reported_heights)
head(reported_heights)
class(reported_heights$height)

numero <- as.numeric(reported_heights$height)
is.na(numero)
sum(is.na(numero))


nuevo <- reported_heights %>% 
  dplyr::mutate(new_height = as.numeric(height)) %>% 
  dplyr::filter(is.na(new_height)) %>% 
  head(10)




#################################################################################
#################################################################################
#PROBLEMA DE PRECIPITACION
library(dplyr)

Datos <- read_csv("https://raw.githubusercontent.com/ryali93/ProgramacionR/master/data/raingaugeDataset.csv")

datos_estacion <- Datos %>% 
  dplyr::filter(!is.na(qc00000132))

datos_esta <- Datos %>% 
  dplyr::select(date, qc00000132) %>%
  dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  dplyr::rename(pp = qc00000132) %>% 
  dplyr::arrange(date)

pp_mensual <- datos_esta %>% 
  group_by(date = str_sub(date, 1, 7)) %>%
  dplyr::mutate(missVal = (sum(is.na(pp)) * 100) / n()) %>% 
  dplyr::summarise(
    pp = sum(pp, na.rm = T), 
    missVal = unique(missVal)
  ) %>% 
  dplyr::mutate(
    pp = ifelse(missVal >= 10, NA, pp),
    date = as.Date(sprintf("%1$s-01", date))
  )

view(pp_mensual)
#verificando al serie de tiempo
seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by = "day") %>% length()
#para saber el numero de dias

##################################################################################
##################################################################################
##################################################################################
#PRACTICANDO

estaciones <- read_csv("https://raw.githubusercontent.com/ryali93/ProgramacionR/master/data/listRaingauge.csv")

estaciones_extrac <- estaciones %>%
  dplyr::filter(NOM_EST == "SAN MIGUEL") %>% 
  dplyr::select(CODIGO) %>%
  as.character()
  

Datos

datos_estaciones <- Datos %>%
  dplyr::select(date, all_of(estaciones_extrac)) %>% 
  dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% 
  dplyr::rename(pp = qc00000247) %>% 
  dplyr::arrange()

#comprobando el numero de datos que se tiene
tail(datos_estaciones)
seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by = "day") %>% length()
  
#Cantidad de misiing values a paso diario

missing_values <- datos_estaciones %>% 
  dplyr::mutate(
    missing_pp = sum(is.na(pp))
  ) %>% 
  dplyr::summarise(
    missing_pp = unique(missing_pp)
  )

#Cantidad de precipitacion mensual que no superoe el 10% de missing values

pp_mensual02 <- datos_estaciones %>% 
  group_by(date = str_sub(date, 1, 7)) %>% 
  dplyr::mutate(
    valores_NA = sum(is.na(pp))*100/n()
  ) %>% 
  dplyr::summarise(
    pp = sum(pp, na.rm = T),
    valores_NA = unique(valores_NA)
  ) %>% 
  dplyr::mutate(
    pp = ifelse(valores_NA >= 10, NA, pp),
    date = as.Date(sprintf("%1$s-01", date)),
    meses = str_sub(date, 6, 7)
  )

library(ggplot2)
#Graficando
ggplot(pp_mensual02, aes(date, pp)) +
  geom_line(color = "blue")

pp_mensual02$valores_NA %>% table()

view(pp_mensual02)

sprintf("%1$s-02", "2980-03")

sprintf("2000-%1$s-01", "1980")


view(missing_values)

missing_values$missing_pp %>% table()


library(tidyverse)

# For better printing
iris <- as_tibble(iris)
mtcars <- as_tibble(mtcars)

names()

cle01 <- iris %>% pivot_longer(everything())
cle <- mtcars %>% pivot_longer(mtcars, mpg)
view(cle)


############################################################################################################################
############################################################################################################################
#PIVOT_LONGER, que se puede cambiar de filas a columnas y/o viciversa
relig_income
to <- relig_income %>%
  pivot_longer(!religion, names_to = "income", values_to = "count")

iris
prueba01 <- iris %>%
  pivot_longer(!Species, names_to = "nombres", values_to = "cantidad")
############################################################################################################################
############################################################################################################################

#EVERITHING
iris <- as_tibble(iris)
mtcars <- as_tibble(mtcars)
#usando everithing para seleccionar como se muestra a continuacion
iris %>% select(everything())
#ahora tambien se puede utilizar de la siguinete forma
mtcars %>% pivot_longer(everything())

############################################################################################################################
############################################################################################################################

## be careful with the format: most things in R are floats
## only integer-valued reals get coerced to integer.

sprintf("%s is %f feet tall\n", "Sven", 7.1)      # OK
try(sprintf("%s is %i feet tall\n", "Sven", 7.1)) # not OK
sprintf("%s is %i feet tall\n", "Sven", 7  )  # OK

## use a literal % :

sprintf("%.0f%% said yes (out of a sample of size %.0f)", 66.666, 3)

## various formats of pi :

sprintf("%f", pi)
sprintf("%.3f", pi)
sprintf("%1.0f", pi)
sprintf("%5.1f", pi)
sprintf("%05.1f", pi)
sprintf("%+f", pi)
sprintf("% f", pi)
sprintf("%-10f", pi) # left justified
sprintf("%e", pi)
sprintf("%E", pi)
sprintf("%g", pi)
sprintf("%g",   1e6 * pi) # -> exponential
sprintf("%.9g", 1e6 * pi) # -> "fixed"
sprintf("%G", 1e-6 * pi)

## no truncation:
sprintf("%1.f", 101)

## re-use one argument three times, show difference between %x and %X
xx <- sprintf("%1$d %1$x %1$X", 0:15)
xx <- matrix(xx, dimnames = list(rep("", 16), "%d%x%X"))
noquote(format(xx, justify = "right"))

## More sophisticated:

sprintf("min 10-char string '%10s'",
        c("a", "ABC", "and an even longer one"))

## Platform-dependent bad example from qdapTools 1.0.0:
## may pad with spaces or zeroes.
sprintf("%09s", month.name)

n <- 1:18
sprintf(paste0("e with %2d digits = %.", n, "g"), n, exp(1))

## Using arguments out of order
sprintf("second %2$1.0f, first %1$5.2f, third %3$1.0f", pi, 2, 3)

## Using asterisk for width or precision
sprintf("precision %.*f, width '%*.3f'", 3, pi, 8, pi)

## Asterisk and argument re-use, 'e' example reiterated:
sprintf("e with %1$2d digits = %2$.*1$g", n, exp(1))

## re-cycle arguments
sprintf("%s %d", "test", 1:3)

## binary output showing rounding/representation errors
x <- seq(0, 1.0, 0.1); y <- c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)
cbind(x, sprintf("%a", x), sprintf("%a", y))

############################################################################################################################
############################################################################################################################
#MIS GRAFICOS EN GGPLOT
library(datos)
millas
View(millas)
names(millas)
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, size = cilindros, color = ciudad, shape = clase))

########################_DENTRO DE aes()_############################
#TODOS LOS QUE SE MUESTRAN DEEBEN DE ESTAR DENTRO DE aes()
#color = color de los diferentes clases
#size = tamaño de los puntos
#alpha = transparencia de los puntos
#shape = formas(estetica) de los puntos (ojo solo te grafica 6 clases)

########################_AFUERA DE aes()_############################
#color = si queremos el mismo color para todas las variables 


