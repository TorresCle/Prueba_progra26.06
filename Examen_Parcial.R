###############################################################################
###############################################################################
#PREGUNTA 01
#1.1
funcion <- function(altitud){
  parametro = 81.4
  if (altitud >=1000 & altitud <= 3000){
    resultado = parametro - 2*(altitud - 1000)/500
    
  } else if (altitud > 3000 & altitud <= 4000){
    resultado = parametro - 0.5 * (altitud - 3000)/500
    
  } else {
    resultado = parametro
  }
  resultado
}
funcion(1000)


#1.2
A <- c(3, 2, 1, 2, -1, 4, -2, 3, 2)
B <- c(0, 9, -4)

ecuacion1 <- matrix(A, ncol = 3)
ecuacion2 <- matrix(B, ncol = 1)

solve(ecuacion1, ecuacion2)

################################################################################
################################################################################

library(dplyr)
library(readr)

datos <- read_csv("mods_clima_uh.csv")

datos <- datos %>% 
  filter(
    uh_name == "Cuenca Tumbes"
  ) %>% 
  select(bh_month, everything()) %>% 
  mutate(bh_month = sprintf("%1$02d", bh_month))

View(datos)

#2.a 
observado <- datos %>%
  group_by(bh_esc) %>% 
  summarise(
    bh_pc = sum(bh_pc, na.rm = T)
  ) %>% 
  filter(bh_esc == "Observado")


################################################################################
#2.b

models <- c("ACCESS 1.0", "HadGEM2-ES", "MPI-ESM-LR", "Observado")
model <- data.frame(month.abb)

for (i in 1:4) {
  valor <- datos %>% 
    filter(bh_esc == models[i]) %>% 
    select(bh_pc)
  
  model <- cbind(model, valor)
}
model
colnames(model) <- c("meses", "ACCESS 1.0", "HadGEM2-ES", "MPI-ESM-LR", "Observado")

#Ahora realizamos las el sesgo para cada uno de los modelos

sesgo <- model %>% 
  mutate(
    SESGO_ACCESS = (abs(`ACCESS 1.0` - Observado)/Observado)*100,
    SESGO_HADGEM2 = (abs(`HadGEM2-ES` - Observado)/Observado)*100,
    SESGO_MPI = (abs(`MPI-ESM-LR` - Observado)/Observado)*100
  ) %>% 
  summarise(
    SESGO_ACCESS = mean(SESGO_ACCESS),
    SESGO_HADGEM2 = mean(SESGO_HADGEM2),
    SESGO_MPI = mean(SESGO_MPI)
  )

#Con ello podemos precisar que el tercer modelos es mas preciso (SESGO_MPI)

#3.c
# AL MOMENTO DE EVALUAR ANTERIORMENTE PODEMOS OBSERVAR QUE EL MODLEO MAS PRECISO ES MPI

#3.d realizamos la grafica 

library(ggplot2)
datos

library(ggplot2)
ggplot(datos) +
  geom_bar(mapping = aes(x = bh_month, y = bh_pc), stat = "identity") +
  facet_wrap(~bh_esc) +
  scale_x_discrete(labels = month.abb)
  labs(x = "Meses", y = "Precipitación") 




ggplot(data = datos) +
  geom_bar(stat = "identity", fill = "#048ABF", aes(x = bh_month, y = bh_pc)) +
  scale_x_discrete(
    labels = month.abb
  ) +
  labs(y = "Precipitación", x = "Meses") +
  facet_wrap(~bh_esc, nrow = 2) 
  
ggplot(data = grafico) +
  geom_bar(stat = "identity", fill = "#048ABF", aes(x = bh_month, y = bh_pc)) +
  scale_x_discrete(
    labels = month.abb
  ) +
  labs(y = "precipitación (mm)", x = "meses") +
  facet_wrap(~bh_esc, nrow = 2)



#################################################################################
#################################################################################
#PARTE 03

Data03 <- read_csv("temperatureDataset.csv")

Data_temp <- Data03 %>% 
  dplyr::select(DATE, qc00000746) %>%
  dplyr::mutate(
    DATE = as.Date(DATE, format = "%d/%m/%Y")) %>% 
  dplyr::rename(Temp = qc00000746) %>% 
  dplyr::arrange()

#Comprobando el numero de datos
tail(Data_temp)
seq(as.Date("1928-11-02"), as.Date("2015-10-31"), by = "day") %>% length()

#Es correcto el numero de datos, asi que no tenemos ningun problema en este caso:

#convertimos los -99.9 a NA
Data_temp <- Data_temp %>% 
  dplyr::mutate(
    Temp = ifelse(Temp == -99.9, NA, Temp)
  )

#Cantidad de NA a paso diario para todos los años
NA_diario <- Data_temp %>% 
  dplyr::mutate(
    cantidad_NA = sum(is.na(Temp))
  ) %>% 
  dplyr::summarise(
    cantidad_NA = unique(cantidad_NA)
  )

#Determinamos la cantidad de missing values para sep1983-Agos1984
Sep_Agos <- Data_temp %>% 
  dplyr::filter(DATE>="1983-09-01" & DATE <="1984-08-31") %>% 
  dplyr::mutate(
    cantidad_NA01 = sum(is.na(Temp))
  ) %>% 
  dplyr::summarise(
    cantidad_NA01 = unique(cantidad_NA01)
  )

#Determinamos la cantidad de missing values para sep1997-Agos1998
Sep_Agos01 <- Data_temp %>% 
  dplyr::filter(
    DATE>="1997-09-01" & DATE <="1998-08-31"
  ) %>% 
  dplyr::mutate(
    cantidad_NA02 = sum(is.na(Temp))
  ) %>% 
  dplyr::summarise(
    cantidad_NA02 = unique(cantidad_NA02)
  )

#PASO DE TIEMPO A PASO MENSUAL

Tem_mensual <- Data_temp %>% 
  group_by(
    DATE = str_sub(DATE, 1, 7)
  ) %>% 
  dplyr::mutate(
    valores_NA03 = sum(is.na(Temp))*100/n()
  ) %>% 
  dplyr::summarise(
    Temp = mean(Temp, na.rm = T),
    valores_NA03 = unique(valores_NA03)
  ) %>% 
  dplyr::mutate(
    Temp = ifelse(valores_NA03 >= 5, NA, Temp),
    DATE = as.Date(sprintf("%1$s-01", DATE)),
    meses = str_sub(DATE, 6, 7)
  )

ggplot(Tem_mensual, aes(DATE, Temp)) +
  geom_line(color = "blue")


#Determinamos la cantidad de missing values mensuales
mensual_NA <- Tem_mensual %>%
  dplyr::filter(
    DATE >= "2005-01-01" & DATE <="2010-12-01"
  ) %>% 
  dplyr::mutate(
    valores_NA04 = sum(is.na(Temp))
  ) %>% 
  dplyr::summarise(
    valores_NA04 = unique(valores_NA04)
  )
View(mensual_NA)


#ploteamos la Variabilidad de los valores mensuales para el periodo 1980 a 2013
variabilidad <- Tem_mensual %>% 
  dplyr::filter(
    DATE >="1980-01-01" & DATE <= "2013-12-01"
  )

ggplot(variabilidad, aes(meses, Temp)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(
    labels = month.abb
  )





view(Tem_mensual)
Data_temp$Temp %>% table()
