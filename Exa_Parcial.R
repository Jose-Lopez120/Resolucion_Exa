# PARTE 1

# Ejercicio 1
alt <- function(x){  # x valor de la altitud 
  if (x>= 1000 & x<= 3000){ # primera condicion
    v <- 81.4 -(2*((x-1000)/500))
    
  }
  
  if (x>= 3000 & x<= 4000){ # segunda condicion 
    v <- 81.4 - (2*(2000/500)) - (0.5*(x-3000)/500)
  }
  if (x>4000){ #tercera condicion
    v <- 81.4 - (2*(2000/500)) - (0.5*(1000/500))
  }
  return(v)
}

alt(4000)

# Ejercicio 2

a1 <- c(3,2,1)
a2 <- c(2,-1,4)
a3 <- c(-2,3,2) 

cbind(a1,a2,a3) # Matriz tranpuesta
A = cbind(a1,a2,a3) # Matriz de la ecuacion
b1 = c(0,9,-4)
cbind(b1)
B = cbind(b1) # Matriz de las incognitas

solve(A) # Determina la inversa de una matriz
solve(A) %*% B # Producto matricial



## Parte 2

library(tidyverse)

# uh name → Nombre de cuenca
# bh esc → Escenario (observado y modelos climaticos)
# bh month → Meses
# bh pc → Precipitacion
# bh er → Evapotranspiracion Real
# bh rh → Rendimiento Hidrico
# bh qd → Caudal


clima_uh <- read_csv("D:/R/Parte3/mods_clima_uh.csv") # CARGA EL ARCHIVO 


cuenc <- dplyr::filter(clima_uh, uh_name == "Cuenca Tumbes") # FILTRO DE LA CUENCA TUMBES 
View(cuenc)

# a) calcular la precipitacion acumulada anual "valores observados"
cuenc %>% dplyr::filter(bh_esc == "Observado") %>% 
  summarise(sum(bh_pc))

# b) porcentaje de sesgo de los escenarios climaticos respecto a los datos 
# observados para cada mes


sesgo_acces <- cuenc %>% dplyr::filter(bh_esc == "Observado" | bh_esc == "ACCESS 1.0") %>%
  group_by(bh_month) %>% 
  summarise(
    sesg_acces = ((last(bh_pc) - first(bh_pc))/first(bh_pc))*100)

sesgo_hadgem <- cuenc %>% dplyr::filter(bh_esc == "Observado" | bh_esc == "HadGEM2-ES") %>% 
  group_by(bh_month) %>%
  summarise(
    sesg_hadgem = ((last(bh_pc) - first(bh_pc))/first(bh_pc))*100)

sesgo_mpi <- cuenc %>% dplyr::filter(bh_esc == "Observado" | bh_esc == "MPI-ESM-LR") %>% 
  group_by(bh_month) %>%
  summarise(
    sesg_mpi = ((last(bh_pc) - first(bh_pc))/first(bh_pc))*100)

# (Modelo - observados )/ observados * 100

# c) De la pregunta anterior ¿Cuál es el escenario climático más preciso?

# d) Grafícar, con ggplot2, la precipitación (enero a diciembre) observada y modelos climáticos. 

ggplot(data = cuenc) +
  geom_point(mapping = aes(x = bh_month , y = bh_pc) ) + facet_wrap(~ bh_esc,   nrow = 2) + 
  scale_x_continuous("Meses") + 
  scale_y_continuous("Precipitación")
    
## PARTE 3

# a) Determine la cantidad de missing values para los años hidrologicos
# Sep1983-Agos1984 y Sep1997-Agos1998.

data <- read.csv("D:/R/Parte3/temperatureDataset.csv") %>% 
  dplyr::select(DATE, qc00000837) %>%
  mutate(DATE = as.Date(DATE, format = "%d/%m/%Y")) %>% 
  dplyr::rename(temp = qc00000837) %>% 
  arrange(DATE) %>% 
  dplyr::filter(DATE >= "1983-09-01",DATE <= "1984-08-31" | DATE >= "1997-09-01",DATE <= "1998-08-31") %>% 
  mutate(valor = temp) %>% 
  mutate(valor == -99.9, NA) %>% 
  mutate(sumNA = sum(valor == -99.9))

# b) Calcule la serie de tiempo mensual

serie <- data %>% 
  group_by(DATE = str_sub(DATE,1,7)) %>%
  mutate(miss_Val = sum(valor == -99.9)) %>% 
  summarise(temp = mean(temp, na.rm = T)) %>% 
  mutate(min = min_rank(temp))

# No se observo nomalia alguna, ya que sigue su flujo natural de mayores 
# precipitaciones en los meses de (enero-marzo) y las de menores precipitaciones
# en los meses (julio-setiembre)

# c) Determine la cantidad de missing values de la serie de tiempo a paso 
# mensual para los años 2005 y 2010

data2 <- read.csv("D:/R/Parte3/temperatureDataset.csv") %>% 
  dplyr::select(DATE, qc00000837) %>%
  mutate(DATE = as.Date(DATE, format = "%d/%m/%Y")) %>% 
  rename(temp = qc00000837) %>% 
  arrange(DATE) %>% 
  dplyr::filter(DATE >= "2005-01-01",DATE <= "2005-12-31" | DATE >= "2010-01-01",DATE <= "2010-12-31") %>%
  mutate(valor = temp) %>% 
  mutate(valor == -99.9, NA) %>% 
  mutate(sumNA = sum(valor == -99.9)) %>% 
  group_by(DATE = str_sub(DATE,1,7)) %>%
  summarise(miss_Val = sum(valor == -99.9))


# d) Cree una funcion que calcule, apartir de los datos de temperatura mensual,
# la climatologia (Ene-Dic).



p_80_95 <- climatologia(1980,1995) 
p_96_10 <- climatologia(1996,2010)

ggplot() + geom_point( p_80_95,mapping = aes(x = DATE , y = temper), color = "red")+
  geom_point(p_96_10,mapping = aes(x = DATE , y = temper), show.legend = T , color = "blue") + 
  scale_x_discrete(label = month.abb)







# e) Plotear la varibilidad de los valores mensuales (Ene-Dic) para el periodo
# 1980 - 2013 y describirlo

data4 <- read.csv("D:/R/Parte3/temperatureDataset.csv") %>% 
  dplyr::select(DATE, qc00000837) %>%
  mutate(DATE = as.Date(DATE, format = "%d/%m/%Y")) %>% 
  dplyr::rename(temp = qc00000837) %>% 
  arrange(DATE) %>% 
  dplyr::filter(DATE >= "1980-01-01",DATE <= "2013-12-31") %>%
  mutate(tempNA = temp,
         tempNA = na_if(tempNA,-99.9)) %>% 
  group_by(DATE = str_sub(DATE,1,7)) %>%
  mutate(
    miss = sum(is.na(tempNA))
  ) %>% 
  summarise(tempNA = sum(tempNA, na.rm = T),
            miss = unique(miss)
  ) %>% 
  mutate(
    tempNA = ifelse(miss >= 5*31/100, NA, tempNA),
    DATE = as.Date(sprintf("%1$s-01", DATE)),
    month = str_sub(DATE, 6,7)
  )

ggplot(data4, aes(month, tempNA)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(
    labels = month.abb 
  ) +
  scale_x("Meses") + 
  scale_y("Precipitacion")
