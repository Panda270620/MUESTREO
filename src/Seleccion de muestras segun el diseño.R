#Estructuracion de los datos
set.seed(1234)

library(pacman)
p_load(readr, tidyverse, TeachingSampling)

divipola <- read.csv2("DIVIPOLA.csv")
N <- nrow(divipola)
n <- 25


#---------------------------MAS----------------------------
#----------------------------------------------------------
#Coordenado negativo
#----------------------------------------------------------
  
#1. Asignar coordenada

divipola$zeta <- runif(N, 0, 1)

#2. Ordenar el marco por la variable Z

divipola_ordenada <- divipola[order(divipola$zeta), ] #menor a mayor

#3. Seleccionamos los primeros n

muestra_mas_coordnegativo <- divipola_ordenada[1:n, ]
head(muestra_mas_coordnegativo)

#----------------------------------------------------------
#Fan, Muller y Rezucha
#----------------------------------------------------------

set.seed(1234)
indices <- TeachingSampling::S.SI(N,n)
muestra_mas_fanMuller <- divipola[indices, ]


#---------------------------BER----------------------------
#1. Se le asigna a cada elemento un numero aleatorio de 0 a 1
#2. Determinamos el valor de pi
#3. El criterio de seleccion es que se incluye en la muestra si z_k < pi

pi <- 0.1
muestra_ber <- divipola[divipola$zeta < pi, ]
SupportRS(N)
