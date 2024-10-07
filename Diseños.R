library(agricolae)
# Cargar librerías necesarias
if(!require(agricolae)) install.packages("agricolae", dependencies=TRUE)
library(agricolae)

# Definir los tratamientos (dosis de fertilización nitrogenada en kg/ha)
tratamientos <- c(0, 50, 100)

# Número de repeticiones por tratamiento
repeticiones <- 5

# Crear el diseño experimental (Diseño Completamente Aleatorizado - DCA)
dca <- design.crd(tratamientos, repeticiones, seed = 123)

# Mostrar el diseño generado
print(dca$book)

# Simular datos de rendimiento o respuesta (puedes reemplazar esta parte con tus propios datos)
set.seed(123)
respuesta <- rnorm(15, mean = rep(c(20, 30, 40), each = repeticiones), sd = 5)

# Añadir la columna de respuesta al diseño
dca$book$rendimiento <- respuesta

# Mostrar los datos finales
print(dca$book)

# Realizar ANOVA
modelo <- aov(rendimiento ~ factor(tratamientos), data = dca$book)
summary(modelo)

# Realizar test de comparación de medias (Tukey)
comparacion <- HSD.test(modelo, "factor(tratamientos)", group=TRUE)
print(comparacion)


# DBCA ---------------------------------------------------------------------
# Cargar librerías necesarias
if (!require(agricolae)) install.packages("agricolae", dependencies = TRUE)
library(agricolae)

# Definir los niveles de los factores
dosis <- c(0, 50, 100)  # Dosis de fertilización nitrogenada (kg/ha)
cultivares <- c("Peruanita", "Canchán")  # Cultivares de papa

# Número de repeticiones (bloques)
repeticiones <- 4

# Crear el diseño experimental (Diseño en Bloques Completos Aleatorizados - DBCA)
dbca <- design.rcbd(trt = interaction(dosis, cultivares), r = repeticiones, seed = 123)

# Mostrar el diseño generado
print(dbca$book)

# Simular datos de rendimiento (puedes reemplazar esto con tus propios datos reales)
set.seed(123)
respuesta <- rnorm(24, mean = rep(c(30, 40, 50, 60, 70, 80), each = repeticiones), sd = 5)

# Añadir la columna de respuesta al diseño
dbca$book$rendimiento <- respuesta

# Asegurarse de que los factores están correctamente definidos
dbca$book$dosis <- factor(dbca$book$trt[, 1])
dbca$book$cultivar <- factor(dbca$book$trt[, 2])

# Mostrar los datos finales
print(dbca$book)

# Realizar ANOVA
modelo <- aov(rendimiento ~ dosis * cultivar + block, data = dbca$book)
summary(modelo)

# Realizar test de comparación de medias (Tukey)
comparacion_dosis <- HSD.test(modelo, "dosis", group = TRUE)
comparacion_cultivares <- HSD.test(modelo, "cultivar", group = TRUE)

# Imprimir los resultados de las comparaciones
print(comparacion_dosis)
print(comparacion_cultivares)


# DBCA --------------------------------------------------------------------
# Definir los factores
fertilizacion <- c(0, 50, 100)  # Dosis de fertilización nitrogenada (kg/ha)
cultivares <- c("Peruanita", "Canchan")  # Cultivares de papa

# Crear una combinación de todos los tratamientos
tratamientos <- expand.grid(Fertilizacion = fertilizacion, Cultivar = cultivares)

# Definir el número de bloques (repeticiones)
repeticiones <- 4

# Crear el diseño en bloques completamente al azar (DBCA)
diseño_dbca <- design.ab(rcbd, treatments=list(fertilizacion, cultivares), r=repeticiones, serie=2)

# Mostrar el diseño experimental
print(diseño_dbca$book)

# Graficar el diseño experimental
plot(diseño_dbca)

# DBCA --------------------------------------------------------------------
# DBCA --------------------------------------------------------------------

library(agricolae)
library(tidyverse)

# fertilizante: 0, 50, 100
# cultivar: canchan y peruanita
trt<-c(3, 2) # factorial 3x2


outdesign <-design.ab(trt, r=4, design = "rcbd")
book<-outdesign$book

book %>% str()

ds <- book %>% 
  mutate(ferti = case_when(
    A %in% 1 ~ "0"
    ,  A %in% 2 ~ "50"
    ,  A %in% 3 ~ "100"
  )) %>% 
  mutate(cultivar = case_when(
    B %in% 1 ~ "canchan"
    ,  B %in% 2 ~ "peruanita"
  )) 

