library(dplyr)

# Establece el directorio de trabajo donde están los archivos CSV
setwd("C:/Users/protu/Downloads/archive")

# Lista todos los archivos CSV en el directorio
archivos <- list.files(pattern = "Power-Networks-LCL-June2015\\(withAcornGps\\)v2_.*\\.csv")

# Leer y combinar todos los archivos en un solo dataframe
mundo <- lapply(archivos, function(x) {
  read.csv(x, header = TRUE, sep = ",", stringsAsFactors = FALSE)
}) %>% bind_rows()
#str(mundo)
#head(mundo)
# Verificar la conversión y contar NAs
sum(is.na(mundo$KWH.hh..per.half.hour.))
# Contar valores faltantes en cada columna
sapply(mundo, function(x) sum(is.na(x)))


# Convertir la columna DateTime a formato POSIXct
mundo$DateTime <- as.POSIXct(mundo$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
# Limpiar la columna KWH.hh..per.half.hour., convertir "Null" y otros caracteres no numéricos a NA
mundo$KWH.hh..per.half.hour. <- as.numeric(gsub("Null", "", gsub("[^0-9\\.]", "", mundo$KWH.hh..per.half.hour., perl = TRUE)))
str(mundo)
head(mundo)

# Verificar la conversión y contar NAs
sum(is.na(mundo$KWH.hh..per.half.hour.))
# Contar valores faltantes en cada columna
sapply(mundo, function(x) sum(is.na(x)))
# Detectar valores no numéricos en una columna que debería ser numérica
sum(grepl("[^0-9\\.]", mundo$KWH.hh..per.half.hour.))
# Verificar los valores que no se pudieron convertir y que se convirtieron en NA
mundo[is.na(mundo$KWH.hh..per.half.hour.), ]


#######################################################################
summary(mundo)

# Calcular el total de valores NA en la columna específica
total_nas <- sum(is.na(mundo$KWH.hh..per.half.hour.))

# Calcular el total de observaciones en la columna
total_observaciones <- nrow(mundo)

# Calcular la proporción de valores NA
proporcion_na <- (total_nas / total_observaciones) * 100

# Definir el umbral para la decisión
umbral <- 5

# Decision sobre la imputación por la mediana
if (proporcion_na <= umbral) {
  message("La proporción de datos faltantes es ", sprintf("%.10f%%", proporcion_na), 
          ", que está dentro del umbral de ", umbral, "%. Es apropiado utilizar la imputación por la mediana.")
} else {
  message("La proporción de datos faltantes es ", sprintf("%.10f%%", proporcion_na), 
          ", que excede el umbral de ", umbral, "%. Se recomienda considerar métodos de imputación más sofisticados.")
}

# Imputación con la mediana
median_value <- median(mundo$KWH.hh..per.half.hour., na.rm = TRUE)
mundo$KWH.hh..per.half.hour.[is.na(mundo$KWH.hh..per.half.hour.)] <- median_value

# Verificación después de la imputación
sum(is.na(mundo$KWH.hh..per.half.hour.))  # Debería ser 0
head(mundo)

# Gráfico de series temporales post-imputación
plot(mundo$DateTime, mundo$KWH.hh..per.half.hour., type = "l", main = "Consumo de Energía Post-Imputación", xlab = "Tiempo", ylab = "KWH por media hora")


######################################
# Verificar los valores que no se pudieron convertir y que se convirtieron en NA
mundo[is.na(mundo$KWH.hh..per.half.hour.), ]

library(zoo)

# Identificar índices de valores NA antes de la imputación
indices_na_antes <- which(is.na(mundo$KWH.hh..per.half.hour.))

# Mostrar los valores NA antes de la imputación
valores_na_antes <- mundo[indices_na_antes, ]
valores_na_antes

mundo$KWH.hh..per.half.hour. <- na.approx(mundo$KWH.hh..per.half.hour., rule = 2)

# Mostrar los valores que antes eran NA, después de la imputación
valores_na_despues <- mundo[indices_na_antes, ]

# Comparar antes y después de la imputación
comparacion <- data.frame(
  Indices = indices_na_antes,
  Antes = valores_na_antes$KWH.hh..per.half.hour.,
  Despues = valores_na_despues$KWH.hh..per.half.hour.
)

# Imprimir la comparación
print(comparacion)

# Verificar si aún quedan valores NA después de la imputación
sum_na_despues <- sum(is.na(mundo$KWH.hh..per.half.hour.))
print(paste("Número de NA después de la imputación:", sum_na_despues))

##############################################
#Interpolacion con Spline
library(zoo)

# Identificar índices de valores NA antes de la imputación
indices_na_antes <- which(is.na(mundo$KWH.hh..per.half.hour.))

# Mostrar los valores NA antes de la imputación
valores_na_antes <- mundo[indices_na_antes, ]
valores_na_antes

# Interpolación spline para imputar valores NA
mundo$KWH.hh..per.half.hour. <- na.spline(mundo$KWH.hh..per.half.hour.)

# Mostrar los valores que antes eran NA, después de la imputación
valores_na_despues <- mundo[indices_na_antes, ]

# Comparar antes y después de la imputación
comparacion <- data.frame(
  Indices = indices_na_antes,
  Antes = valores_na_antes$KWH.hh..per.half.hour.,
  Despues = valores_na_despues$KWH.hh..per.half.hour.
)

# Imprimir la comparación
print(comparacion)

# Verificar si aún quedan valores NA después de la imputación
sum_na_despues <- sum(is.na(mundo$KWH.hh..per.half.hour.))
print(paste("Número de NA después de la imputación:", sum_na_despues))

###########################################
#Código para k-NN
# Verificar nombres de columnas
# Asegúrate de que no hay NA en las columnas antes de aplicar k-NN
head(mundo)
mundo$DateTimeNum[is.na(mundo$DateTimeNum)] <- median(mundo$DateTimeNum, na.rm = TRUE)
mundo$KWH.hh..per.half.hour.[is.na(mundo$KWH.hh..per.half.hour.)] <- median(mundo$KWH.hh..per.half.hour., na.rm = TRUE)

# Confirmar que las columnas son numéricas
mundo$DateTimeNum <- as.numeric(mundo$DateTimeNum)
mundo$KWH.hh..per.half.hour. <- as.numeric(mundo$KWH.hh..per.half.hour.)

# Aplicar knnImputation
library(DMwR)
mundo_imp <- knnImputation(mundo[, c("DateTimeNum", "KWH.hh..per.half.hour.")], k = 5)

# Reemplazar valores NA con los valores imputados
mundo$KWH.hh..per.half.hour.[is.na(mundo$KWH.hh..per.half.hour.)] <- mundo_imp$data[is.na(mundo$KWH.hh..per.half.hour.), "KWH.hh..per.half.hour."]

# Verificar cuántos NA quedan
na_count_after <- sum(is.na(mundo$KWH.hh..per.half.hour.))
print(paste("Número de NA después de la imputación:", na_count_after))


library(caret)
preProcess_values <- preProcess(mundo[, c("DateTimeNum", "KWH.hh..per.half.hour.")], method = 'knnImpute')

# Aplicar la imputación
mundo_imputed <- predict(preProcess_values, newdata = mundo)
head(mundo)



############################################
#Métodos de Descomposición de Series Temporales (STL)

head(mundo)
library(forecast)
# Descomposición STL
fit <- stl(mundo$KWH.hh..per.half.hour., s.window = "periodic", robust = TRUE)
mundo$KWH.hh..per.half.hour. <- na.interp(fit$time.series[, "seasonal"] + fit$time.series[, "trend"])

head(mundo)
