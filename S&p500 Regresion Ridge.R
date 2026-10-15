rm(list = ls())

#Revisar directorio de salida getwd()
#Definir directorio de salida EJ. setwd(file.choose())  

library(httr2)
library(ggplot2)
library(quantmod)
library(xts)


#DESCARGA
fecha_inicio = as.Date("2021-01-01")
fecha_fin = as.Date("2026-09-21")

url = "https://scanner.tradingview.com/america/scan"

consulta = list(
  symbols = list(symbolset = list("SYML:SP;SPX")),
  columns = list("name","description","sector"),
  sort = list(sortBy = "name",sortOrder = "asc"),
  range = list(0, 600)
)

respuesta = request(url) |>
  req_method("POST") |>
  req_headers(
    `User-Agent` = "Mozilla/5.0",
    Origin = "https://es.tradingview.com",
    Referer = "https://es.tradingview.com/symbols/SPX/components/"
  ) |>
  req_body_json(consulta, auto_unbox = TRUE) |>
  req_perform() |>
  resp_body_json(simplifyVector = FALSE)

# Extraer datos
sp500 = do.call(
  rbind,
  lapply(respuesta$data, function(x) {
    data.frame(
      Simbolo = as.character(x$d[[1]]),
      Empresa = as.character(x$d[[2]]),
      Sector = as.character(x$d[[3]]),
      stringsAsFactors = FALSE
    )
  })
)

sp500 = sp500[order(sp500$Empresa), ] # Ordenar alfabeticamente por empresa
sp500$Numero = seq_len(nrow(sp500)) # Agregar numeracion
sp500 = sp500[, c("Numero", "Simbolo", "Empresa", "Sector")] #Numero como primera columna
rownames(sp500) = NULL # Reiniciar nombres de las filas

#AED de SECTOR

Sector = as.factor(sp500$Sector)

#---Tabla de frecuencia

#Frecuencia absoluta
fi = table(Sector)

#Frecuencia relativa
hi = prop.table(fi)

#Porcentaje
porcentaje = 100 * hi

#Tabla final
tabla = data.frame(
  Categoria = names(fi),
  fi = as.vector(fi),
  hi = round(as.vector(hi), 3),
  Porcentaje = round(as.vector(porcentaje), 2)
)

tabla

#grafico sector
ggplot(tabla,
       aes(x = reorder(Categoria, fi), y = fi)) +
  geom_col() + geom_text( aes(label = fi),hjust = -0.2, size = 3.5) +
  coord_flip() + labs(title = "Empresas del S&P 500 por sector",
                      x = "Sector", y = "Frecuencia absoluta") +
  expand_limits(y = max(tabla$fi) * 1.1) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
  )

##---Descarga de datos precios his. de las emp.

#Adaptar simbolos de TradingView al formato de Yahoo

sp500$Simbolo_Yahoo = gsub( "\\.", "-", sp500$Simbolo )

# Ejemplo:
# BRK.B -> BRK-B
# BF.B  -> BF-B

precios = list() #Lista donde se guardaran los precios
fallidos = character() # Tickers que no pudieron descargarse

#Descargar cada empresa

for (i in seq_len(nrow(sp500))) {
  
  simbolo = sp500$Simbolo_Yahoo[i]
  
  cat(i, "/", nrow(sp500), " - Descargando:", simbolo, "\n")
  
  datos = try( getSymbols( simbolo,
                           src = "yahoo",
                           from = fecha_inicio, to = fecha_fin + 1,
                           auto.assign = FALSE, warnings = FALSE),
               silent = TRUE)
  
  
  #Si la descarga fue exitosa
  
  if (!inherits(datos, "try-error")) {
    precio = Ad(datos)
    colnames(precio) = sp500$Simbolo[i]
    precios[[sp500$Simbolo[i]]] = precio
    
    #Si no existen datos
  } else {
    fallidos = c(fallidos, sp500$Simbolo[i])
  }
  
  Sys.sleep(0.1)
}


#UNIR TODAS LAS EMPRESAS

matriz_precios = do.call(merge, c(precios, all = TRUE))
saveRDS(matriz_precios, file = "matriz_precios.rds")
getwd()
matriz_precios = readRDS("matriz_precios.rds")

#Empresas con NA?
na_empresa = colSums( is.na(matriz_precios))
# Porcentaje de datos faltantes por empresa
porcentaje_na = 100 * na_empresa / nrow(matriz_precios)
sort(porcentaje_na, decreasing = TRUE)

#A esta fecha 16 empresas presentan algun porcentaje de NA
#destacando HONA, FDXF , Q , SNDK con sobre el 70% de datos faltantes

#Acontinuacion se evaluara que empresas precentan NA de forma atipica
# Cuartiles
Q1 = quantile(porcentaje_na, 0.25)
Q3 = quantile(porcentaje_na, 0.75)

# Rango intercuartilico
RIC = Q3 - Q1

# Limite superior
limite = Q3 + 1.5 * RIC

Q1
Q3
RIC
limite

#Efectivamente como la mayoria de las empresas no contienen datos faltantes
#se detectaran como atipicas todas las empresas que si las tengan 

#Deteccion de empresas atipicas
empresas_atipicas = porcentaje_na[porcentaje_na > limite]
sort(empresas_atipicas, decreasing = TRUE) 

nombres_empresas_atipicas = names(empresas_atipicas)

# Seleccionar solamente las empresas atipicas
matriz_atipicas = matriz_precios[, nombres_empresas_atipicas, drop = FALSE]

# Aqui se busca distinguir si los datos faltantes corresponden a
# periodos anteriores a la primera observacion disponible, o si
# existen valores NA dentro del periodo efectivo de cotizacion
# de las empresas identificadas como atipicas.

patron_na = data.frame(
  Empresa = colnames(matriz_atipicas),
  Total_NA = colSums(is.na(matriz_atipicas)),
  Porcentaje_NA = round(
    100 * colSums(is.na(matriz_atipicas)) /
      nrow(matriz_atipicas),
    2
  ),
  NA_Internos = 0,
  Observaciones_Periodo = 0
)

for (j in seq_len(ncol(matriz_atipicas))) {
  
  x = matriz_atipicas[, j]
  
  observados = which(!is.na(x))
  
  if (length(observados) > 1) {
    
    inicio = min(observados)
    fin = max(observados)
    
    # Cantidad de NA dentro del periodo observado
    patron_na$NA_Internos[j] =
      sum(is.na(x[inicio:fin]))
    
    # Longitud del periodo efectivo de observacion
    patron_na$Observaciones_Periodo[j] =
      fin - inicio + 1
  }
}

# Porcentaje de NA internos respecto al periodo efectivo
patron_na$Porcentaje_NA_Internos =
  round(
    100 * patron_na$NA_Internos /
      patron_na$Observaciones_Periodo,
    3
  )

# Ordenar desde la empresa con mayor porcentaje total de NA
patron_na = patron_na[
  order(patron_na$Porcentaje_NA, decreasing = TRUE),
]

rownames(patron_na) = NULL

#---DESCRIPCION DE LA TABLA DE DATOS FALTANTES

# La tabla patron_na resume el comportamiento de los datos
# faltantes de las empresas previamente identificadas como
# atipicas.

# Empresa:
#   Simbolo bursatil de la empresa.

# Total_NA:
#   Numero total de valores faltantes (NA) de la empresa
#   considerando todo el periodo analizado.

# Porcentaje_NA:
#   Porcentaje de datos faltantes respecto al numero total de
#   fechas contenidas en la matriz de precios.

# NA_Internos:
#   Numero de valores faltantes ubicados entre la primera y la
#   ultima observacion disponible de la empresa.

# Observaciones_Periodo:
#   Numero de fechas comprendidas entre la primera y la ultima
#   observacion disponible de la empresa, ambas inclusive.
#   Este valor representa la longitud de su periodo efectivo de
#   observacion dentro de la matriz.

# Porcentaje_NA_Internos:
#   Porcentaje de datos faltantes dentro del periodo efectivo
#   de observacion de la empresa:

#       100 * NA_Internos / Observaciones_Periodo

# Esto permite distinguir entre empresas cuyos valores faltantes
# se concentran antes de su primera observacion disponible y
# empresas que presentan interrupciones dentro de su propia
# serie historica.

patron_na

# CRITERIO PARA LA ELIMINACION DE EMPRESAS

# Se establece como criterio conservar aquellas empresas que
# presenten al menos un 80% de cobertura temporal respecto al
# periodo total analizado.
# Esto equivale a permitir como maximo un 20% de datos faltantes.

# El criterio busca evitar la imputacion de grandes bloques de
# observaciones inexistentes, principalmente en empresas cuya
# primera observacion disponible ocurre considerablemente despues
# del inicio del periodo de estudio.

# Dado que la mayor parte de estos NA se encuentra antes de la
# primera observacion disponible y no corresponde a interrupciones
# dentro de la serie, no resulta apropiado imputarlos como si fueran
# datos faltantes aleatorios.

# Por lo tanto:
#   Porcentaje_NA <= 20%  -> Conservar
#   Porcentaje_NA >  20%  -> Eliminar

# Los NA internos se analizan separadamente, ya que representan
# interrupciones efectivas dentro de una serie ya observada.


# Umbral maximo permitido de datos faltantes
umbral_na = 20

patron_na$Decision = ifelse( patron_na$Porcentaje_NA <= umbral_na, 
                             "Conservar","Eliminar")

patron_na

empresas_eliminar =
  patron_na$Empresa[
    patron_na$Decision == "Eliminar"
  ]

empresas_conservar =
  patron_na$Empresa[
    patron_na$Decision == "Conservar"
  ]

empresas_eliminar
empresas_conservar

# Dada la información obtenida, se evaluará la conveniencia de conservar
# las empresas "CEG", "HOOD", "APP", "COIN" y "EXE". Posteriormente,
# se determinará el procedimiento a seguir con "FISV", cuyo patrón de
# datos faltantes es distinto al observado en las empresas anteriores.
#...
# La decisión de mantener estas empresas no debe basarse únicamente en
# su porcentaje individual de datos faltantes. Su inclusión puede obligar
# a desplazar la fecha inicial del período común de análisis, provocando
# la eliminación de observaciones válidas para todas las demás empresas.
#
# Por lo tanto, conservar una empresa con una serie histórica más corta
# puede implicar una pérdida global de información considerablemente mayor
# que la pérdida asociada a eliminar únicamente dicha empresa.
#
# En consecuencia, se evaluará el costo de conservar cada empresa en
# términos de la cantidad de fechas y observaciones que deberían
# eliminarse del resto de la matriz para obtener un período común completo.



# EVALUACION DEL COSTO DE CONSERVAR EMPRESAS


# Las empresas con coberturas considerablemente menores ya fueron
# identificadas para su eliminacion. A continuacion se evalua el
# efecto de conservar o eliminar "CEG", "HOOD", "APP", "COIN" y
# "EXE" sobre el periodo comun de observacion de toda la matriz.
#
# Para cada escenario se calcula:
#
# - Numero de empresas conservadas.
# - Primera fecha comun disponible.
# - Numero de fechas disponibles desde dicha fecha.
# - Tamaño efectivo potencial de la matriz, definido como:
#
#             Informacion = N_fechas * N_empresas
#
# Este criterio permite comparar la informacion aportada por mantener
# una empresa con la cantidad de observaciones del resto de las
# empresas que se pierden al desplazar el inicio del periodo.

# ============================================================
# EVALUACION DE EMPRESAS CON COBERTURA TEMPORAL PARCIAL
# ============================================================

# Las empresas con una cobertura temporal muy reducida ya fueron
# identificadas previamente y se excluyen de esta etapa del analisis.

empresas_eliminar_previas = c(
  "HONA", "FDXF", "Q", "SNDK", "GEV",
  "SOLV", "RDDT", "VLTO", "KVUE", "GEHC"
)

# MATRIZ PARA LA EVALUACION DE ESCENARIOS

# Se construye una matriz temporal excluyendo las empresas cuya
# eliminacion ya fue determinada.

matriz_revision = matriz_precios[,!colnames(matriz_precios) %in% empresas_eliminar_previas,drop = FALSE]

# FISV se excluye temporalmente de esta evaluacion, debido a que
# presenta un NA interno. Su caso sera analizado posteriormente
# de manera separada.

matriz_revision = matriz_revision[,colnames(matriz_revision) != "FISV",drop = FALSE]

#            DEFINICION DE LOS ESCENARIOS

# Se evaluara el efecto de eliminar progresivamente las empresas
# que restringen el inicio comun de la matriz.

escenarios = list( "Conservar todas" = character(0),
  "Eliminar CEG" = c("CEG"),
  "Eliminar CEG + HOOD" = c("CEG", "HOOD"),
  "Eliminar CEG + HOOD + APP" = c("CEG", "HOOD", "APP"),
  "Eliminar CEG + HOOD + APP + COIN" = c("CEG", "HOOD", "APP", "COIN"),
  "Eliminar CEG + HOOD + APP + COIN + EXE" = c("CEG", "HOOD", "APP", "COIN", "EXE"))


#     CONSTRUCCION DE LA TABLA DE RESPALDO

tabla_respaldo = data.frame()

for (nombre in names(escenarios)) {
  
  # Empresas que se eliminan en el escenario actual
  eliminar = escenarios[[nombre]]

  # Construir la matriz correspondiente al escenario
  matriz_temp = matriz_revision[,!colnames(matriz_revision) %in% eliminar,drop = FALSE]
  
  # Primera observacion disponible para cada empresa
  primera_obs = sapply(seq_len(ncol(matriz_temp)),function(j) {
    min(which(!is.na(matriz_temp[, j])))})
  
  # El inicio comun esta determinado por la empresa cuya
  # primera observacion disponible ocurre mas tarde.
  inicio_comun = max(primera_obs)
  
  # Numero de fechas disponibles desde el inicio comun
  # hasta el final del periodo.
  n_fechas = nrow(matriz_temp) - inicio_comun + 1
  
  # Numero de empresas conservadas
  n_empresas = col(matriz_temp)
  
  # Cantidad total de informacion efectiva de la matriz:
  #      Informacion = numero de fechas * numero de empresas
  
  informacion = n_fechas * n_empresas
  
  # Agregar resultados a la tabla
  tabla_respaldo = rbind(tabla_respaldo,
    data.frame(Escenario = nombre,
      Empresas = n_empresas,
      Inicio_Comun = as.Date(index(matriz_temp)[inicio_comun]),
      Fechas_Comunes = n_fechas,
      Informacion_Total = informacion))}

#      COMPARACION ENTRE ESCENARIOS
# Cambio en la cantidad de informacion respecto al
# escenario inmediatamente anterior.

tabla_respaldo$Cambio_Informacion = c(NA,diff(tabla_respaldo$Informacion_Total))

# Cambio porcentual respecto al escenario inmediatamente anterior.

tabla_respaldo$Cambio_Porcentual = round(100 * tabla_respaldo$Cambio_Informacion /
                                           c(NA, head(tabla_respaldo$Informacion_Total, -1)),2)
# Numero de fechas recuperadas respecto al escenario
# inmediatamente anterior.

tabla_respaldo$Fechas_Recuperadas = c(NA,diff(tabla_respaldo$Fechas_Comunes))

# Ganancia absoluta de informacion respecto al
# escenario original, donde se conservan todas las empresas.

tabla_respaldo$Ganancia_Desde_Original = tabla_respaldo$Informacion_Total - tabla_respaldo$Informacion_Total[1]

# Ganancia porcentual de informacion respecto al
# escenario original.

tabla_respaldo$Ganancia_Porc_Original = round(100 * tabla_respaldo$Ganancia_Desde_Original / tabla_respaldo$Informacion_Total[1], 2 )

#        TABLA FINAL DE RESPALDO

tabla_respaldo

#     INTERPRETACION DE LOS ESCENARIOS

# La eliminacion de CEG permite recuperar 120 fechas para las
# empresas restantes, incrementando la cantidad total de informacion
# de la matriz en aproximadamente un 10.01%.

# Al eliminar adicionalmente HOOD se recuperan otras 73 fechas,
# generando un incremento adicional del 5.43%.

# En conjunto, la eliminacion de CEG y HOOD incrementa la informacion
# disponible desde 576624 hasta 668850 observaciones, lo que equivale
# a una ganancia aproximada del 15.99% respecto al escenario original.

# En cambio, eliminar APP de manera individual no resulta conveniente,
# ya que permite recuperar solamente una fecha y reduce la cantidad
# ttal de informacion de la matriz en aproximadamente un 0.13%.

# APP y COIN presentan fechas iniciales muy cercanas, por lo que su
# eliminacion debe analizarse de manera conjunta y no individual.

# Aunque continuar eliminando APP, COIN y EXE permite aumentar
# nuevamente el numero total de observaciones disponibles, las
# ganancias marginales son progresivamente menores y requieren
# sacrificar variables adicionales.

# Por lo tanto, se adopta como criterio de equilibrio entre cobertura
# temporal y cobertura transversal eliminar CEG y HOOD, conservando
# APP, COIN y EXE para las etapas posteriores del analisis.

# Finalmente, FISV sera evaluada de manera independiente, debido a
# que presenta un NA interno y, por lo tanto, corresponde a un
# problema diferente al inicio tardio de las series.


index(matriz_precios)[ is.na(matriz_precios$FISV)]

#que ocurrio con FISV EL "2025-11-12" ? por que no hay datos?

# FISV si cotizo el 2025-11-12. En dicha fecha, la accion registro
# un precio de cierre de 64.38 USD.

# Dado que el valor faltante corresponde a una omision de la fuente
# utilizada y no a la ausencia real de cotizacion, el NA sera
# reemplazado por el valor original observado para esa fecha.

matriz_precios["2025-11-12", "FISV"] = 64.38

#         ELIMINACION DE EMPRESAS CON DATOS FALTANTES

# Se eliminan todas las empresas identificadas previamente en
# patron_na, excepto FISV. Esta empresa se conserva debido a que
# su unico dato faltante fue identificado y reemplazado por el
# valor original observado para dicha fecha.

(empresas_eliminar = setdiff(patron_na$Empresa,"FISV"))

matriz_precios_dep = matriz_precios[,!colnames(matriz_precios) %in% empresas_eliminar,drop = FALSE]

dim(matriz_precios)
dim(matriz_precios_dep)

#      NUEVA REVISION DE DATOS FALTANTES

colSums(is.na(matriz_precios_dep))
sum(is.na(matriz_precios_dep))

#     GUARDAR MATRIZ DE PRECIOS LIMPIA

matriz_precios = matriz_precios_dep

#--COMPROBACIONES INICIALES

dim(matriz_precios)
class(matriz_precios)

# Periodo analizado
(fecha_inicial = as.Date(first(index(matriz_precios))))
(fecha_final   = as.Date(last(index(matriz_precios))))

# NUmero de fechas y empresas
(n_fechas   = nrow(matriz_precios))
(n_empresas = ncol(matriz_precios))

# Comprobar que no existan precios no positivos
sum(matriz_precios <= 0, na.rm = TRUE)

# Comprobar si esque existen fechas duplicadas
sum(duplicated(index(matriz_precios)))

# Comprobar si esque existen simbolos duplicados
sum(duplicated(colnames(matriz_precios)))

# Resumen general de la base
resumen_base = data.frame(
  Fecha_Inicial = fecha_inicial,
  Fecha_Final = fecha_final,
  Numero_Fechas = n_fechas,
  Numero_Empresas = n_empresas,
  Valores_Faltantes = sum(is.na(matriz_precios)),
  Precios_No_Positivos =
    sum(matriz_precios <= 0, na.rm = TRUE)
)

resumen_base

#-----FUNCIONES AUXILIARES

asimetria = function(x) {
  x = x[is.finite(x)]
  n = length(x)
  
  if (n < 3 || sd(x) == 0) {
    return(NA_real_)
  }
  
  z = (x - mean(x)) / sd(x)
  n / ((n - 1) * (n - 2)) * sum(z^3)
}


curtosis_exceso = function(x) {
  x = x[is.finite(x)]
  n = length(x)
  
  if (n < 4 || sd(x) == 0) {
    return(NA_real_)
  }
  
  z = (x - mean(x)) / sd(x)
  
  termino_1 = n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum(z^4)
  termino_2 = 3 * (n - 1)^2 / ((n - 2) * (n - 3))
  
  termino_1 - termino_2
}


moda = function(x, digitos = 2) {
  x = round(x[is.finite(x)], digitos)
  frecuencias = table(x)
  
  if (length(frecuencias) == 0) {
    return(NA_real_)
  }
  
  as.numeric(names(frecuencias)[which.max(frecuencias)])
}

#POR EMPRESA: PRECIOS

precios = as.matrix(matriz_precios)

resumen_precios = data.frame(Empresa = colnames(precios),
                             Observaciones = apply(precios, 2, function(x) sum(is.finite(x))),
                             Media = apply(precios, 2, mean, na.rm = TRUE),
                             Mediana = apply(precios, 2, median, na.rm = TRUE),
                             Desviacion_Estandar = apply(precios, 2, sd, na.rm = TRUE),
                             Minimo = apply(precios, 2, min, na.rm = TRUE),
                             Q1 = apply(precios, 2, quantile, probs = 0.25, na.rm = TRUE),
                             Q3 = apply(precios, 2, quantile, probs = 0.75, na.rm = TRUE),
                             Maximo = apply(precios, 2, max, na.rm = TRUE),
                             Asimetria = apply(precios, 2, asimetria),
                             Curtosis_Exceso = apply(precios, 2, curtosis_exceso))

# Rango y RIC
resumen_precios$Rango = resumen_precios$Maximo - resumen_precios$Minimo
resumen_precios$RIC = resumen_precios$Q3 - resumen_precios$Q1

# Coef. de variacion
resumen_precios$Coeficiente_Variacion = resumen_precios$Desviacion_Estandar / resumen_precios$Media

# Ordenar las columnas
resumen_precios = resumen_precios[, c("Empresa",
                                      "Observaciones",
                                      "Media", 
                                      "Mediana",
                                      "Desviacion_Estandar",
                                      "Coeficiente_Variacion",
                                      "Minimo",
                                      "Q1",
                                      "Q3",
                                      "Maximo",
                                      "Rango",
                                      "RIC",
                                      "Asimetria",
                                      "Curtosis_Exceso")]

# Redondear variables 
resumen_precios[-1] = round(resumen_precios[-1], 4)
head(resumen_precios)

#--RENDIMIENTOS LOGARITMICOS

# r_t = log(P_t / P_{t-1}). Al partir de la matriz depurada, todas las
# empresas comparten las mismas fechas y el cálculo conserva la alineación.
rendimientos = na.omit(diff(log(matriz_precios)))
matriz_rendimientos = as.matrix(rendimientos)

stopifnot(nrow(matriz_rendimientos) > 0,!anyNA(matriz_rendimientos),all(is.finite(matriz_rendimientos)))

# Resumen de rendimientos por empresa
resumen_rendimientos = data.frame(
  Empresa = colnames(matriz_rendimientos),
  Observaciones = nrow(matriz_rendimientos),
  Media = apply(matriz_rendimientos, 2, mean),
  Mediana = apply(matriz_rendimientos, 2, median),
  Desviacion_Estandar = apply(matriz_rendimientos, 2, sd),
  Minimo = apply(matriz_rendimientos, 2, min),
  Q1 = apply(matriz_rendimientos, 2, quantile, probs = 0.25),
  Q3 = apply(matriz_rendimientos, 2, quantile, probs = 0.75),
  Maximo = apply(matriz_rendimientos, 2, max),
  Asimetria = apply(matriz_rendimientos, 2, asimetria),
  Curtosis_Exceso = apply(matriz_rendimientos, 2, curtosis_exceso),
  row.names = NULL
)

resumen_rendimientos$Rango = resumen_rendimientos$Maximo - resumen_rendimientos$Minimo
resumen_rendimientos$RIC = resumen_rendimientos$Q3 - resumen_rendimientos$Q1

# Indicador equiponderado: promedio transversal de los rendimientos diarios.
rendimiento_mercado = rowMeans(matriz_rendimientos)

resumen_global = data.frame(
  Estadistico = c(
    "Observaciones",
    "Media diaria",
    "Mediana diaria",
    "Desviacion estandar diaria",
    "Minimo diario",
    "Maximo diario",
    "Asimetria",
    "Curtosis en exceso"
  ),
  Valor = c(
    length(rendimiento_mercado),
    mean(rendimiento_mercado),
    median(rendimiento_mercado),
    sd(rendimiento_mercado),
    min(rendimiento_mercado),
    max(rendimiento_mercado),
    asimetria(rendimiento_mercado),
    curtosis_exceso(rendimiento_mercado)
  )
)

# Evolucion anual del indicador equiponderado. El rendimiento acumulado se
# obtiene componiendo los rendimientos logarítmicos del año.
fechas_rendimientos = as.Date(index(rendimientos))
anios = format(fechas_rendimientos, "%Y")

resumen_anual = do.call(
  rbind,
  lapply(split(seq_along(rendimiento_mercado), anios), function(idx) {
    x = rendimiento_mercado[idx]
    data.frame(
      Anio = as.integer(format(fechas_rendimientos[idx[1]], "%Y")),
      Observaciones = length(x),
      Rendimiento_Medio_Diario = mean(x),
      Volatilidad_Diaria = sd(x),
      Rendimiento_Acumulado = exp(sum(x)) - 1,
      Minimo_Diario = min(x),
      Maximo_Diario = max(x)
    )
  })
)
rownames(resumen_anual) = NULL

# Correlaciones por pares en formato largo. Se conserva sólo el triangulo
# superior para no duplicar cada pareja de empresas.
matriz_correlaciones = cor(matriz_rendimientos)
indices_correlacion = which(upper.tri(matriz_correlaciones),arr.ind = TRUE)

tabla_correlaciones = data.frame(
  Empresa_1 = rownames(matriz_correlaciones)[indices_correlacion[, 1]],
  Empresa_2 = colnames(matriz_correlaciones)[indices_correlacion[, 2]],
  Correlacion = matriz_correlaciones[indices_correlacion],
  row.names = NULL
)

tabla_correlaciones = tabla_correlaciones[order(abs(tabla_correlaciones$Correlacion), decreasing = TRUE),]
rownames(tabla_correlaciones) = NULL

#--DETECCION DE JORNADAS EXTREMAS

Q1_mercado = quantile(rendimiento_mercado, 0.25)
Q3_mercado = quantile(rendimiento_mercado, 0.75)

RIC_mercado = Q3_mercado - Q1_mercado

limite_inferior = Q1_mercado - 1.5 * RIC_mercado
limite_superior = Q3_mercado + 1.5 * RIC_mercado

es_atipico = rendimiento_mercado < limite_inferior | rendimiento_mercado > limite_superior

jornadas_atipicas = data.frame(Fecha = as.Date(index(rendimientos)[es_atipico]),
                               Rendimiento_Mercado = rendimiento_mercado[es_atipico])

(jornadas_atipicas = jornadas_atipicas[ order(jornadas_atipicas$Rendimiento_Mercado),])

nrow(jornadas_atipicas)


#                      REGRESION RIDGE


if (!requireNamespace("glmnet", quietly = TRUE)) {
  stop("Debe instalar glmnet: install.packages('glmnet')")
}

#      COMPROBACIONES Y RENDIMIENTOS DE LAS EMPRESAS


if (!inherits(matriz_precios, c("xts", "zoo"))) {
  stop("matriz_precios debe ser un objeto xts o zoo.")
}

if (anyNA(matriz_precios)) {
  stop("matriz_precios todavia contiene valores NA.")
}

if (any(matriz_precios <= 0)) {
  stop("Existen precios no positivos; no se puede aplicar log().")
}

if (anyDuplicated(index(matriz_precios)) > 0) {
  stop("Existen fechas duplicadas.")
}

if (anyDuplicated(colnames(matriz_precios)) > 0) {
  stop("Existen simbolos duplicados.")
}

matriz_retornos = na.omit(diff(log(matriz_precios)))


#             DESCARGA DEL INDICE S&P 500 (VAR DEP.)

fecha_inicio_modelo = as.Date(first(index(matriz_precios)))
fecha_fin_modelo = as.Date(last(index(matriz_precios)))

sp500_indice = getSymbols(
  "^GSPC",
  src = "yahoo",
  from = fecha_inicio_modelo,
  to = fecha_fin_modelo + 1,
  auto.assign = FALSE
)

precio_sp500 = Ad(sp500_indice)
colnames(precio_sp500) = "SP500"
retorno_sp500 = na.omit(diff(log(precio_sp500)))

#        ALINEACION DE RESPUESTA Y PREDICTORES

datos_modelo = merge(retorno_sp500,matriz_retornos,join = "inner")

datos_modelo = na.omit(datos_modelo)

Y = as.numeric(datos_modelo[, "SP500"])
X = as.matrix(datos_modelo[, colnames(datos_modelo) != "SP500"])
fechas_modelo = as.Date(index(datos_modelo))

stopifnot(
  nrow(X) == length(Y),
  nrow(X) == length(fechas_modelo),
  !anyNA(X),
  !anyNA(Y),
  all(is.finite(X)),
  all(is.finite(Y))
)

# Eliminar solamente predictores de varianza cero, si existieran.
desviaciones = apply(X, 2, sd)
variables_constantes = names(desviaciones)[!is.finite(desviaciones) | desviaciones == 0]

if (length(variables_constantes) > 0) {
  X = X[, !colnames(X) %in% variables_constantes, drop = FALSE]
}

cat("Observaciones alineadas:", nrow(X), "\n")
cat("Empresas utilizadas:", ncol(X), "\n")
cat("Periodo:", as.character(min(fechas_modelo)), "a", as.character(max(fechas_modelo)), "\n")


#            DIAGNOSTICO DE MULTICOLINEALIDAD

# Se diagnostica la correlacion, pero no se eliminan variables por este
# motivo: ridge esta disenado para trabajar con predictores correlacionados.
matriz_correlacion = cor(X)
correlaciones_pares = matriz_correlacion[upper.tri(matriz_correlacion)]

tabla_correlaciones_altas = data.frame(
  Umbral = c(0.70, 0.80, 0.90),
  Numero_Pares = c(
    sum(abs(correlaciones_pares) > 0.70),
    sum(abs(correlaciones_pares) > 0.80),
    sum(abs(correlaciones_pares) > 0.90)
  )
)

print(tabla_correlaciones_altas)


#        PARTICION CRONOLOGICA: 80% ENTRENAMIENTO, 20% PRUEBA

n = nrow(X)
n_train = floor(0.80 * n)

if (n_train < 100 || (n - n_train) < 30) {
  stop("No hay suficientes observaciones para dividir entrenamiento y prueba.")
}

idx_train = seq_len(n_train)
idx_test = seq.int(n_train + 1L, n)

X_train = X[idx_train, , drop = FALSE]
Y_train = Y[idx_train]
fechas_train = fechas_modelo[idx_train]

X_test = X[idx_test, , drop = FALSE]
Y_test = Y[idx_test]
fechas_test = fechas_modelo[idx_test]

tabla_particion = data.frame(
  Muestra = c("Entrenamiento", "Prueba"),
  Inicio = c(min(fechas_train), min(fechas_test)),
  Fin = c(max(fechas_train), max(fechas_test)),
  Observaciones = c(length(Y_train), length(Y_test))
)

print(tabla_particion)

# No se estandariza X antes de dividir los datos. glmnet estandarizara cada
# ajuste usando exclusivamente las observaciones de entrenamiento respectivas.

#      VALIDACION TEMPORAL PARA SELECCIONAR LAMBDA

lambda_grid = 10^seq(-8, 0, length.out = 200)
K = 5L

# La primera mitad del entrenamiento forma la ventana inicial. La segunda
# mitad se divide en cinco bloques futuros consecutivos de validacion.
inicio_validacion = floor(0.50 * n_train) + 1L
bordes = floor(seq(inicio_validacion, n_train + 1L, length.out = K + 1L))

folds_temporales = lapply(seq_len(K), function(k) {
  lista_validacion = seq.int(bordes[k], bordes[k + 1L] - 1L)
  lista_entrenamiento = seq_len(bordes[k] - 1L)
  
  list(
    entrenamiento = lista_entrenamiento,
    validacion = lista_validacion
  )
})

mse_folds = matrix(
  NA_real_,
  nrow = K,
  ncol = length(lambda_grid)
)

for (k in seq_len(K)) {
  id_entrenamiento = folds_temporales[[k]]$entrenamiento
  id_validacion = folds_temporales[[k]]$validacion
  
  ajuste_k = glmnet::glmnet(
    x = X_train[id_entrenamiento, , drop = FALSE],
    y = Y_train[id_entrenamiento],
    alpha = 0,
    lambda = lambda_grid,
    family = "gaussian",
    intercept = TRUE,
    standardize = TRUE
  )
  
  prediccion_k = predict(
    ajuste_k,
    newx = X_train[id_validacion, , drop = FALSE],
    s = lambda_grid
  )
  
  errores_k = sweep(
    prediccion_k,
    1,
    Y_train[id_validacion],
    FUN = "-"
  )
  
  mse_folds[k, ] = colMeans(errores_k^2)
}

mse_cv = colMeans(mse_folds)
se_cv = apply(mse_folds, 2, sd) / sqrt(K)

# Lambda que minimiza el error de validacion.
indice_min = which.min(mse_cv)
lambda_min = lambda_grid[indice_min]

# Regla de un error estandar: mayor lambda cuyo error no supera el minimo
# mas un error estandar. Es una solucion mas regularizada y estable.
limite_1se = mse_cv[indice_min] + se_cv[indice_min]
indices_admisibles = which(mse_cv <= limite_1se)
indice_1se = indices_admisibles[which.max(lambda_grid[indices_admisibles])]
lambda_1se = lambda_grid[indice_1se]

lambda_final = lambda_1se

tabla_cv = data.frame(
  Lambda = lambda_grid,
  Log10_Lambda = log10(lambda_grid),
  MSE = mse_cv,
  Error_Estandar = se_cv
)

tabla_lambda = data.frame(
  Regla = c("Minimo", "Un error estandar", "Seleccion final"),
  Lambda = c(lambda_min, lambda_1se, lambda_final)
)

print(tabla_lambda)

#       AJUSTE RIDGE FINAL

modelo_ridge = glmnet::glmnet(
  x = X_train,
  y = Y_train,
  alpha = 0,
  lambda = lambda_grid,
  family = "gaussian",
  intercept = TRUE,
  standardize = TRUE
)

pred_train = as.numeric(
  predict(modelo_ridge, newx = X_train, s = lambda_final)
)

pred_test = as.numeric(
  predict(modelo_ridge, newx = X_test, s = lambda_final)
)

#  METRICAS

calcular_metricas = function(observado, predicho) {
  residuos = observado - predicho
  sse = sum(residuos^2)
  sst = sum((observado - mean(observado))^2)
  
  c(
    MSE = mean(residuos^2),
    RMSE = sqrt(mean(residuos^2)),
    MAE = mean(abs(residuos)),
    R2 = if (sst > 0) 1 - sse / sst else NA_real_,
    Correlacion = if (sd(predicho) > 0) {
      cor(observado, predicho)
    } else {
      NA_real_
    }
  )
}

metricas_train = calcular_metricas(Y_train, pred_train)
metricas_test = calcular_metricas(Y_test, pred_test)

# Comparacion con un modelo ingenuo que predice la media de entrenamiento.
pred_test_media = rep(mean(Y_train), length(Y_test))
metricas_media = calcular_metricas(Y_test, pred_test_media)

tabla_metricas = data.frame(
  Muestra = c("Entrenamiento ridge", "Prueba ridge", "Prueba media"),
  rbind(metricas_train, metricas_test, metricas_media),
  row.names = NULL
)

print(round(tabla_metricas[-1], 8))


#            COEFICIENTES

coeficientes = as.matrix(coef(modelo_ridge, s = lambda_final))

tabla_coeficientes = data.frame(
  Variable = rownames(coeficientes),
  Coeficiente = as.numeric(coeficientes[, 1]),
  row.names = NULL
)

tabla_coeficientes$Magnitud_Absoluta = abs(tabla_coeficientes$Coeficiente)
tabla_coeficientes = tabla_coeficientes[order(tabla_coeficientes$Magnitud_Absoluta, decreasing = TRUE),]
rownames(tabla_coeficientes) = NULL

head(tabla_coeficientes, 20)

# Ridge contrae los coeficientes, pero normalmente no los hace cero.


#      GRAFICOS

grafico_cv = ggplot(tabla_cv, aes(x = Log10_Lambda, y = MSE)) +
  geom_ribbon(
    aes(ymin = pmax(MSE - Error_Estandar, 0),
      ymax = MSE + Error_Estandar),
    fill = "grey70",
    alpha = 0.4 ) + geom_line(linewidth = 0.8) + geom_vline(
    xintercept = log10(lambda_min), color = "blue", linetype = "dashed") +
  geom_vline( xintercept = log10(lambda_1se), color = "red", linetype = "dashed") +
  labs(
    title = "Seleccion temporal de lambda",
    subtitle = "Azul: minimo; rojo: regla de un error estandar",
    x = expression(log[10](lambda)),
    y = "MSE de validacion"
  ) + theme_minimal()

print(grafico_cv)

datos_prediccion = data.frame(
  Fecha = fechas_test,
  Observado = Y_test,
  Predicho = pred_test,
  Residuo = Y_test - pred_test
)

grafico_prediccion = ggplot(datos_prediccion) +
  geom_line(
    aes(x = Fecha, y = Observado, color = "Observado"),
    linewidth = 0.55
  ) +
  geom_line(
    aes(x = Fecha, y = Predicho, color = "Predicho"),
    linewidth = 0.55
  ) +
  scale_color_manual(
    values = c(Observado = "black", Predicho = "red")
  ) +
  labs(
    title = "Rendimiento observado y estimado en prueba",
    x = "Fecha",
    y = "Rendimiento logaritmico",
    color = NULL
  ) +
  theme_minimal()

print(grafico_prediccion)

grafico_dispersion = ggplot(
  datos_prediccion,
  aes(x = Observado, y = Predicho)
) +
  geom_point(alpha = 0.55) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "red",
    linetype = "dashed"
  ) +
  coord_equal() +
  labs(
    title = "Valores observados frente a predichos",
    x = "Observado",
    y = "Predicho"
  ) +
  theme_minimal()

print(grafico_dispersion)

# Trayectorias de los coeficientes ridge.
plot(
  modelo_ridge,
  xvar = "lambda",
  label = FALSE,
  main = "Trayectorias de coeficientes ridge"
)
abline(v = log(lambda_final), col = "red", lty = 2)

#              RESULTADO FINAL

resultado_ridge = list(
  lambda_min = lambda_min,
  lambda_1se = lambda_1se,
  lambda_final = lambda_final,
  metricas = tabla_metricas,
  coeficientes = tabla_coeficientes,
  predicciones_prueba = datos_prediccion,
  validacion = tabla_cv,
  modelo = modelo_ridge
)

cat("\nLambda final:", lambda_final, "\n")
cat("\nMetricas:\n")
print(tabla_metricas)
cat("\nCoeficientes de mayor magnitud:\n")
print(head(tabla_coeficientes, 20))


#          DIAGNOSTICO DE RESIDUOS DEL MODELO RIDGE

# Estos diagnosticos complementan la evaluacion predictiva. No se usan para
# seleccionar lambda y, por lo tanto, la muestra de prueba sigue reservada
# para evaluar el procedimiento final.

calcular_diagnosticos_residuos = function(observado, predicho, muestra) {
  residuos = observado - predicho
  n_obs = length(residuos)
  rezago = min(20L, max(1L, floor(n_obs / 5)))
  
  # Dependencia lineal serial de los residuos.
  prueba_ljung_box = Box.test(
    residuos,
    lag = rezago,
    type = "Ljung-Box",
    fitdf = 0
  )
  
  # Dependencia serial en la magnitud de los residuos. Esta es una
  # comprobacion sencilla de agrupamiento de volatilidad tipo ARCH.
  prueba_ljung_box_cuadrados = Box.test(
    residuos^2,
    lag = rezago,
    type = "Ljung-Box",
    fitdf = 0
  )
  
  # Prueba auxiliar de heterocedasticidad. Se modela la varianza residual
  # mediante el nivel y el cuadrado del valor ajustado.
  ajuste_varianza = lm(residuos^2 ~ predicho + I(predicho^2))
  estadistico_heterocedasticidad = n_obs * summary(ajuste_varianza)$r.squared
  gl_heterocedasticidad = 2L
  p_heterocedasticidad = pchisq( estadistico_heterocedasticidad, df = gl_heterocedasticidad, lower.tail = FALSE)
  
  # Jarque-Bera se incluye solo como descripcion de la forma distributiva.
  # La normalidad no es un requisito para calcular ni predecir con ridge.
  media_residuos = mean(residuos)
  desv_residuos = sd(residuos)
  asimetria_residuos = mean(((residuos - media_residuos) / desv_residuos)^3)
  curtosis_residuos = mean(((residuos - media_residuos) / desv_residuos)^4)
  estadistico_jb = n_obs / 6 * (asimetria_residuos^2 + (curtosis_residuos - 3)^2 / 4)
  p_jb = pchisq(estadistico_jb, df = 2, lower.tail = FALSE)
  
  data.frame(
    Muestra = muestra,
    Observaciones = n_obs,
    Media_Residuo = media_residuos,
    Desviacion_Residuo = desv_residuos,
    Rezago = rezago,
    Ljung_Box_Q = unname(prueba_ljung_box$statistic),
    Ljung_Box_p = prueba_ljung_box$p.value,
    Ljung_Box_Cuadrados_Q = unname(prueba_ljung_box_cuadrados$statistic),
    Ljung_Box_Cuadrados_p = prueba_ljung_box_cuadrados$p.value,
    Heterocedasticidad_LM = estadistico_heterocedasticidad,
    Heterocedasticidad_gl = gl_heterocedasticidad,
    Heterocedasticidad_p = p_heterocedasticidad,
    Jarque_Bera = estadistico_jb,
    Jarque_Bera_p = p_jb,
    Asimetria = asimetria_residuos,
    Curtosis = curtosis_residuos,
    row.names = NULL
  )
}

residuos_train = Y_train - pred_train
residuos_test = Y_test - pred_test

tabla_diagnosticos = rbind(calcular_diagnosticos_residuos(Y_train, pred_train, "Entrenamiento"),
  calcular_diagnosticos_residuos(Y_test, pred_test, "Prueba"))

print(tabla_diagnosticos)

datos_diagnostico = rbind(
  data.frame(
    Fecha = fechas_train,
    Muestra = "Entrenamiento",
    Observado = Y_train,
    Ajustado = pred_train,
    Residuo = residuos_train
  ),
  data.frame(
    Fecha = fechas_test,
    Muestra = "Prueba",
    Observado = Y_test,
    Ajustado = pred_test,
    Residuo = residuos_test
  )
)

grafico_residuos_ajustados = ggplot(
  datos_diagnostico,
  aes(x = Ajustado, y = Residuo)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(alpha = 0.45, size = 1.2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  facet_wrap(~ Muestra, scales = "free") +
  labs(
    title = "Residuos frente a valores ajustados",
    x = "Valor ajustado",
    y = "Residuo"
  ) +
  theme_minimal()

grafico_qq_residuos = ggplot(
  datos_diagnostico,
  aes(sample = Residuo)
) +
  stat_qq(alpha = 0.5, size = 1.2) +
  stat_qq_line(color = "red") +
  facet_wrap(~ Muestra, scales = "free") +
  labs(
    title = "Grafico QQ de los residuos",
    x = "Cuantiles teoricos",
    y = "Cuantiles observados"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(margin = margin(b = 18)),
    strip.text = element_text(margin = margin(t = 8, b = 8))
  )

directorio_diagnosticos = "resultados_diagnostico_ridge"
dir.create(directorio_diagnosticos, showWarnings = FALSE)

write.csv(
  tabla_diagnosticos,
  file.path(directorio_diagnosticos, "tabla_diagnosticos_residuos.csv"),
  row.names = FALSE
)

write.csv(
  datos_diagnostico,
  file.path(directorio_diagnosticos, "residuos_ridge.csv"),
  row.names = FALSE
)

ggsave(
  file.path(directorio_diagnosticos, "ridge-residuos-ajustados.png"),
  grafico_residuos_ajustados,
  width = 9,
  height = 5.5,
  dpi = 300
)

ggsave(
  file.path(directorio_diagnosticos, "ridge-qq-residuos.png"),
  grafico_qq_residuos,
  width = 9,
  height = 5.5,
  dpi = 300
)

png(
  file.path(directorio_diagnosticos, "ridge-acf-residuos.png"),
  width = 1800,
  height = 1400,
  res = 220
)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
acf(
  residuos_train,
  lag.max = min(40L, length(residuos_train) - 1L),
  main = "ACF residuos - entrenamiento"
)
acf(
  residuos_train^2,
  lag.max = min(40L, length(residuos_train) - 1L),
  main = "ACF residuos al cuadrado - entrenamiento"
)
acf(
  residuos_test,
  lag.max = min(40L, length(residuos_test) - 1L),
  main = "ACF residuos - prueba"
)
acf(
  residuos_test^2,
  lag.max = min(40L, length(residuos_test) - 1L),
  main = "ACF residuos al cuadrado - prueba"
)
dev.off()

resultado_ridge$diagnosticos_residuos = tabla_diagnosticos
resultado_ridge$residuos = datos_diagnostico

saveRDS(
  resultado_ridge,
  file.path(directorio_diagnosticos, "resultado_ridge_completo.rds")
)

cat("\nDiagnosticos de residuos guardados en:",
    normalizePath(directorio_diagnosticos), "\n")
