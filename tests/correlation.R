## Instala si falta cada paquete y luego lo carga
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("janitor", quietly = TRUE)) install.packages("janitor")
if (!requireNamespace("broom", quietly = TRUE)) install.packages("broom")

library(readr)
library(dplyr)
library(janitor)
library(broom)

####------------------------------------------------------------
#### Leer CSV y limpiar nombres a snake_case
#### - Se normalizan nombres para evitar espacios/símbolos raros
####------------------------------------------------------------

raw <- readr::read_csv("dbs/FinalCleanedDatasetAfricaMalaria.csv", show_col_types = FALSE)
dataset <- janitor::clean_names(raw)

####------------------------------------------------------------
#### Detectar columnas por patrón y renombrar a corto
#### - Los títulos pueden variar (p.ej., "safely_managed" vs "at_least_basic"),
#### por eso buscamos por expresiones regulares en nombres normalizados.
####------------------------------------------------------------

nm <- names(dataset)

# Incidencia por 1.000 personas en riesgo
col_inc <- nm[grepl("^incidence_of_malaria.*population_at_risk$", nm, ignore.case = TRUE)]

# Agua segura: acepta "safely_managed" o "at_least_basic"
col_water <- nm[grepl("^people_using_.*drinking_water_services.*_of_population$", nm, ignore.case = TRUE)]

# Saneamiento: acepta "safely_managed" o "at_least_basic"
col_san <- nm[grepl("^people_using_.*sanitation_services.*_of_population$", nm, ignore.case = TRUE)]

# % de población urbana
col_urban <- nm[grepl("^urban_population.*_of_total_population$", nm, ignore.case = TRUE)]

# País y año
col_country <- nm[grepl("^country_name$", nm, ignore.case = TRUE)]
col_year <- nm[grepl("^year$", nm, ignore.case = TRUE)]

# Chequeo claro: si falta alguna columna esperada, se detiene con mensaje descriptivo
found <- lengths(list(col_country,col_year,col_inc,col_water,col_san,col_urban)) > 0
if (!all(found)) {
  stop(sprintf(
    "No se encontraron todas las columnas esperadas.\nFaltan: %s",
    paste(c("country_name","year","incidence","water","sanitation","urban")[!found], collapse=", ")
  ))
}

# Renombrar a corto y forzar numérico en las 4 variables de interés
dataset <- dataset %>%
  rename(
    country = dplyr::all_of(col_country[1]),
    year = dplyr::all_of(col_year[1]),
    incidence = dplyr::all_of(col_inc[1]),
    safe_water_pct = dplyr::all_of(col_water[1]),
    sanitation_pct = dplyr::all_of(col_san[1]),
    urban_pop_pct = dplyr::all_of(col_urban[1])
  ) %>%
  mutate(
    incidence = suppressWarnings(as.numeric(incidence)),
    safe_water_pct = suppressWarnings(as.numeric(safe_water_pct)),
    sanitation_pct = suppressWarnings(as.numeric(sanitation_pct)),
    urban_pop_pct = suppressWarnings(as.numeric(urban_pop_pct))
  )

# Chequeo visual de que los nombres cortos estén presentes
print(names(dataset))

####------------------------------------------------------------
#### Selección del año de análisis
####------------------------------------------------------------

year_target <- 2017
dyear <- dataset %>% filter(year == year_target)


#' 5. **Independencia de las observaciones**
#' Esta es una suposición clave tanto para Pearson como para Spearman.
#' Al filtrar por un solo año (`year_target`), cada fila representa un país diferente.
#' Esto se conoce como "diseño de corte transversal" (cross-sectional).
#' Este diseño garantiza que la observación de un país (ej. Angola) no influye
#' en la observación de otro (ej. Egipto).
#' La siguiente comprobación de duplicados verifica esta independencia por diseño.

dup_chk <- dyear %>% count(country, name = "n") %>% filter(n > 1)
if (nrow(dup_chk) > 0) {
  warning("Hay países duplicados para el año elegido. Revisar independencia por diseño.")
}

####------------------------------------------------------------
#### Función para evaluar supuestos de Pearson
####------------------------------------------------------------

#'
#' ## Supuestos de Correlación de Pearson (r)
#'
#' A continuación, se evalúan los supuestos clásicos de una relación lineal.
#' La correlación de Pearson es una medida de asociación, pero para
#' realizar una prueba de hipótesis (inferencia) sobre ella, se asumen
#' condiciones similares a las de un Modelo de Regresión Lineal (lm).
#' Esta función (`check_pearson_assumptions`) evalúa formalmente dichos supuestos
#' (como normalidad y homocedasticidad) para justificar la elección del test.
#'
#' 1.  **Variables cuantitativas y continuas**:
#'     Nuestras variables (`incidence`, `sanitation_pct`, etc.) cumplen esto.
#'
#' 2.  **Relación lineal (X vs Y)**:
#'     La relación debe ser lineal. Esto se verifica visualmente con un
#'     diagrama de dispersión. Si es una curva, Pearson no es adecuado.
#'
#' 3.  **Normalidad (idealmente bivariada)**:
#'     Pearson asume que los datos provienen de una distribución normal.
#'     La función lo chequea para cada variable (univariante):
#'     - `Y`: La variable dependiente (en nuestro caso, `incidence`).
#'     - `X`: Las variables independientes (ej. `sanitation_pct`, `safe_water_pct`).
#'
#' 4.  **Homocedasticidad (varianza constante)**:
#'     La variabilidad de los errores debe ser constante (nube de puntos
#'     sin forma de cono). La función usa un test tipo White para esto.
#'
#' 5.  **Independencia de las observaciones**:
#'     Ya verificado por el diseño de corte transversal (año 2017).
#'
#' 6.  **Ausencia de outliers extremos**:
#'     Los QQ-plots ayudan a detectar valores atípicos que pueden
#'     distorsionar el coeficiente de Pearson.
#' 

check_pearson_assumptions <- function(df, xvar, yvar = "incidence",
                                      make_plots = TRUE, qq_residuals = FALSE) {
  # Submuestra consistente (mismas filas sin NA para X e Y)
  dd <- df %>%
    dplyr::select(dplyr::all_of(c(yvar, xvar))) %>%
    tidyr::drop_na()
  
  if (nrow(dd) < 5) {
    return(data.frame(
      pair = paste(yvar, "~", xvar),
      n_used = nrow(dd),
      shapiro_y_p = NA_real_, # diagnóstico univariante (Y)
      shapiro_x_p = NA_real_, # diagnóstico univariante (X)
      white_like_p = NA_real_, # homocedasticidad
      stringsAsFactors = FALSE
    ))
  }
  
  # Diagnóstico univariante (Shapiro) — no condiciona la validez de Pearson
  #' 3. **Normalidad (en Y)**: Verificación univariante.
  sh_y <- tryCatch(shapiro.test(dd[[yvar]]), error = function(e) NULL)
  #' 3. **Normalidad (en X)**: Verificación univariante.
  sh_x <- tryCatch(shapiro.test(dd[[xvar]]), error = function(e) NULL)
  
  # Ajuste lineal SOLO para obtener residuales (White) — sin gráfico de Res vs ŷ
  fit <- lm(stats::reformulate(xvar, yvar), data = dd)
  res <- resid(fit)
  fitv <- fitted(fit)
  
  # Homocedasticidad tipo White (LM = n*R^2 de e^2 ~ ŷ + ŷ^2)
  #' H0: La varianza es constante (Homocedasticidad).
  #' Si p < 0.05, se rechaza H0 y tenemos Heterocedasticidad (problema).
  aux <- lm(I(res^2) ~ fitv + I(fitv^2))
  n <- nrow(dd); k <- 2L
  LM <- n * summary(aux)$r.squared
  white <- pchisq(LM, df = k, lower.tail = FALSE)
  
  # QQ-plots: Y y X (univariantes)
  if (make_plots) {
    # Si el dispositivo es chico (ej. chunks Rmd), abrir uno más grande para evitar "figure margins too large"
    sz <- try(dev.size("in"), silent = TRUE)
    if (inherits(sz, "try-error") || any(is.na(sz)) || sz[1] < 6 || sz[2] < 4) {
      suppressWarnings(try(dev.new(width = 8, height = 5), silent = TRUE))
    }
    op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
    
    if (qq_residuals) {
      par(mfrow = c(1,3), mar = c(4.5,4.5,1.6,1))
    } else {
      par(mfrow = c(1,2), mar = c(4.5,4.5,1.6,1))
    }
    
    #' 6. **Ausencia de outliers / 3. Normalidad (Visual)**
    #' El QQ-plot compara los datos con una normal perfecta (la línea).
    #' Si los puntos se alejan mucho de la línea (especialmente en los
    #' extremos), indica falta de normalidad y posibles outliers.
    
    # QQ de Y
    qqnorm(dd[[yvar]], main = paste("QQ de", yvar)); qqline(dd[[yvar]])
    # QQ de X
    qqnorm(dd[[xvar]], main = paste("QQ de", xvar)); qqline(dd[[xvar]])
    # (Opcional) QQ de residuales
    if (qq_residuals) {
      qqnorm(res, main = paste("QQ de residuales:", yvar, "~", xvar)); qqline(res)
    }
  }
  
  data.frame(
    pair = paste(yvar, "~", xvar),
    n_used = nrow(dd),
    shapiro_y_p = if (!is.null(sh_y)) sh_y$p.value else NA_real_,
    shapiro_x_p = if (!is.null(sh_x)) sh_x$p.value else NA_real_,
    white_like_p = white,
    stringsAsFactors = FALSE
  )
}

####------------------------------------------------------------
#### Ejecutar la evaluación de supuestos de Pearson
####------------------------------------------------------------

predictors <- c("sanitation_pct", "safe_water_pct", "urban_pop_pct")

assump_tbl <- dplyr::bind_rows(lapply(predictors, function(x) {
  # qq_residuals = FALSE para NO mostrar QQ de residuales (solo Y y X)
  check_pearson_assumptions(dyear, xvar = x, make_plots = TRUE, qq_residuals = FALSE)
}))

#' La tabla 'assump_tbl' mostrará los p-values de los tests de Shapiro
#' (shapiro_y_p, shapiro_x_p) y Homocedasticidad (white_like_p).
#' Si algún p-value de Shapiro es < 0.05, se viola el supuesto de normalidad.
#' Si white_like_p < 0.05, se viola el supuesto de homocedasticidad.
print(assump_tbl)


#--- spearman -----


# Predictores a contrastar contra `incidence`
pairs <- c("sanitation_pct", "safe_water_pct", "urban_pop_pct")

# Función: corre Spearman para una pareja (incidence ~ xvar)
spearman_row <- function(xvar) {
  keep <- stats::complete.cases(dyear$incidence, dyear[[xvar]])
  ct <- suppressWarnings(
    cor.test(dyear$incidence[keep], dyear[[xvar]][keep],
             method = "spearman", exact = FALSE, use = "pairwise.complete.obs")
  )
  data.frame(
    pair = paste("incidence ~", xvar),
    n_used = sum(keep),
    rho = unname(ct$estimate), # tamaño y dirección de la asociación
    p_value = ct$p.value, # contraste H0: rho = 0 (dos colas por defecto)
    stringsAsFactors = FALSE
  )
}

# Ejecutar Spearman en las tres parejas y ajustar p-values por múltiples pruebas (BH)
tab <- do.call(rbind, lapply(pairs, spearman_row))
tab$p_adj_BH <- p.adjust(tab$p_value, method = "BH")

# Presentación: redondeo y orden por p ajustado
tab_out <- within(tab, {
  rho <- round(rho, 3)
  p_value <- signif(p_value, 3)
  p_adj_BH <- signif(p_adj_BH, 3)
})
tab_out <- tab_out[order(tab_out$p_adj_BH), ]
print(tab_out)
