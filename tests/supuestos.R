## Instala si falta cada paquete y luego lo carga
if (!requireNamespace("readr",   quietly = TRUE)) install.packages("readr")
if (!requireNamespace("dplyr",   quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("janitor", quietly = TRUE)) install.packages("janitor")
if (!requireNamespace("broom",   quietly = TRUE)) install.packages("broom")

library(readr)
library(dplyr)
library(janitor)
library(broom)

####------------------------------------------------------------
#### Leer CSV y limpiar nombres a snake_case
####   - Se normalizan nombres para evitar espacios/símbolos raros
####------------------------------------------------------------

raw <- readr::read_csv("FinalCleanedDatasetAfricaMalaria.csv", show_col_types = FALSE)
dataset <- janitor::clean_names(raw)

####------------------------------------------------------------
#### Detectar columnas por patrón y renombrar a corto
####   - Los títulos pueden variar (p.ej., "safely_managed" vs "at_least_basic"),
####     por eso buscamos por expresiones regulares en nombres normalizados.
####------------------------------------------------------------

nm <- names(dataset)

# Incidencia por 1.000 personas en riesgo
col_inc   <- nm[grepl("^incidence_of_malaria.*population_at_risk$", nm, ignore.case = TRUE)]

# Agua segura: acepta "safely_managed" o "at_least_basic"
col_water <- nm[grepl("^people_using_.*drinking_water_services.*_of_population$", nm, ignore.case = TRUE)]

# Saneamiento: acepta "safely_managed" o "at_least_basic"
col_san   <- nm[grepl("^people_using_.*sanitation_services.*_of_population$", nm, ignore.case = TRUE)]

# % de población urbana
col_urban <- nm[grepl("^urban_population.*_of_total_population$", nm, ignore.case = TRUE)]

# País y año
col_country <- nm[grepl("^country_name$", nm, ignore.case = TRUE)]
col_year    <- nm[grepl("^year$",         nm, ignore.case = TRUE)]

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
    country         = dplyr::all_of(col_country[1]),
    year            = dplyr::all_of(col_year[1]),
    incidence       = dplyr::all_of(col_inc[1]),
    safe_water_pct  = dplyr::all_of(col_water[1]),
    sanitation_pct  = dplyr::all_of(col_san[1]),
    urban_pop_pct   = dplyr::all_of(col_urban[1])
  ) %>%
  mutate(
    incidence       = suppressWarnings(as.numeric(incidence)),
    safe_water_pct  = suppressWarnings(as.numeric(safe_water_pct)),
    sanitation_pct  = suppressWarnings(as.numeric(sanitation_pct)),
    urban_pop_pct   = suppressWarnings(as.numeric(urban_pop_pct))
  )

# Chequeo visual de que los nombres cortos estén presentes
print(names(dataset))

####------------------------------------------------------------
#### Selección del año de análisis 
####------------------------------------------------------------

year_target <- 2017
dyear <- dataset %>% filter(year == year_target)

# Independencia por diseño (1 país = 1 fila en ese año). Se hace por si acaso un Chequeo simple de duplicados.
dup_chk <- dyear %>% count(country, name = "n") %>% filter(n > 1)
if (nrow(dup_chk) > 0) {
  warning("Hay países duplicados para el año elegido. Revisar independencia por diseño.")
}

####------------------------------------------------------------
#### Función para evaluar supuestos de Pearson (salvo linealidad)
####   - Normalidad: Shapiro–Wilk en X, Y y residuales del modelo Y ~ X
####   - Homocedasticidad: test tipo White (auxiliar e^2 ~ ŷ + ŷ^2)
####   - Gráficos: Residuales vs Ajustados y QQ-plot de residuales
####   - Nota: Independencia se justifica por diseño (corte transversal)
####------------------------------------------------------------

check_pearson_assumptions <- function(df, xvar, yvar = "incidence", make_plots = TRUE) {
  # Subset consistente (sin NA en la pareja)
  dd <- df %>% select(dplyr::all_of(c(yvar, xvar))) %>% tidyr::drop_na()
  if (nrow(dd) < 5) {
    return(data.frame(
      pair                = paste(yvar, "~", xvar),
      n_used              = nrow(dd),
      shapiro_y_p         = NA_real_,
      shapiro_x_p         = NA_real_,
      shapiro_residuals_p = NA_real_,
      white_like_p        = NA_real_,
      stringsAsFactors    = FALSE
    ))
  }
  
  # Normalidad univariante (X y Y) y de residuales del ajuste lineal
  sh_y <- tryCatch(shapiro.test(dd[[yvar]]), error = function(e) NULL)
  sh_x <- tryCatch(shapiro.test(dd[[xvar]]),  error = function(e) NULL)
  
  fit  <- lm(stats::reformulate(xvar, yvar), data = dd)
  res  <- resid(fit)
  fitv <- fitted(fit)
  sh_r <- tryCatch(shapiro.test(res), error = function(e) NULL)
  
  # Homocedasticidad tipo White (LM = n*R^2 de e^2 ~ ŷ + ŷ^2; p grande sugiere varianza constante)
  aux   <- lm(I(res^2) ~ fitv + I(fitv^2))
  n     <- nrow(dd)
  k     <- 2L  # fitv y fitv^2
  LM    <- n * summary(aux)$r.squared
  white <- pchisq(LM, df = k, lower.tail = FALSE)
  
  # Gráficos diagnósticos 
  if (make_plots) {
    op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
    par(mfrow = c(1,2), mar = c(4.5,4.5,1.6,1))
    plot(fitv, res, xlab = "Ajustados (ŷ)", ylab = "Residuales",
         main = paste("Res vs Ajustados:", yvar, "~", xvar))
    abline(h = 0, lty = 2)
    qqnorm(res, main = paste("QQ residuales:", yvar, "~", xvar)); qqline(res)
  }
  
  data.frame(
    pair                = paste(yvar, "~", xvar),
    n_used              = nrow(dd),
    shapiro_y_p         = if (!is.null(sh_y)) sh_y$p.value else NA_real_,
    shapiro_x_p         = if (!is.null(sh_x)) sh_x$p.value else NA_real_,
    shapiro_residuals_p = if (!is.null(sh_r)) sh_r$p.value else NA_real_,
    white_like_p        = white,
    stringsAsFactors    = FALSE
  )
}

####------------------------------------------------------------
#### Ejecutar evaluación de supuestos para cada predictor
####   - Se evalúa Incidence ~ sanitation_pct, ~ safe_water_pct, ~ urban_pop_pct
####   - Interpretación básica:
####       * shapiro_residuals_p >= 0.05  => normalidad de errores razonable
####       * white_like_p       >= 0.05  => homocedasticidad plausible
####       * Independencia: declarada por diseño en corte transversal
####------------------------------------------------------------

predictors <- c("sanitation_pct", "safe_water_pct", "urban_pop_pct")

assump_tbl <- dplyr::bind_rows(lapply(predictors, function(x) {
  check_pearson_assumptions(dyear, xvar = x, make_plots = TRUE)
}))

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
    pair    = paste("incidence ~", xvar),
    n_used  = sum(keep),
    rho     = unname(ct$estimate),   # tamaño y dirección de la asociación
    p_value = ct$p.value,            # contraste H0: rho = 0 (dos colas por defecto)
    stringsAsFactors = FALSE
  )
}

# Ejecutar Spearman en las tres parejas y ajustar p-values por múltiples pruebas (BH)
tab <- do.call(rbind, lapply(pairs, spearman_row))
tab$p_adj_BH <- p.adjust(tab$p_value, method = "BH")

# Presentación: redondeo y orden por p ajustado
tab_out <- within(tab, {
  rho      <- round(rho, 3)
  p_value  <- signif(p_value, 3)
  p_adj_BH <- signif(p_adj_BH, 3)
})
tab_out <- tab_out[order(tab_out$p_adj_BH), ]
print(tab_out)

