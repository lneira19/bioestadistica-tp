# Lectura del .CSV
dataset = read.csv(
  "dbs/FinalCleanedDatasetAfricaMalaria.csv", 
  header=TRUE, 
  sep=",")

# Convertir a Data Frame
dataset = as.data.frame(dataset)
col_names = colnames(dataset)
col_names

# Averiguamos el tipo de dato de cada columna
column_types = sapply(dataset, class)
for (i in 1:length(column_types)) {
  cat("Columna:", names(column_types)[i], "\n")
  cat("- Tipo de dato: ", column_types[i], "\n", sep="")
}

# --- Carga y filtrado de datos (basado en tu script) ---
# Asegúrate de haber corrido estas líneas primero

# --- Verificación de Supuestos: Test de Normalidad de Shapiro-Wilk ---

# La Hipótesis Nula (H₀) de este test es que los datos *sí* siguen una distribución normal.
# - Si p < 0.05: Rechazamos H₀. Los datos NO son normales.
# - Si p > 0.05: No podemos rechazar H₀. Los datos SON (o podrían ser) normales.

year_of_interest = 2016
year_data = subset(dataset, Year == year_of_interest)

cat("--- 1. Test de Normalidad: Incidencia de Malaria ---\n")
# Es muy probable que este p-value sea < 0.05
shapiro_test_malaria <- shapiro.test(year_data$Incidence.of.malaria..per.1.000.population.at.risk.)
print(shapiro_test_malaria)
cat("\n")


cat("--- 2. Test de Normalidad: Población Urbana ---\n")
shapiro_test_urbana <- shapiro.test(year_data$Urban.population....of.total.population.)
print(shapiro_test_urbana)
cat("\n")


cat("--- 3. Test de Normalidad: Acceso a Agua ---\n")
shapiro_test_agua <- shapiro.test(year_data$People.using.at.least.basic.drinking.water.services....of.population.)
print(shapiro_test_agua)
cat("\n")


cat("--- 4. Test de Normalidad: Acceso a Sanidad ---\n")
shapiro_test_sanidad <- shapiro.test(year_data$People.using.at.least.basic.sanitation.services....of.population.)
print(shapiro_test_sanidad)
cat("\n")


# Ver lo Restultados Obtenidos
cat("Resultados del Test de Normalidad de Shapiro-Wilk para el año", year_of_interest, ":\n")
cat("1. Incidencia de Malaria: p-value =", shapiro_test_malaria$p.value, "\n")
cat("2. Población Urbana: p-value =", shapiro_test_urbana$p.value, "\n")
cat("3. Acceso a Agua: p-value =", shapiro_test_agua$p.value, "\n")
cat("4. Acceso a Sanidad: p-value =", shapiro_test_sanidad$p.value, "\n")
cat("\n")
cat("Interpretación:\n")
cat("- Si el p-value es menor a 0.05, rechazamos la hipótesis nula y concluimos que los datos no siguen una distribución normal.\n")
cat("- Si el p-value es mayor a 0.05, no podemos rechazar la hipótesis nula y concluimos que los datos podrían seguir una distribución normal.\n")
cat("\n")
