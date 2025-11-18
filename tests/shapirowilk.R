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

# --- Verificación de Supuestos: Test de Normalidad de Shapiro-Wilk ---

# La Hipótesis Nula (H₀) de este test es que los datos *sí* siguen una distribución normal.
# La Hipótesis Alternativa (H₁) es que los datos *no* siguen una distribución normal.

# - Si p < 0.05: Rechazamos H₀, entonces los datos NO son normales.
# - Si p > 0.05: No podemos rechazar H₀, entonces los datos SÍ son normales.

year_of_interest = 2016
year_data = subset(dataset, Year == year_of_interest)

print("--- 1. Test de Normalidad: Incidencia de Malaria ---")
shapiro_test_malaria <- shapiro.test(year_data$Incidence.of.malaria..per.1.000.population.at.risk.)
print(shapiro_test_malaria)


print("--- 2. Test de Normalidad: Población Urbana ---")
shapiro_test_urbana <- shapiro.test(year_data$Urban.population....of.total.population.)
print(shapiro_test_urbana)


print("--- 3. Test de Normalidad: Acceso a Agua ---")
shapiro_test_agua <- shapiro.test(year_data$People.using.at.least.basic.drinking.water.services....of.population.)
print(shapiro_test_agua)


print("--- 4. Test de Normalidad: Acceso a Sanidad ---")
shapiro_test_sanidad <- shapiro.test(year_data$People.using.at.least.basic.sanitation.services....of.population.)
print(shapiro_test_sanidad)


# Ver lo Restultados Obtenidos
cat(
  "Resultados del Test de Normalidad de Shapiro-Wilk para el año", year_of_interest, ":\n",
  "1. Incidencia de Malaria: p-value =", shapiro_test_malaria$p.value, "\n",
  "2. Población Urbana: p-value =", shapiro_test_urbana$p.value, "\n",
  "3. Acceso a Agua: p-value =", shapiro_test_agua$p.value, "\n",
  "4. Acceso a Sanidad: p-value =", shapiro_test_sanidad$p.value, "\n",
  "Interpretación:\n",
  "- Si el p-value es menor a 0.05, rechazamos la hipótesis nula y concluimos que los datos no siguen una distribución normal.\n",
  "- Si el p-value es mayor a 0.05, no podemos rechazar la hipótesis nula y concluimos que los datos podrían seguir una distribución normal.\n")
