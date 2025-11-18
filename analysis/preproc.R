# Lectura del .CSV
dataset = read.csv(
  "dbs/DatasetAfricaMalaria.csv", 
  header=TRUE, 
  sep=",")

# Convertir a Data Frame
dataset = as.data.frame(dataset)

# Dimensiones del DF
dimensions = dim(dataset)
for (i in 1:length(dimensions)) {
  cat("Dimensión", i, ":", dimensions[i], "\n")
}

# Nombres de columnas del DF
columns_name = colnames(dataset)
print(columns_name)

# Tipo de variable para cada columna
column_types = sapply(dataset, class)
print(column_types)

# Valores únicos de columnas categóricas
categ_columns = c(1,2,3)
categ_dataset = dataset[, categ_columns]
unique_values = lapply(categ_dataset, unique)
unique_values

# Investigar valores faltantes
missing_values = sapply(dataset, function(x) sum(is.na(x)))
for (i in 1:length(missing_values)) {
  cat("Columna:", names(missing_values)[i], "\n")
  cat("- Valores faltantes: ", missing_values[i]," (",100*missing_values[i]/dimensions[1],"%) \n", sep="")
}

# DATASET REDUCIDO
# Variables de interés: país, año, incidencia de malaria cada 1000 habitantes, población urbana, acceso al agua potable, acceso a servicios de salud
reduced_dataset = dataset[, c(1,2,4,17,19,22)]

# 1- Se averiguan cuántos datos faltantes hay y en qué columnas
rds_missing_values = sapply(reduced_dataset, function(x) sum(is.na(x)))
for (i in 1:length(rds_missing_values)) {
  cat("Columna:", names(rds_missing_values)[i], "\n")
  cat("- Valores faltantes: ", rds_missing_values[i]," (",100*rds_missing_values[i]/dim(reduced_dataset)[1],"%) \n", sep="")
}

# 2- Se eliminan filas con datos faltantes en 'Incidencia'
cleaned_dataset = reduced_dataset[!is.na(reduced_dataset$Incidence.of.malaria..per.1.000.population.at.risk.), ]
countries_with_no_incidence_data = setdiff(unique(reduced_dataset$Country.Name), unique(cleaned_dataset$Country.Name))
cat("Países sin datos de incidencia de malaria:", countries_with_no_incidence_data)

cds_missing_values = sapply(cleaned_dataset, function(x) sum(is.na(x)))
for (i in 1:length(cds_missing_values)) {
  cat("Columna:", names(cds_missing_values)[i], "\n")
  cat("- Valores faltantes: ", cds_missing_values[i]," (",100*cds_missing_values[i]/dim(cleaned_dataset)[1],"%) \n", sep="")
}

# 3- Se obtiene un dataframe mostrando aquellas filas que poseen un dato faltante en alguna de las columnas restantes
final_missing_dataset = cleaned_dataset[!complete.cases(cleaned_dataset), ]
countries_with_missing_data = unique(final_missing_dataset$Country.Name)
cat("Países con datos faltantes en otras variables:", countries_with_missing_data)

# 4- Se hace una limpieza final del dataset eliminando los países con datos faltantes en las variables restantes
final_cleaned_dataset = cleaned_dataset[which(!cleaned_dataset$Country.Name %in% countries_with_missing_data), ]

final_dimensions = dim(final_cleaned_dataset)
cat("Dimensiones del dataset final limpio: ", final_dimensions[1], " filas y ", final_dimensions[2], " columnas.\n")

final_unique_values = lapply(final_cleaned_dataset[, c(1,2)], unique)
final_unique_values

# Guardar el dataset limpio en un nuevo archivo .CSV
write.csv(final_cleaned_dataset, "dbs/FinalCleanedDatasetAfricaMalaria.csv", row.names=FALSE)
