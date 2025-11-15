# Módulos
library(ggplot2)

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

# Visualización de datos

# 1- Evolución de las variables para cada año
# BOXPLOT DE INCIDENCIA DE MALARIA PARA CADA AÑO
ggplot(dataset, aes(x=factor(Year), y=Incidence.of.malaria..per.1.000.population.at.risk.)) +
  geom_boxplot(fill="lightblue", color="darkblue") +
  labs(title="Incidencia de malaria en África (2007-2017)",
       x="Año",
       y="Indicencia cada 1000 habitantes en riesgo") +
  theme_minimal()
# BOXPLOT DE POBLACIÓN URBANA PARA CADA AÑO
ggplot(dataset, aes(x=factor(Year), y=Urban.population....of.total.population.)) +
  geom_boxplot(fill="lightgreen", color="darkgreen") +
  labs(title="Población urbana en África (2007-2017)",
       x="Año",
       y="Población urbana (% del total)") +
  ylim(0, 100) +
  theme_minimal()
# BOXPLOT DE ACCESO A AGUA POTABLE PARA CADA AÑO
ggplot(dataset, aes(x=factor(Year), y=People.using.at.least.basic.drinking.water.services....of.population.)) +
  geom_boxplot(fill="lightcoral", color="darkred") +
  labs(title="Acceso a agua potable en África (2007-2017)",
       x="Año",
       y="Acceso a agua potable (% del total)") +
  ylim(0, 100) +
  theme_minimal()
# BOXPLOT DE ACCESO A SERVICIOS DE SALUD PARA CADA AÑO
ggplot(dataset, aes(x=factor(Year), y=People.using.at.least.basic.sanitation.services....of.population.)) +
  geom_boxplot(fill="lightyellow", color="darkorange") +
  labs(title="Acceso a servicios de salud en África (2007-2017)",
       x="Año",
       y="Acceso a servicios de salud (% del total)") +
  ylim(0, 100) +
  theme_minimal()

# 2- Para un año en particular
year_of_interest = 2012
year_data = subset(dataset, Year == year_of_interest)

# Resumen estadístico
summary(year_data)
for (i in 3:ncol(year_data)) {
  cat("Columna:", colnames(year_data)[i], "\n")
  cat("- Media: ", mean(year_data[[i]], na.rm=TRUE), "\n", sep="")
  cat("- Mediana: ", median(year_data[[i]], na.rm=TRUE), "\n", sep="")
  cat("- Desviación estándar: ", sd(year_data[[i]], na.rm=TRUE), "\n", sep="")
  cat("- Mínimo: ", min(year_data[[i]], na.rm=TRUE), "\n", sep="")
  cat("- Máximo: ", max(year_data[[i]], na.rm=TRUE), "\n\n", sep="")
}

# 2.a - HISTOGRAMAS
# HISTOGRAMA DE INCIDENCIA DE MALARIA
ggplot(year_data, aes(x=Incidence.of.malaria..per.1.000.population.at.risk.)) +
  geom_histogram(binwidth=50, fill="lightblue", color="darkblue", boundary=0, closed="left") +
  scale_x_continuous(breaks=seq(0, 50+max(year_data$Incidence.of.malaria..per.1.000.population.at.risk., na.rm=TRUE), by=50)) +
  scale_y_continuous(breaks=seq(0, dim(year_data)[1], by=1)) +
  labs(title=paste("Histograma de incidencia de malaria en África en", year_of_interest),
       x="Incidencia cada 1000 habitantes en riesgo",
       y="Frecuencia") +
  theme_minimal()

# HISTOGRAMA DE POBLACIÓN URBANA
ggplot(year_data, aes(x=Urban.population....of.total.population.)) +
  geom_histogram(binwidth=10, fill="lightgreen", color="darkgreen", boundary=0, closed="left") +
  scale_x_continuous(breaks=seq(0, 100, by=10)) +
  scale_y_continuous(breaks=seq(0, dim(year_data)[1], by=1)) +
  labs(title=paste("Histograma de población urbana en África en", year_of_interest),
       x="Población urbana (% del total)",
       y="Frecuencia") +
  theme_minimal()
# HISTOGRAMA DE ACCESO A AGUA POTABLE
ggplot(year_data, aes(x=People.using.at.least.basic.drinking.water.services....of.population.)) +
  geom_histogram(binwidth=10, fill="lightcoral", color="darkred", boundary=0, closed="left") +
  scale_x_continuous(breaks=seq(0, 100, by=10)) +
  scale_y_continuous(breaks=seq(0, dim(year_data)[1], by=1)) +
  labs(title=paste("Histograma de acceso a agua potable en África en", year_of_interest),
       x="Acceso a agua potable (% del total)",
       y="Frecuencia") +
  theme_minimal()
# HISTOGRAMA DE ACCESO A SERVICIOS DE SALUD
ggplot(year_data, aes(x=People.using.at.least.basic.sanitation.services....of.population.)) +
  geom_histogram(binwidth=10, fill="lightyellow", color="darkorange", boundary=0, closed="left") +
  scale_x_continuous(breaks=seq(0, 100, by=10)) +
  scale_y_continuous(breaks=seq(0, dim(year_data)[1], by=1)) +
  labs(title=paste("Histograma de acceso a servicios de salud en África en", year_of_interest),
       x="Acceso a servicios de salud (% del total)",
       y="Frecuencia") +
  theme_minimal()

# 2.b - GRÁFICOS DE BARRAS
# INCIDENCIA DE MALARIA POR PAÍS EN EL AÑO DE INTERÉS
ggplot(year_data, aes(x=reorder(Country.Name, Incidence.of.malaria..per.1.000.population.at.risk.), y=Incidence.of.malaria..per.1.000.population.at.risk.)) +
  geom_bar(stat="identity", fill="lightblue", color="darkblue") +
  coord_flip() +
  labs(title=paste("Incidencia de malaria por país en África en", year_of_interest),
       x="País",
       y="Incidencia cada 1000 habitantes en riesgo") +
  scale_y_continuous(breaks=seq(0, 50+max(year_data$Incidence.of.malaria..per.1.000.population.at.risk., na.rm=TRUE), by=50)) +
  theme_minimal()

# POBLACIÓN URBANA POR PAÍS EN EL AÑO DE INTERÉS
ggplot(year_data, aes(x=reorder(Country.Name, Urban.population....of.total.population.), y=Urban.population....of.total.population.)) +
  geom_bar(stat="identity", fill="lightgreen", color="darkgreen") +
  coord_flip() +
  labs(title=paste("Población urbana por país en África en", year_of_interest),
       x="País",
       y="Población urbana (% del total)") +
  scale_y_continuous(breaks=seq(0, 100, by=10)) +
  theme_minimal()

# ACCESO A AGUA POTABLE POR PAÍS EN EL AÑO DE INTERÉS
ggplot(year_data, aes(x=reorder(Country.Name, People.using.at.least.basic.drinking.water.services....of.population.), y=People.using.at.least.basic.drinking.water.services....of.population.)) +
  geom_bar(stat="identity", fill="lightcoral", color="darkred") +
  coord_flip() +
  labs(title=paste("Acceso a agua potable por país en África en", year_of_interest),
       x="País",
       y="Acceso a agua potable (% del total)") +
  scale_y_continuous(breaks=seq(0, 100, by=10)) +
  theme_minimal()

# ACCESO A SERVICIOS DE SALUD POR PAÍS EN EL AÑO DE INTERÉS
ggplot(year_data, aes(x=reorder(Country.Name, People.using.at.least.basic.sanitation.services....of.population.), y=People.using.at.least.basic.sanitation.services....of.population.)) +
  geom_bar(stat="identity", fill="lightyellow", color="darkorange") +
  coord_flip() +
  labs(title=paste("Acceso a servicios de salud por país en África en", year_of_interest),
       x="País",
       y="Acceso a servicios de salud (% del total)") +
  scale_y_continuous(breaks=seq(0, 100, by=10)) +
  theme_minimal()

# 2.c - DIAGRAMAS DE DISPERSIÓN
# DIAGRAMA DE DISPERSIÓN: INCIDENCIA VS POBLACIÓN URBANA
ggplot(year_data, aes(x=Urban.population....of.total.population., y=Incidence.of.malaria..per.1.000.population.at.risk.)) +
  geom_point(color="darkblue") +
  labs(title=paste("Incidencia de malaria vs Población urbana en África en", year_of_interest),
       x="Población urbana (% del total)",
       y="Incidencia cada 1000 habitantes en riesgo") +
  xlim(0, 100) +
  theme_minimal()

# DIAGRAMA DE DISPERSIÓN: INCIDENCIA VS ACCESO A AGUA POTABLE
ggplot(year_data, aes(x=People.using.at.least.basic.drinking.water.services....of.population., y=Incidence.of.malaria..per.1.000.population.at.risk.)) +
  geom_point(color="darkred") +
  labs(title=paste("Incidencia de malaria vs Acceso a agua potable en África en", year_of_interest),
       x="Acceso a agua potable (% del total)",
       y="Incidencia cada 1000 habitantes en riesgo") +
  xlim(0, 100) +
  theme_minimal()

# DIAGRAMA DE DISPERSIÓN: INCIDENCIA VS ACCESO A SERVICIOS DE SALUD
ggplot(year_data, aes(x=People.using.at.least.basic.sanitation.services....of.population., y=Incidence.of.malaria..per.1.000.population.at.risk.)) +
  geom_point(color="darkorange") +
  labs(title=paste("Incidencia de malaria vs Acceso a servicios de salud en África en", year_of_interest),
       x="Acceso a servicios de salud (% del total)",
       y="Incidencia cada 1000 habitantes en riesgo") +
  xlim(0, 100) +
  theme_minimal()