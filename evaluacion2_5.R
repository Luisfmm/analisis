library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)
# Obtener el último año con datos no NA en emisiones de carbono
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
# max($year)
#  summarise(ultimo_anio = max(year))
#  select(year)%>%max()
  pull(year)%>%max()  

  # Filtrar solo los años con datos de emisiones
  carbon_data <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  arrange(year)

# Ver el primer y último año con sus emisiones
carbon_data %>%
  slice(c(1, n())) %>%
  select(year, carbon_emissions)  

carbon_data %>%
  summarise(
    diferencia = last(carbon_emissions) - first(carbon_emissions)
  )


# Filtrar años con datos de anomalía global
temp_data <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  arrange(year)

# Ver el primer y último año con sus anomalías
temp_data %>%
  slice(c(1, n())) %>%
  select(year, temp_anomaly)

temp_data %>%
  summarise(
    aumento_total = last(temp_anomaly) - first(temp_anomaly)
  )

# Crear gráfico y guardarlo en el objeto 'p'
p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(x = year, y = temp_anomaly)) +
  geom_line(color = "firebrick", size = 1) +
  labs(
    title = "Anomalía de Temperatura Global a lo Largo del Tiempo",
    x = "Año",
    y = "Anomalía de temperatura (°C)"
  ) +
  theme_minimal()

# Mostrar el gráfico
p
p + geom_hline(yintercept = 0, color = "blue", linetype = "dashed")

p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(x = year, y = temp_anomaly)) +
  geom_line(color = "firebrick", size = 1) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  annotate("text", x = 2000, y = 0.05, label = "Media del siglo XX", color = "blue") +
  labs(
    title = "Anomalía de temperatura relativa a la media del siglo XX, 1880–2018",
    x = "Año",
    y = "Anomalía de temperatura (grados C)"
  ) +
  theme_minimal()

# Crear gráfico con tres series de anomalía
p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = temp_anomaly, color = "Global"), size = 1) +
  geom_line(aes(y = ocean_anomaly, color = "Océano"), size = 1) +
  geom_line(aes(y = land_anomaly, color = "Tierra"), size = 1) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  geom_text(aes(x = 2000, y = 0.05, label = "Media del siglo XX"), color = "blue") +
  scale_color_manual(values = c("Global" = "firebrick", "Océano" = "steelblue", "Tierra" = "forestgreen")) +
  ggtitle("Anomalía de temperatura relativa a la media del siglo XX, 1880–2018") +
  ylab("Anomalía de temperatura (grados C)") +
  xlab("Año") +
  theme_minimal() +
  labs(color = "Superficie")

# Mostrar gráfico
p

data(greenhouse_gases)

# Crear gráfico con facet_grid y línea vertical
p <- greenhouse_gases %>%
  ggplot(aes(x = year, y = concentration)) +
  geom_line(color = "darkred") +
  geom_vline(xintercept = 1850, linetype = "dashed", color = "blue") +
  facet_grid(gas ~ ., scales = "free_y") +
  ggtitle("Concentración de gases de efecto invernadero a lo largo del tiempo") +
  ylab("Concentración (ppm para CO2, ppb para CH4 y N2O)") +
  xlab("Año") +
  theme_minimal()
p

# Calcular el cambio de concentración desde 1850 al último año por gas
greenhouse_gases %>%
  filter(year >= 1850) %>%
  group_by(gas) %>%
  summarise(
    inicio = concentration[year == min(year)],
    fin = concentration[year == max(year)],
    cambio = fin - inicio
  ) %>%
  arrange(cambio)



data(temp_carbon)

# Gráfico de línea de emisiones de carbono por año
p <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  ggplot(aes(x = year, y = carbon_emissions)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(
    title = "Emisiones de carbono a lo largo del tiempo",
    x = "Año",
    y = "Toneladas métricas de carbono emitidas por año"
  ) +
  theme_minimal()

# Mostrar gráfico
p
