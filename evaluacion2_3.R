# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(titanic)

# Preparar datos
data <- titanic::titanic_train %>%
  filter(Fare > 0) %>%                          # Eliminar tarifas de 0
  mutate(Survived = factor(Survived, labels = c("No", "Yes")))  # Etiquetas legibles

# Crear boxplot con jitter y transformación log2
ggplot(data, aes(x = Survived, y = Fare)) +
  geom_boxplot(outlier.shape = NA) +            # Boxplot sin outliers
  geom_jitter(width = 0.2, alpha = 0.3, color = "blue") +  # Jitter con alfa
  scale_y_continuous(trans = "log2") +          # Transformación log2 del eje y
  ylab("Fare (log2)") +
  xlab("Survived") +
  ggtitle("Distribución de Tarifa según Supervivencia (log2)") +
  theme_minimal()

# Calcular IQR excluyendo tarifas iguales a 0
iqr_fare <- titanic::titanic_train %>%
  filter(Fare > 0) %>%
  summarize(IQR = IQR(Fare))

# Filtramos para quitar tarifas cero o NA
titanic_filtered <- titanic %>%
  filter(!is.na(Fare) & Fare > 0)

# Calculamos Q1, Q2 (mediana), Q3 y el IQR
cuartiles <- titanic_filtered %>%
  summarize(
    Q1 = quantile(Fare, 0.25),
    Median = quantile(Fare, 0.5),
    Q3 = quantile(Fare, 0.75),
    IQR = IQR(Fare)
  )

print(cuartiles)

titanic %>%
  group_by(Pclass) %>%
  summarize(
    total_pasajeros = n(),
    sobrevivientes = sum(Survived == 1, na.rm = TRUE)
  )

# 1. Gráfico básico de Pclass relleno por Survived (conteos)
p1 <- ggplot(titanic, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar() +
  labs(x = "Clase del pasajero", fill = "Supervivió") +
  ggtitle("Conteo básico de clase por supervivencia")

# 2. Mismo gráfico pero usando position_fill() para proporciones relativas (por clase)
p2 <- ggplot(titanic, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = position_fill()) +
  labs(x = "Clase del pasajero", fill = "Supervivió", y = "Proporción") +
  ggtitle("Proporción de supervivencia dentro de cada clase")

# 3. Gráfico de supervivencia rellenado por clase, con position_fill() para proporciones relativas (por supervivencia)
p3 <- ggplot(titanic, aes(x = factor(Survived), fill = factor(Pclass))) +
  geom_bar(position = position_fill()) +
  labs(x = "Supervivió", fill = "Clase del pasajero", y = "Proporción") +
  ggtitle("Proporción de clases dentro de cada grupo de supervivencia")

# Mostrar gráficos
print(p1)
print(p2)
print(p3)

# Filtrar datos con edad no NA
titanic_filtered <- titanic %>% filter(!is.na(Age))

# Gráfico de densidad con facetado por Sex y Pclass
ggplot(titanic_filtered, aes(x = Age, fill = factor(Survived))) +
  geom_density(alpha = 0.3, position = "identity") +
  facet_grid(Sex ~ Pclass) +
  scale_y_continuous(name = "Recuento") +
  scale_fill_manual(values = c("red", "green"), 
                    name = "Sobrevivió", 
                    labels = c("No", "Sí")) +
  labs(x = "Edad", title = "Distribución de edad según sexo, clase y supervivencia") +
  theme_minimal()

ggplot(data, aes(x = Age, fill = Survived)) +
  geom_density(alpha = 0.2, position = "identity", aes(y = ..count..))
