# Cargar paquetes necesarios
library(ggplot2)
library(dplyr)
library(titanic)

# Usar el dataset titanic_train incluido en el paquete
data <- titanic::titanic_train

# Calcular media y desviación estándar de la edad (sin NA)
params <- data %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

# Crear QQ-plot
data %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = list(mean = params$mean, sd = params$sd)) +
  geom_abline() +
  labs(title = "QQ-Plot de la Edad de los Pasajeros del Titanic",
       x = "Cuantiles Teóricos",
       y = "Cuantiles Observados")


# Convertir Survived a factor para mejor visualización
data$Survived <- factor(data$Survived, labels = c("No", "Yes"))

# Crear gráfico de barras con barras separadas por sexo
ggplot(data, aes(x = Survived, fill = Sex)) +
  geom_bar(position = position_dodge()) +
  labs(
    title = "Supervivencia en el Titanic por Sexo",
    x = "¿Sobrevivió?",
    y = "Cantidad de Pasajeros"
  ) +
  scale_fill_manual(values = c("lightblue", "pink")) +
  theme_minimal()


# Cargar los datos y limpiar NAs
data <- titanic::titanic_train %>%
  filter(!is.na(Age)) %>%
  mutate(Survived = factor(Survived, labels = c("No", "Yes")))

# Graficar densidad con conteo y relleno por estado de supervivencia
ggplot(data, aes(x = Age, fill = Survived)) +
  geom_density(alpha = 0.2, aes(y = ..count..)) +
  labs(
    title = "Distribución de Edad por Supervivencia",
    x = "Edad",
    y = "Conteo de Pasajeros",
    fill = "¿Sobrevivió?"
  ) +
  theme_minimal()


# Cargar datos y limpiar edades NA
data <- titanic::titanic_train %>%
  filter(!is.na(Age)) %>%
  mutate(Survived = factor(Survived, labels = c("No", "Yes")))

# Crear rangos de edad personalizados
data <- data %>%
  mutate(age_group = case_when(
    Age >= 0  & Age <= 8  ~ "0-8",
    Age > 8   & Age <= 18 ~ "10-18",
    Age > 18  & Age <= 30 ~ "18-30",
    Age > 30  & Age <= 50 ~ "30-50",
    Age > 50  & Age <= 70 ~ "50-70",
    Age > 70  & Age <= 80 ~ "70-80",
    TRUE ~ NA_character_
  ))

# Calcular proporción de sobrevivientes por grupo de edad
proporciones <- data %>%
  filter(!is.na(age_group)) %>%
  group_by(age_group) %>%
  summarize(
    total = n(),
    sobrevivientes = sum(Survived == "Yes"),
    proporcion_sobrevivientes = sobrevivientes / total
  )

print(proporciones)
