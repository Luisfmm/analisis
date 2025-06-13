options(digits = 3) # reportar 3 cifras significativas
library(tidyverse)
library(titanic)
library(dplyr)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
titanic_train

library(ggplot2)
library(titanic)  # asumiendo que el dataset está cargado aquí
data("titanic_train")  # cargamos el dataset Titanic (train)

# Quitamos NA en Age para que el gráfico funcione bien
titanic_clean <- titanic_train[!is.na(titanic_train$Age), ]

# Gráfico de densidad de Age por Sex
ggplot(titanic_clean, aes(x = Age, fill = Sex)) +
  geom_density(alpha = 0.5) +          # alpha para transparencia
  labs(title = "Distribución de Edad por Sexo en Titanic",
       x = "Edad",
       y = "Densidad") +
  theme_minimal()
table(titanic_train$Sex)
prop.table(table(titanic_train$Sex[titanic_train$Age >= 18 & titanic_train$Age <= 35]))
table(titanic_train$Sex[titanic_train$Age == 40])
titanic_train[which.max(titanic_train$Age), ]
table(titanic_train$Sex[titanic_train$Age < 18])

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

