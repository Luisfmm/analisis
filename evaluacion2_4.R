library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3) # reportar 3 dígitos significativos
mean(stars$magnitude)
sd(stars$magnitude)

# Gráfico de densidad de la magnitud
stars %>%
  ggplot(aes(x = magnitude)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(
    title = "Densidad de la Magnitud de las Estrellas",
    x = "Magnitud",
    y = "Densidad"
  ) +
  theme_minimal()
# Gráfico de densidad de la temperatura de las estrellas
stars %>%
  ggplot(aes(x = temp)) +
  geom_density(fill = "orange", alpha = 0.5) +
 # scale_x_log10() +
  labs(
    title = "Densidad de la Temperatura de las Estrellas (escala log)",
    x = "Temperatura (log10 Kelvin)",
    y = "Densidad"
  ) +
  theme_minimal()

stars %>%
  ggplot(aes(x = temp, y = magnitude)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  scale_x_log10() +         # Escala logarítmica en temperatura
  scale_x_reverse() +       # Más caliente a la izquierda
  scale_y_reverse() +       # Más brillante arriba
  labs(
    title = "Diagrama HR: Temperatura vs Magnitud",
    x = "Temperatura (K)",
    y = "Magnitud Aparente"
  ) +
  theme_minimal()

# Filtro para excluir la secuencia principal y las enanas blancas
# y quedarnos con estrellas brillantes pero frías (gigantes rojas dispersas)
gigantes_frias <- stars %>%
  filter(
    temp < 8000,     # baja temperatura
    magnitude < 1           # relativamente brillantes
  )

# Temperatura promedio aproximada
mean(gigantes_frias$temp)

stars %>%
  ggplot(aes(x = temp, y = magnitude)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  scale_x_log10(trans = "reverse") +  # Temperatura: caliente a la izquierda
  scale_y_reverse() +                 # Magnitud: más brillante arriba
  labs(
    title = "Diagrama HR con Regiones Estelares",
    x = "Temperatura (log10 K, caliente a la izquierda)",
    y = "Magnitud Aparente"
  ) +
  # Etiquetas de regiones estelares
  annotate("text", x = 10000, y = 15, label = "Enanas blancas", color = "gray30", size = 4) +
  annotate("text", x = 6000, y = 5, label = "Secuencia principal", color = "black", size = 4) +
  annotate("text", x = 3500, y = 0, label = "Gigantes rojas", color = "red4", size = 4) +
  theme_minimal()

stars %>%
  ggplot(aes(x = temp, y = magnitude, label = star)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_text(size = 3, vjust = -0.5, check_overlap = TRUE) +
  scale_x_log10(trans = "reverse") +
  scale_y_reverse() +
  labs(
    title = "Diagrama HR con nombres de estrellas",
    x = "Temperatura (log10 K, caliente a la izquierda)",
    y = "Magnitud Aparente"
  ) +
  theme_minimal()

stars %>%
  ggplot(aes(x = temp, y = magnitude, color = type)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_x_log10(trans = "reverse") +
  scale_y_reverse() +
  scale_color_manual(values = c(
    "#000000", "#AAAAAA", "#0022BB", "#22BB00", "#CCCCCC",
    "#CC00CC", "#CCCC00", "#00CCCC", "#CC0000", "#888888"
  )) +
  labs(
    title = "Diagrama HR coloreado por tipo de estrella",
    x = "Temperatura (log10 K, caliente a la izquierda)",
    y = "Magnitud Aparente",
    color = "Tipo de estrella"
  ) +
  theme_minimal()
