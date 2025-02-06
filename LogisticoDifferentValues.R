library(deSolve)
library(ggplot2)

# Definir la ecuación diferencial
logistic_model <- function(t, x, params) {
  with(as.list(c(x, params)), {
    dx <- r * x * (1 - x / k)  # Ecuación logística
    list(dx)
  })
}

# Parámetros comunes
k <- 100              # Capacidad de carga (fija)
x0 <- c(x = 10)       # Población inicial
times <- seq(0, 200, by = 1)  # Extendemos el tiempo para ver mejor las diferencias

# Resolver para dos valores de r
r_values <- c(0.1, 0.01)
solutions <- list()

for (i in seq_along(r_values)) {
  params <- c(r = r_values[i], k = k)
  sol <- ode(y = x0, times = times, func = logistic_model, parms = params)
  solutions[[i]] <- data.frame(sol, r = paste("r =", r_values[i]))  # Agregar etiqueta
}

# Combinar todas las soluciones
combined_df <- do.call(rbind, solutions)

# Crear la gráfica
ggplot(combined_df, aes(x = time, y = x, color = r)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Crecimiento Logístico con Diferentes Tasas",
    x = "Tiempo",
    y = "Tamaño de la Población",
    color = "Tasa de crecimiento"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +  # Colores personalizados
  geom_hline(yintercept = k, linetype = "dashed", color = "gray50") +  # Línea de capacidad de carga
  annotate("text", x = max(times), y = k, label = paste("K =", k), 
           hjust = 1.1, vjust = -0.5, color = "gray30")
