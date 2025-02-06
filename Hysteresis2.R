library(ggplot2)

# Función para resolver numéricamente x³ - r x + h = 0 y extraer raíces reales
solve_cubic <- function(h, r) {
  # Coeficientes del polinomio: x³ + 0x² - r x + h = 0
  coeffs <- c(h, -r, 0, 1)  # Polinomio: h - r x + 0 x² + 1 x³
  roots <- polyroot(coeffs)  # Encuentra todas las raíces (reales y complejas)
  # Filtrar raíces reales (parte imaginaria cercana a cero)
  real_roots <- Re(roots[abs(Im(roots)) < 1e-6])
  return(real_roots)
}

# Generar datos para r < 0 y r > 0
set.seed(123)
h_values <- seq(-2, 2, by = 0.05)  # Rango de valores de h
r_neg <- -2                       # Ejemplo de r < 0
r_pos <- 2                        # Ejemplo de r > 0

# Resolver para r < 0
df_neg <- data.frame()
for (h in h_values) {
  roots <- solve_cubic(h, r_neg)
  if (length(roots) > 0) {
    df_neg <- rbind(df_neg, data.frame(r = r_neg, h = h, x = roots))
  }
}

# Resolver para r > 0
df_pos <- data.frame()
for (h in h_values) {
  roots <- solve_cubic(h, r_pos)
  if (length(roots) > 0) {
    df_pos <- rbind(df_pos, data.frame(r = r_pos, h = h, x = roots))
  }
}

# Combinar datos y clasificar por signo de r
combined_df <- rbind(df_neg, df_pos)
combined_df$r_category <- ifelse(combined_df$r > 0, "r > 0", "r < 0")

# Gráfica
ggplot(combined_df, aes(x = h, y = x, color = r_category)) +
  geom_point(size = 1, alpha = 0.7) +
  labs(
    title = "Soluciones de x³ - r x + h = 0",
    x = "h",
    y = "x",
    color = "Categoría de r"
  ) +
  scale_color_manual(values = c("r > 0" = "red", "r < 0" = "blue")) +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")
