library(shiny)
library(ggplot2)
library(bslib)

# Función que define dx/dt para la bifurcación transcrítica
dx_dt <- function(x, r) {
  r * x - x^2
}

# Función que define el potencial V(x, r)
potential_V <- function(x, r) {
  (x^3) / 3 - r * (x^2) / 2
}

# Generar datos para el diagrama de bifurcación
bifurcation_data <- data.frame()
r_vals <- seq(-2, 2, length.out = 100)
for (r in r_vals) {
  x_stable <- ifelse(r > 0, r, 0)
  x_unstable <- ifelse(r > 0, 0, r)
  bifurcation_data <- rbind(bifurcation_data,
                            data.frame(r = r, x = x_stable, stability = "Estable"),
                            data.frame(r = r, x = x_unstable, stability = "Inestable"))
}

# Definir la interfaz de usuario
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  titlePanel("Bifurcación Transcrítica"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("r", "Parámetro r:", min = -2, max = 2, value = 0, step = 0.01)
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("phasePlot")),
        column(6, plotOutput("potentialPlot"))
      ),
      fluidRow(
        column(6, plotOutput("bifurcationDiagram")),
        column(6, plotOutput("vectorField"))
      )
    )
  )
)

# Definir el servidor
server <- function(input, output) {
  output$phasePlot <- renderPlot({
    x_vals <- seq(-2, 2, length.out = 200)
    dx_vals <- sapply(x_vals, dx_dt, r = input$r)
    
    # Encontrar puntos de equilibrio
    equilibria <- if (input$r != 0) c(0, input$r) else c(0)
    
    ggplot(data.frame(x = x_vals, dx = dx_vals), aes(x, dx)) +
      geom_line(color = "blue", size = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_point(data = data.frame(x = equilibria, dx = 0), aes(x, dx), color = "red", size = 3) +
      labs(title = "Gráfico de Fase", x = "x", y = "dx/dt") +
      theme_minimal()
  })
  
  output$bifurcationDiagram <- renderPlot({
    ggplot(bifurcation_data, aes(x = r, y = x, linetype = stability, color = stability)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c("Estable" = "blue", "Inestable" = "red")) +
      scale_linetype_manual(values = c("Estable" = "solid", "Inestable" = "dashed")) +
      geom_vline(xintercept = input$r, linetype = "dashed", color = "black", size = 1.2) +
      labs(title = "Diagrama de Bifurcación", x = "r", y = "Puntos de Equilibrio") +
      theme_minimal()
  })
  
  output$vectorField <- renderPlot({
    t_vals <- seq(0, 5, length.out = 20)
    x_vals <- seq(-2, 2, length.out = 20)
    grid <- expand.grid(t = t_vals, x = x_vals)
    grid$dx <- dx_dt(grid$x, input$r)
    
    ggplot(grid, aes(x = t, y = x, color = dx)) +
      geom_segment(aes(xend = t + 0.1, yend = x + dx * 0.1), arrow = arrow(length = unit(0.2, "cm"))) +
      scale_color_gradient2(low = "blue", mid = "white", high = "red") +
      labs(title = "Campo Vectorial (x vs t)", x = "t", y = "x") +
      theme_minimal()
  })
  
  output$potentialPlot <- renderPlot({
    x_vals <- seq(-2, 2, length.out = 200)
    V_vals <- sapply(x_vals, potential_V, r = input$r)
    
    ggplot(data.frame(x = x_vals, V = V_vals), aes(x, V)) +
      geom_line(color = "purple", size = 1.2) +
      labs(title = "Función Potencial V(x, r)", x = "x", y = "V(x, r)") +
      theme_minimal()
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
