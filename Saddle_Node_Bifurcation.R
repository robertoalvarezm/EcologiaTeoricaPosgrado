library(shiny)
library(ggplot2)
library(bslib)

# Function defining dx/dt for the saddle-node bifurcation
dx_dt <- function(x, r) {
  r - x^2
}

# Function defining the potential V(x, r)
potential_V <- function(x, r) {
  (x^3) / 3 - r * x
}

# Generate data for the bifurcation diagram
bifurcation_data <- data.frame()
r_vals <- seq(-2, 2, length.out = 100)
for (r in r_vals) {
  if (r > 0) {
    x_stable <- sqrt(r)
    x_unstable <- -sqrt(r)
    bifurcation_data <- rbind(bifurcation_data,
                              data.frame(r = r, x = x_stable, stability = "Stable"),
                              data.frame(r = r, x = x_unstable, stability = "Unstable"))
  }
}

# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  titlePanel("Saddle-Node Bifurcation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("r", "Parameter r:", min = -2, max = 2, value = 0, step = 0.01)
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("phasePlot")),
        column(6, plotOutput("potentialPlot"))
      ),
      fluidRow(
        column(6, uiOutput("bifurcationMessage"), plotOutput("bifurcationDiagram")),
        column(6, plotOutput("vectorField"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  output$phasePlot <- renderPlot({
    x_vals <- seq(-2, 2, length.out = 200)
    dx_vals <- sapply(x_vals, dx_dt, r = input$r)
    
    # Find equilibrium points
    equilibria <- if (input$r > 0) c(-sqrt(input$r), sqrt(input$r)) else numeric(0)
    
    ggplot(data.frame(x = x_vals, dx = dx_vals), aes(x, dx)) +
      geom_line(color = "blue", size = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_point(data = data.frame(x = equilibria, dx = 0), aes(x, dx), color = "red", size = 3) +
      labs(title = "Phase Plot", x = "x", y = "dx/dt") +
      theme_minimal()
  })
  
  output$bifurcationMessage <- renderUI({
    if (input$r < 0) {
      h3("No hay soluciones para r < 0, por lo que el diagrama de bifurcación es inexistente.", style = "color: red;")
    } else {
      NULL
    }
  })
  
  output$bifurcationDiagram <- renderPlot({
    if (input$r < 0) return(NULL)
    
    ggplot(bifurcation_data, aes(x = r, y = x, linetype = stability, color = stability)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c("Stable" = "blue", "Unstable" = "red")) +
      scale_linetype_manual(values = c("Stable" = "solid", "Unstable" = "dashed")) +
      geom_vline(xintercept = input$r, linetype = "dashed", color = "black", size = 1.2) +
      labs(title = "Bifurcation Diagram", x = "r", y = "Equilibrium Points") +
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
      labs(title = "Vector Field (x vs t)", x = "t", y = "x") +
      theme_minimal()
  })
  
  output$potentialPlot <- renderPlot({
    x_vals <- seq(-2, 2, length.out = 200)
    V_vals <- sapply(x_vals, potential_V, r = input$r)
    
    ggplot(data.frame(x = x_vals, V = V_vals), aes(x, V)) +
      geom_line(color = "purple", size = 1.2) +
      labs(title = "Potential Function V(x, r)", x = "x", y = "V(x, r)") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

