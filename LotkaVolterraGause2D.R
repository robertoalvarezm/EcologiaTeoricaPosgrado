library(shiny)
library(bslib)
library(deSolve)
library(ggplot2)

# Define the system of differential equations
lotka_comp <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dx <- r1 * x - a * x^2 - c * x * y
    dy <- r2 * y - b * y^2 - d * x * y
    return(list(c(dx, dy)))
  })
}

ui <- page_sidebar(
  title = "Competitive Species Model",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    title = "Parameters",
    sliderInput("r1", "r₁ (growth rate of species 1)", 
                min = 0, max = 2, value = 1, step = 0.1),
    sliderInput("r2", "r₂ (growth rate of species 2)", 
                min = 0, max = 2, value = 1, step = 0.1),
    sliderInput("a", "a (intra-specific of species 1)", 
                min = 0, max = 1, value = 0.2, step = 0.05),
    sliderInput("b", "b (intra-specific of species 2)", 
                min = 0, max = 1, value = 0.2, step = 0.05),
    sliderInput("c", "c (inter 2 on species 1)", 
                min = 0, max = 1, value = 0.3, step = 0.05),
    sliderInput("d", "d (inter of species 1 on species 2)", 
                min = 0, max = 1, value = 0.3, step = 0.05)
  ),
  layout_columns(
    card(
      card_header("Species 1 (x) vs Time"),
      plotOutput("plot_x")
    ),
    card(
      card_header("Species 2 (y) vs Time"),
      plotOutput("plot_y")
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression for solving the ODE system
  solution <- reactive({
    # Time points
    times <- seq(0, 50, by = 0.1)
    
    # Initial conditions
    state <- c(x = 1, y = 1)
    
    # Parameters
    parameters <- c(
      r1 = input$r1,
      r2 = input$r2,
      a = input$a,
      b = input$b,
      c = input$c,
      d = input$d
    )
    
    # Solve the system
    ode(y = state, times = times, func = lotka_comp, parms = parameters)
  })
  
  # Plot for species 1 (x)
  output$plot_x <- renderPlot({
    sol_df <- as.data.frame(solution())
    ggplot(sol_df, aes(x = time, y = x)) +
      geom_line(color = "blue", linewidth = 1.2) +
      theme_bw() +
      labs(x = "Time", y = "Population of Species 1") +
      theme(text = element_text(size = 14))
  })
  
  # Plot for species 2 (y)
  output$plot_y <- renderPlot({
    sol_df <- as.data.frame(solution())
    ggplot(sol_df, aes(x = time, y = y)) +
      geom_line(color = "red", linewidth = 1.2) +
      theme_bw() +
      labs(x = "Time", y = "Population of Species 2") +
      theme(text = element_text(size = 14))
  })
}

shinyApp(ui, server)

