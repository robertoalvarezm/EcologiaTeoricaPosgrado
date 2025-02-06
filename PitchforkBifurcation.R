if (!require(deSolve)) install.packages("deSolve")
library(shiny)
library(bslib)
library(deSolve)
library(ggplot2)
library(tidyr)

# Define the ODE function
cubic_ode <- function(t, state, params) {
  x <- state[1]  # Get x value from state
  r <- params[1] # Get r value from params
  dx <- r * x - x^3
  return(list(c(dx)))  # Return as a list containing a vector
}

# Define the potential function
potential <- function(x, r) {
  x^4/4 - r * x^2/2
}

ui <- page_sidebar(
  title = "Cubic ODE Solutions: ẋ = rx - x³",
  sidebar = sidebar(
    numericInput("r_neg", "r < 0:", value = -1, step = 0.1),
    numericInput("r_pos", "r > 0:", value = 1, step = 0.1),
    numericInput("tmax", "Time range:", value = 10, min = 1, max = 50),
    numericInput("num_ic", "Number of initial conditions:", 
                 value = 5, min = 3, max = 10)
  ),
  layout_column_wrap(
    width = 1,
    card(
      card_header("Solution curves for different values of r"),
      plotOutput("phase_plot")
    ),
    card(
      card_header("Potential V(x) = x⁴/4 - rx²/2"),
      plotOutput("potential_plot")
    )
  )
)

server <- function(input, output) {
  
  # Generate solution data
  get_solutions <- function(r_value, times, initial_conditions) {
    solutions <- lapply(initial_conditions, function(x0) {
      # Solve ODE with explicit numeric vectors
      out <- ode(
        y = c(x0),           # Initial state as numeric vector
        times = times,       # Time points
        func = cubic_ode,    # ODE function
        parms = c(r_value)   # Parameter as numeric vector
      )
      data.frame(
        t = out[, 1],
        x = out[, 2],
        r = as.factor(r_value),
        ic = x0
      )
    })
    do.call(rbind, solutions)
  }
  
  output$phase_plot <- renderPlot({
    # Time points
    times <- seq(0, input$tmax, length.out = 200)
    
    # Generate initial conditions
    ics <- seq(-2, 2, length.out = input$num_ic)
    
    # Get solutions for r < 0, r = 0, and r > 0
    sol_neg <- get_solutions(input$r_neg, times, ics)
    sol_zero <- get_solutions(0, times, ics)
    sol_pos <- get_solutions(input$r_pos, times, ics)
    
    # Combine all solutions
    all_solutions <- rbind(sol_neg, sol_zero, sol_pos)
    
    # Create plot
    ggplot(all_solutions, aes(x = t, y = x, color = r, group = interaction(r, ic))) +
      geom_line(linewidth = 1, alpha = 0.7) +
      scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A"),
                         labels = c(paste("r =", input$r_neg), 
                                    "r = 0", 
                                    paste("r =", input$r_pos))) +
      labs(x = "Time", y = "x(t)", color = "Parameter r") +
      theme_bw() +
      theme(
        legend.position = "top",
        text = element_text(size = 14),
        panel.grid.minor = element_blank()
      )
  })
  
  output$potential_plot <- renderPlot({
    # Generate x values for potential
    x_vals <- seq(-2.5, 2.5, length.out = 300)
    
    # Calculate potential for different r values
    df_potential <- expand.grid(x = x_vals, r = c(input$r_neg, 0, input$r_pos))
    df_potential$V <- with(df_potential, potential(x, r))
    df_potential$r <- as.factor(df_potential$r)
    
    # Create potential plot
    ggplot(df_potential, aes(x = x, y = V, color = r)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A"),
                         labels = c(paste("r =", input$r_neg), 
                                    "r = 0", 
                                    paste("r =", input$r_pos))) +
      labs(x = "x", y = "V(x)", color = "Parameter r") +
      theme_bw() +
      theme(
        legend.position = "top",
        text = element_text(size = 14),
        panel.grid.minor = element_blank()
      )
  })
}

shinyApp(ui, server)

