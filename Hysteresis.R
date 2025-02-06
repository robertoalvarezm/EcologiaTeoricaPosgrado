if (!require(deSolve)) install.packages("deSolve")
library(shiny)
library(bslib)
library(deSolve)
library(ggplot2)
library(tidyr)

# Define the ODE function with h parameter
cubic_ode <- function(t, state, params) {
  x <- state[1]  
  r <- params[1] 
  h <- params[2] # Add h parameter
  dx <- h + r * x - x^3
  return(list(c(dx)))
}

# Define the potential function with h
potential <- function(x, r, h) {
  -h*x + x^4/4 - r * x^2/2
}

ui <- page_sidebar(
  title = "Cubic ODE Solutions: ẋ = h + rx - x³",
  sidebar = sidebar(
    numericInput("h", "h value:", value = 0.2, step = 0.1),
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
      card_header("Potential V(x) = -hx + x⁴/4 - rx²/2"),
      plotOutput("potential_plot")
    )
  )
)

server <- function(input, output) {
  
  # Generate solution data
  get_solutions <- function(r_value, h_value, times, initial_conditions) {
    solutions <- lapply(initial_conditions, function(x0) {
      out <- ode(
        y = c(x0),
        times = times,
        func = cubic_ode,
        parms = c(r_value, h_value)  # Pass both r and h
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
    times <- seq(0, input$tmax, length.out = 200)
    ics <- seq(-2, 2, length.out = input$num_ic)
    
    # Get solutions for r < 0 and r > 0 (removing r = 0 case)
    sol_neg <- get_solutions(input$r_neg, input$h, times, ics)
    sol_pos <- get_solutions(input$r_pos, input$h, times, ics)
    
    # Combine solutions
    all_solutions <- rbind(sol_neg, sol_pos)
    
    ggplot(all_solutions, aes(x = t, y = x, color = r, group = interaction(r, ic))) +
      geom_line(linewidth = 1, alpha = 0.7) +
      scale_color_manual(values = c("#E41A1C", "#4DAF4A"),
                         labels = c(paste("r =", input$r_neg),
                                    paste("r =", input$r_pos))) +
      labs(x = "Time", y = "x(t)", 
           color = "Parameter r",
           title = paste("h =", input$h)) +
      theme_bw() +
      theme(
        legend.position = "top",
        text = element_text(size = 14),
        panel.grid.minor = element_blank()
      )
  })
  
  output$potential_plot <- renderPlot({
    x_vals <- seq(-2.5, 2.5, length.out = 300)
    
    # Calculate potential for different r values
    df_potential <- expand.grid(x = x_vals, r = c(input$r_neg, input$r_pos))
    df_potential$V <- with(df_potential, potential(x, r, input$h))
    df_potential$r <- as.factor(df_potential$r)
    
    ggplot(df_potential, aes(x = x, y = V, color = r)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = c("#E41A1C", "#4DAF4A"),
                         labels = c(paste("r =", input$r_neg),
                                    paste("r =", input$r_pos))) +
      labs(x = "x", y = "V(x)", 
           color = "Parameter r",
           title = paste("h =", input$h)) +
      theme_bw() +
      theme(
        legend.position = "top",
        text = element_text(size = 14),
        panel.grid.minor = element_blank()
      )
  })
}

shinyApp(ui, server)

