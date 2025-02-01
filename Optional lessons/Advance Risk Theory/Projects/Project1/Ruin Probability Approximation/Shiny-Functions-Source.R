
# Define functions for ruin probability approximations

# 1. Cramér–Lundberg approximation function
cramer_lundberg <- function(u, theta, mu, M_prime_X_R, R) {
  C <- (theta * mu) / (M_prime_X_R - mu * (1 + theta))
  psi_CL <- C * exp(-R * u)
  return(psi_CL)
}

# 2. Exponential approximation function
exponential_approx <- function(u, mu, theta, mu_2, mu_3) {
  exponent <- -1 - (2 * mu * theta * u - mu_2) / sqrt((mu_2)^2 + (4/3) * theta * mu * mu_3)
  psi_E <- exp(exponent)
  return(psi_E)
}

# 3. Lundberg approximation function
lundberg_approx <- function(u, mu, theta, mu_2, mu_3) {
  coefficient <- 1 + (theta * u - (mu_2) / (2 * mu)) * (4 * theta * mu^2 * mu_3) / (3 * (mu_2)^3)
  psi_L <- coefficient * exp(-2 * mu * theta * u / mu_2)
  return(psi_L)
}

# 4. Beekman–Bowers approximation function
beekman_bowers_approx <- function(u, mu, theta, mu_2, mu_3) {
  alpha <- 1 + ((4 * mu * mu_3 / (3 * (mu_2)^2) - 1) * theta) / (1 + theta)
  beta <- (2 * mu * theta) / (mu_2 + (4 * mu * mu_3 / (3 * mu_2 - mu_2)) * theta)
  G_u <- pgamma(u, shape = alpha, rate = 1 / beta)
  psi_BB <- (1 / (1 + theta)) * (1 - G_u)
  return(psi_BB)
}

# 5. Rényi approximation function
renyi_approx <- function(u, mu, theta, mu_2) {
  psi_R <- (1 / (1 + theta)) * exp(-2 * mu * theta * u / (mu_2 * (1 + theta)))
  return(psi_R)
}

# 6. De Vylder approximation function
de_vylder_approx <- Vectorize(function(u, theta, mu, mu_2, mu_3) {
  beta_bar <- 3 * mu_2 / mu_3
  lambda_bar <- (9 * mu * (mu_2)^3) / (2 * (mu_3)^2)
  theta_bar <- (2 * mu * mu_3) / (3 * (mu_2)^2) * theta
  result <- (1 / (1 + theta_bar)) * exp(-theta_bar * beta_bar * u / (1 + theta_bar))
  return(result)
})

# 8. Heavy Traffic approximation function
heavy_traffic_approx <- function(u, theta, mu_2) {
  psi_HT <- exp(-2 * theta * u / mu_2)
  return(psi_HT)
}

# 9. Light Traffic Approximation function
light_traffic_approximation <- function(u, theta, mu, survival_function) {
  integrand <- function(x) {
    survival_function(x)
  }
  integral_value <- tryCatch({
    integral(integrand, u, Inf)
  }, error = function(e) {
    warning("Integral failed to converge.")
    return(NA)
  })
  psi_LT <- (1 / ((1 + theta) * mu)) * integral_value
  return(psi_LT)
}

# Define a simple survival function (exponential)
survival_function_example <- function(x) {
  exp(-x / 1)  # Modify this for different distributions
}

# 10. HLT approximation
HLT_Function <- function(u, theta, mu, survival_function_example, mu_2) {
  psi_LT <- light_traffic_approximation(u, theta, mu, survival_function_example)
  psi_HT <- heavy_traffic_approx(u, theta, mu_2)
  result <- theta / (1 + theta) * psi_LT * (theta * mu / (1 + theta)) + (1 / (1 + theta)^2) * psi_HT
  return(result)
}

# Define a function to simulate real values
simulate_real_values <- function(u_vals, theta, mu) {
  # Example simulation for real values (using an exponential function as a placeholder)
  real_values <- exp(-theta * u_vals / mu)
  return(real_values)
}

# Install and load required packages
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(shinythemes)) install.packages("shinythemes")
if (!require(dplyr)) install.packages("dplyr")

library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)

# Load the approximation functions from test.R

# Define a function to simulate real values
simulate_real_values <- function(u_vals, theta, mu) {
  # Example simulation for real values (using an exponential function as a placeholder)
  real_values <- exp(-theta * u_vals / mu)
  return(real_values)
}
# Install and load required packages
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(shinythemes)) install.packages("shinythemes")
if (!require(dplyr)) install.packages("dplyr")
if (!require(DT)) install.packages("DT")
if (!require(plotly)) install.packages("plotly")

library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(DT)
library(plotly)  # For interactive plots

# Define the UI
ui <- fluidPage(
  theme = shinytheme("united"), # Using a vibrant theme
  
  titlePanel("Enhanced Ruin Probability Approximations"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Input Parameters"),
      sliderInput("u", "Initial Reserve (u):", min = 0, max = 200, value = 100, step = 1),
      sliderInput("theta", "Safety Loading (θ):", min = 0.1, max = 1, value = 0.3, step = 0.05),
      sliderInput("mu", "Mean Claim Amount (μ):", min = 0.1, max = 2, value = 1, step = 0.1),
      sliderInput("mu_2", "Second Moment of Claim Size (μ2):", min = 0.1, max = 5, value = 2, step = 0.1),
      sliderInput("mu_3", "Third Moment of Claim Size (μ3):", min = 0.1, max = 10, value = 3, step = 0.1),
      sliderInput("M_prime_X_R", "Derivative of Moment Generating Function at R (M'(X,R)):", 
                  min = 0.1, max = 5, value = 1.5, step = 0.1),
      sliderInput("R", "Adjustment Coefficient (R):", min = 0.01, max = 1, value = 0.05, step = 0.01),
      
      checkboxGroupInput("approximations", "Select Approximations to Display:", 
                         choices = c("Cramer-Lundberg" = "CramerLundberg", 
                                     "Exponential" = "Exponential", 
                                     "Lundberg" = "Lundberg", 
                                     "Beekman-Bowers" = "BeekmanBowers", 
                                     "Rényi" = "Renyi", 
                                     "De Vylder" = "DeVylder", 
                                     "Heavy Traffic" = "HeavyTraffic"),
                         selected = c("CramerLundberg", "Exponential", "Lundberg")),
      
      actionButton("update_plot", "Update Plot", class = "btn-primary") # Styled button
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Comparison Plot", 
                 plotlyOutput("comparisonPlot"),  # Main interactive plot
                 tableOutput("avgErrorTable")),  # Display average error in the same tab
        
        # Separate tabs for each approximation with plots and interactive tables
        tabPanel("Cramer-Lundberg", plotOutput("plot_CramerLundberg"), DTOutput("table_CramerLundberg")),
        tabPanel("Exponential", plotOutput("plot_Exponential"), DTOutput("table_Exponential")),
        tabPanel("Lundberg", plotOutput("plot_Lundberg"), DTOutput("table_Lundberg")),
        tabPanel("Beekman-Bowers", plotOutput("plot_BeekmanBowers"), DTOutput("table_BeekmanBowers")),
        tabPanel("Rényi", plotOutput("plot_Renyi"), DTOutput("table_Renyi")),
        tabPanel("De Vylder", plotOutput("plot_DeVylder"), DTOutput("table_DeVylder")),
        tabPanel("Heavy Traffic", plotOutput("plot_HeavyTraffic"), DTOutput("table_HeavyTraffic"))
      )
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Reactive expression to update the plot only when the action button is clicked
  plot_data <- eventReactive(input$update_plot, {
    u_vals <- seq(0, input$u, by = 1)
    
    # Calculate selected approximations
    approx_data <- list()
    
    if ("CramerLundberg" %in% input$approximations) {
      approx_data$CramerLundberg <- sapply(u_vals, function(u) 
        cramer_lundberg(u, input$theta, input$mu, input$M_prime_X_R, input$R))
    }
    if ("Exponential" %in% input$approximations) {
      approx_data$Exponential <- sapply(u_vals, function(u) 
        exponential_approx(u, input$mu, input$theta, input$mu_2, input$mu_3))
    }
    if ("Lundberg" %in% input$approximations) {
      approx_data$Lundberg <- sapply(u_vals, function(u) 
        lundberg_approx(u, input$mu, input$theta, input$mu_2, input$mu_3))
    }
    if ("BeekmanBowers" %in% input$approximations) {
      approx_data$BeekmanBowers <- sapply(u_vals, function(u) 
        beekman_bowers_approx(u, input$mu, input$theta, input$mu_2, input$mu_3))
    }
    if ("Renyi" %in% input$approximations) {
      approx_data$Renyi <- sapply(u_vals, function(u) 
        renyi_approx(u, input$mu, input$theta, input$mu_2))
    }
    if ("DeVylder" %in% input$approximations) {
      approx_data$DeVylder <- sapply(u_vals, function(u) 
        de_vylder_approx(u, input$theta, input$mu, input$mu_2, input$mu_3))
    }
    if ("HeavyTraffic" %in% input$approximations) {
      approx_data$HeavyTraffic <- sapply(u_vals, function(u) 
        heavy_traffic_approx(u, input$theta, input$mu_2))
    }
    
    # Simulate real values
    approx_data$RealValues <- simulate_real_values(u_vals, input$theta, input$mu)
    
    # Create a data frame for plotting and tables
    data <- data.frame(u = u_vals)
    for (name in names(approx_data)) {
      data[[name]] <- approx_data[[name]]
    }
    
    data
  })
  
  # Render the main comparison plot with plotly (interactive)
  output$comparisonPlot <- renderPlotly({
    data <- plot_data()
    
    plot <- ggplot(data, aes(x = u)) +
      geom_line(aes(y = RealValues), color = "black", linetype = "dashed") +  # Real values line
      
      # Add lines for selected approximations
      geom_line(aes(y = CramerLundberg, color = "Cramer-Lundberg")) +
      geom_line(aes(y = Exponential, color = "Exponential")) +
      geom_line(aes(y = Lundberg, color = "Lundberg")) +
      geom_line(aes(y = BeekmanBowers, color = "Beekman-Bowers")) +
      geom_line(aes(y = Renyi, color = "Rényi")) +
      geom_line(aes(y = DeVylder, color = "De Vylder")) +
      geom_line(aes(y = HeavyTraffic, color = "Heavy Traffic")) +
      
      labs(title = "Comparison of Ruin Probability Approximations",
           x = "Initial Reserve (u)", y = "Ruin Probability") +
      scale_color_manual(values = c("blue", "green", "red", "purple", "orange", "brown", "pink")) +
      theme_minimal()
    
    # Convert ggplot to plotly for interactivity
    ggplotly(plot, tooltip = "text")
  })
  
  # Calculate and render the average error table in the "Comparison Plot" panel
  output$avgErrorTable <- renderTable({
    data <- plot_data()
    
    avg_errors <- sapply(input$approximations, function(name) {
      mean(abs(data[[name]] - data$RealValues), na.rm = TRUE)
    })
    
    avg_error_df <- data.frame(
      Method = input$approximations,
      AverageError = avg_errors
    )
    
    avg_error_df
  })
  
  # Function to render individual plots and interactive tables for each method
  renderApproximationPlotAndTable <- function(name, color) {
    output[[paste0("plot_", name)]] <- renderPlot({
      data <- plot_data()
      ggplot(data, aes(x = u)) +
        geom_line(aes_string(y = name), color = color) +
        geom_line(aes(y = RealValues), color = "black", linetype = "dashed") +
        labs(title = paste0(name, " Approximation"),
             x = "Initial Reserve (u)", y = "Ruin Probability") +
        theme_minimal()
    })
    
    output[[paste0("table_", name)]] <- renderDT({
      data <- plot_data()
      datatable(data[, c("u", name, "RealValues")], options = list(pageLength = 10))
    })
  }
  
  # Generate individual approximation plots and tables
  renderApproximationPlotAndTable("CramerLundberg", "blue")
  renderApproximationPlotAndTable("Exponential", "green")
  renderApproximationPlotAndTable("Lundberg", "red")
  renderApproximationPlotAndTable("BeekmanBowers", "purple")
  renderApproximationPlotAndTable("Renyi", "orange")
  renderApproximationPlotAndTable("DeVylder", "brown")
  renderApproximationPlotAndTable("HeavyTraffic", "pink")
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
