library(rsconnect)
# Load necessary libraries
library(shiny)
library(DT)
library(ggplot2)
library(gridExtra)

# Function to simulate the risk process
simulate_risk_process <- function(u, c, t_max, claim_size_dist, size, prob) {
  # Time points
  t <- seq(0, t_max)
  
  # Simulate number of claims (Negative Binomial process)
  num_claims <- rnbinom(1, size = size, prob = prob)
  
  # Simulate claim times (Uniform distribution)
  claim_times <- sort(runif(num_claims, 0, t_max))
  
  # Simulate claim sizes
  claim_sizes <- claim_size_dist(num_claims)
  
  # Calculate aggregate claims
  S_t <- rep(0, length(t))
  for (i in 1:num_claims) {
    S_t[t >= claim_times[i]] <- S_t[t >= claim_times[i]] + claim_sizes[i]
  }
  
  # Calculate the risk process
  RU <- u + c * t - S_t
  
  return(data.frame(time = t, RU = RU))
}

# Define UI for the application
ui <- fluidPage(
  titlePanel("Risk Process - Cramér-Lundberg Formula"),
  sidebarLayout(
    sidebarPanel(
      numericInput("u", "Initial reserve (u):", value = 600, min = 0),
      numericInput("c", "Premium rate (c):", value = 0.01, min = 0),
      numericInput("t_max", "Maximum time to simulate (t_max):", value = 2000, min = 1),
      numericInput("size", "target for number of successful trials, or dispersion parameter(size):", value = 20, min = 0),
      numericInput("prob", "probability of success in each trial(prob):", value = 0.1, min = 0 , max = 1),
      numericInput("mu", "Meanlog of the Lognormal distribution (mu):", value = 2),
      numericInput("sigma", "Sdlog of the Lognormal distribution (sigma):", value = 0.5, min = 0),
      numericInput("lambda", "Scale parameter of the Pareto distribution (xm):", value = 10, min = 1),
      actionButton("simulate", "Simulate")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Lognormal vs Exponential",
                 plotOutput("combinedPlot")
        ),
        tabPanel("Lognormal Distribution",
                 plotOutput("lognormalPlot"),
                 DTOutput("lognormalTable")
        ),
        tabPanel("Exponential Distribution",
                 plotOutput("ExpPlot"),
                 DTOutput("ExpTable")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$simulate, {
    # List to store multiple simulations
    simulations_lognormal <- list()
    simulations_exp <- list()
    
    # Perform simulations 5 times
    for (i in 1:5) {
      # Lognormal risk process simulation
      claim_size_dist_lognormal <- function(n) { rlnorm(n, meanlog = input$mu, sdlog = input$sigma) }
      simulations_lognormal[[i]] <- simulate_risk_process(input$u, input$c, input$t_max, claim_size_dist_lognormal , input$size , input$prob)
      
      # Pareto risk process simulation
      claim_size_dist_exp <- function(n) { rexp(n , rate = input$lambda) }
      simulations_exp[[i]] <- simulate_risk_process(input$u, input$c, input$t_max, claim_size_dist_exp , input$size , input$prob)
    }
    
    # Lognormal plot
    output$lognormalPlot <- renderPlot({
      ggplot() +
        geom_line(data = simulations_lognormal[[1]], aes(x = time, y = RU), color = "blue") +
        geom_line(data = simulations_lognormal[[2]], aes(x = time, y = RU), color = "blue", linetype = "dashed") +
        geom_line(data = simulations_lognormal[[3]], aes(x = time, y = RU), color = "blue", linetype = "dotted") +
        geom_line(data = simulations_lognormal[[4]], aes(x = time, y = RU), color = "blue", linetype = "twodash") +
        geom_line(data = simulations_lognormal[[5]], aes(x = time, y = RU), color = "blue", linetype = "longdash") +
        labs(title = "Risk Process with Lognormal Distribution",
             x = "Time (t)",
             y = "Risk Process (RU(t))") +
        theme_minimal()
    })
    
    # Pareto plot
    output$ExpPlot <- renderPlot({
      ggplot() +
        geom_line(data = simulations_exp[[1]], aes(x = time, y = RU), color = "red") +
        geom_line(data = simulations_exp[[2]], aes(x = time, y = RU), color = "red", linetype = "dashed") +
        geom_line(data = simulations_exp[[3]], aes(x = time, y = RU), color = "red", linetype = "dotted") +
        geom_line(data = simulations_exp[[4]], aes(x = time, y = RU), color = "red", linetype = "twodash") +
        geom_line(data = simulations_exp[[5]], aes(x = time, y = RU), color = "red", linetype = "longdash") +
        labs(title = "Risk Process with Exponential Distribution",
             x = "Time (t)",
             y = "Risk Process (RU(t))") +
        theme_minimal()
    })
    
    # Combined plot
    output$combinedPlot <- renderPlot({
      plot_lognormal <- ggplot() +
        geom_line(data = simulations_lognormal[[1]], aes(x = time, y = RU), color = "blue") +
        geom_line(data = simulations_lognormal[[2]], aes(x = time, y = RU), color = "blue", linetype = "dashed") +
        geom_line(data = simulations_lognormal[[3]], aes(x = time, y = RU), color = "blue", linetype = "dotted") +
        geom_line(data = simulations_lognormal[[4]], aes(x = time, y = RU), color = "blue", linetype = "twodash") +
        geom_line(data = simulations_lognormal[[5]], aes(x = time, y = RU), color = "blue", linetype = "longdash") +
        labs(title = "Lognormal Distribution",
             x = "Time (t)",
             y = "Risk Process (RU(t))") +
        theme_minimal()
      
      plot_exp <- ggplot() +
        geom_line(data = simulations_exp[[1]], aes(x = time, y = RU), color = "red") +
        geom_line(data = simulations_exp[[2]], aes(x = time, y = RU), color = "red", linetype = "dashed") +
        geom_line(data = simulations_exp[[3]], aes(x = time, y = RU), color = "red", linetype = "dotted") +
        geom_line(data = simulations_exp[[4]], aes(x = time, y = RU), color = "red", linetype = "twodash") +
        geom_line(data = simulations_exp[[5]], aes(x = time, y = RU), color = "red", linetype = "longdash") +
        labs(title = "Exponential Distribution",
             x = "Time (t)",
             y = "Risk Process (RU(t))") +
        theme_minimal()
      grid.arrange(plot_lognormal, plot_exp, ncol = 2)
    })
    # Lognormal table
    output$lognormalTable <- renderDT({
      data.frame(time = simulations_lognormal[[1]]$time, RU = simulations_lognormal[[1]]$RU)
    })
    
    # Exponential table
    output$ExpTable <- renderDT({
      data.frame(time = simulations_exp[[1]]$time, RU = simulations_exp[[1]]$RU)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

