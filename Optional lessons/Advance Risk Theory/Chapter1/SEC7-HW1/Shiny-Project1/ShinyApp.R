library(rsconnect)
# rsconnect::setAccountInfo(name='mehrab-atighi', token='E7B5467C5B0BDD0BDCFB0D290BD7A721', secret='XWzMT3X3/2m1R6I7LTYoXep8ezC1P/fKRgF2wsiG')
#rsconnect::deployApp("F:/PhD-Lessons/Advance Risk Theory/Chapter1/SEC6-HW4/Shiny-Project1/")

# Load necessary libraries
library(shiny)
library(DT)
library(ggplot2)
library(gridExtra)

# Function to simulate the risk process
simulate_risk_process <- function(u, c, t_max, lambda, claim_size_dist) {
  # Time points
  t <- seq(0, t_max)
  
  # Simulate number of claims (Poisson process)
  num_claims <- rpois(1, lambda = lambda) + 1
  
  # Simulate claim times (Poisson process)
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
      numericInput("lambda", "Rate of the Poisson process (lambda):", value = 20, min = 0),
      numericInput("mu", "Meanlog of the Lognormal distribution (mu):", value = 2),
      numericInput("sigma", "Sdlog of the Lognormal distribution (sigma):", value = 0.5, min = 0),
      numericInput("alpha", "Shape parameter of the Pareto distribution (alpha):", value = 2, min = 0),
      numericInput("xm", "Scale parameter of the Pareto distribution (xm):", value = 0.5, min = 0),
      actionButton("simulate", "Simulate")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Lognormal vs Pareto",
                 plotOutput("combinedPlot")
        ),
        tabPanel("Lognormal Distribution",
                 plotOutput("lognormalPlot"),
                 DTOutput("lognormalTable")
        ),
        tabPanel("Pareto Distribution",
                 plotOutput("paretoPlot"),
                 DTOutput("paretoTable")
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
    simulations_pareto <- list()
    
    # Perform simulations 5 times
    for (i in 1:5) {
      # Lognormal risk process simulation
      claim_size_dist_lognormal <- function(n) { rlnorm(n, meanlog = input$mu, sdlog = input$sigma) }
      simulations_lognormal[[i]] <- simulate_risk_process(input$u, input$c, input$t_max, input$lambda, claim_size_dist_lognormal)
      
      # Pareto risk process simulation
      claim_size_dist_pareto <- function(n) { (input$xm / (runif(n)^(1 / input$alpha))) - input$xm }
      simulations_pareto[[i]] <- simulate_risk_process(input$u, input$c, input$t_max, input$lambda, claim_size_dist_pareto)
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
    output$paretoPlot <- renderPlot({
      ggplot() +
        geom_line(data = simulations_pareto[[1]], aes(x = time, y = RU), color = "red") +
        geom_line(data = simulations_pareto[[2]], aes(x = time, y = RU), color = "red", linetype = "dashed") +
        geom_line(data = simulations_pareto[[3]], aes(x = time, y = RU), color = "red", linetype = "dotted") +
        geom_line(data = simulations_pareto[[4]], aes(x = time, y = RU), color = "red", linetype = "twodash") +
        geom_line(data = simulations_pareto[[5]], aes(x = time, y = RU), color = "red", linetype = "longdash") +
        labs(title = "Risk Process with Pareto Distribution",
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
      
      plot_pareto <- ggplot() +
        geom_line(data = simulations_pareto[[1]], aes(x = time, y = RU), color = "red") +
        geom_line(data = simulations_pareto[[2]], aes(x = time, y = RU), color = "red", linetype = "dashed") +
        geom_line(data = simulations_pareto[[3]], aes(x = time, y = RU), color = "red", linetype = "dotted") +
        geom_line(data = simulations_pareto[[4]], aes(x = time, y = RU), color = "red", linetype = "twodash") +
        geom_line(data = simulations_pareto[[5]], aes(x = time, y = RU), color = "red", linetype = "longdash") +
        labs(title = "Pareto Distribution",
             x = "Time (t)",
             y = "Risk Process (RU(t))") +
        theme_minimal()
      grid.arrange(plot_lognormal, plot_pareto, ncol = 2)
    })
    # Lognormal table
    output$lognormalTable <- renderDT({
      data.frame(time = simulations_lognormal[[1]]$time , RU = simulations_lognormal[[1]]$RU)
    })
    
    # Pareto table
    output$paretoTable <- renderDT({
      data.frame(time = simulations_pareto[[1]]$time , RU = simulations_pareto[[1]]$RU)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

