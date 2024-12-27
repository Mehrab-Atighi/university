# Load necessary libraries
library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Term Annuity Calculator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("model", "Select Model:",
                  choices = c("Makeham" = "makeham", "Gompertz" = "gompertz")),
      numericInput("age", "Enter Age:", value = 30, min = 0),
      numericInput("n", "Enter Term (years):", value = 10, min = 1),
      numericInput("interest", "Enter Interest Rate (%):", value = 5, min = 0),
    numericInput("Lx", "Enter Lx:", value = 100000),
    numericInput("S", "Enter Sum insure value:", value = 1000, min = 1)
    ),

    mainPanel(
      textOutput("result"),
      dataTableOutput("premiumTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$result <- renderText({
    age <- input$age
    n <- input$n
    interest <- input$interest / 100
    model <- input$model
    
    # Define Makeham model
    makeham <- function(age, n, interest) {
      A <- 0.00022
      B <- 2.7e-06
      C <- 1.124
      q <- A + B * C^(age + n)
      v <- 1 / (1 + interest)
      annuity <- sum(v^(1:n) * (1 - q)^(1:n))
      return(annuity)
    }
    
    # Define Gompertz model
    gompertz <- function(age, n, interest) {
      B <- 0.00035
      C <- 1.08
      q <- B * C^(age + n)
      v <- 1 / (1 + interest)
      annuity <- sum(v^(1:n) * (1 - q)^(1:n))
      return(annuity)
    }
    
    if (model == "makeham") {
      annuity <- makeham(age, n, interest)
    } else {
      annuity <- gompertz(age, n, interest)
    }
    
    paste("The term annuity is:", round(annuity, 2))
  })
  
  output$premiumTable <- renderDataTable({
    age <- input$age
    n <- input$n
    interest <- input$interest / 100
    model <- input$model
    
    monthly_interest <- (1 + interest)^(1/12) - 1
    term_months <- n * 12
    
    # Define Makeham model
    makeham <- function(age, term_months, monthly_interest) {
      A <- 0.00022
      B <- 2.7e-06
      C <- 1.124
      q <- A + B * C^(age + term_months / 12)
      v <- 1 / (1 + monthly_interest)
      premiums <- v^(1:term_months) * (1 - q)^(1:term_months)
      return(premiums)
    }
    
    # Define Gompertz model
    gompertz <- function(age, term_months, monthly_interest) {
      B <- 0.00035
      C <- 1.08
      q <- B * C^(age + term_months / 12)
      v <- 1 / (1 + monthly_interest)
      premiums <- v^(1:term_months) * (1 - q)^(1:term_months)
      return(premiums)
    }
    
    if (model == "makeham") {
      premiums <- makeham(age, term_months, monthly_interest)
    } else {
      premiums <- gompertz(age, term_months, monthly_interest)
    }
    
    data.frame(Month = 1:term_months, Premium = round(premiums, 2))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
