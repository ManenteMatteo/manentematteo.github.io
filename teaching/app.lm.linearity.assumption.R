# Install packages if you haven't already:
# install.packages(c("shiny", "ggplot2"))

library(shiny)
library(ggplot2)

# --- 1. USER INTERFACE (UI) ---

ui <- fluidPage(
  
  titlePanel("Linear Models: Linearity Assumption"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText(
        "Change the true underlying shape of the data to see what happens 
        when we fit a straight linear model."
      ),
      
      selectInput(
        inputId = "shape",
        label = "True Data Shape:",
        choices = c("Linear", "U-Shape (Quadratic)", "Exponential")
      ),
      
      sliderInput(
        inputId = "noise",
        label = "Noise Level (Standard Deviation):",
        min = 0,
        max = 20,
        value = 5,
        step = 1
      ),
      
      helpText(
        "Top plot: the green line is the true relationship, while the blue line 
        is the fitted linear model."
      ),
      
      helpText(
        "Bottom plot: the red line shows the average trend of the residuals. 
        If the linearity assumption is reasonable, this line should stay 
        approximately flat around zero."
      )
    ),
    
    mainPanel(
      plotOutput("scatterPlot", height = "320px"),
      plotOutput("residualPlot", height = "320px")
    )
  )
)


# --- 2. SERVER LOGIC ---

server <- function(input, output) {
  
  # Generate the dataset dynamically based on user inputs
  sim_data <- reactive({
    
    set.seed(42) # Keeps the random errors stable
    
    # Generate 150 fixed values for X
    X <- seq(0, 10, length.out = 150)
    
    # Generate normally distributed random error
    error <- rnorm(
      n = 150,
      mean = 0,
      sd = input$noise
    )
    
    # Create Y based on the selected true relationship
    if (input$shape == "Linear") {
      
      TrueMean <- 5 + 3 * X
      
    } else if (input$shape == "U-Shape (Quadratic)") {
      
      TrueMean <- 20 - 10 * X + 1.2 * X^2
      
    } else {
      
      TrueMean <- 2 * exp(0.4 * X)
    }
    
    # Observed outcome
    Y <- TrueMean + error
    
    # Fit a linear model, regardless of the true data shape
    mod <- lm(Y ~ X)
    
    data.frame(
      X = X,
      Y = Y,
      TrueMean = TrueMean,
      Fitted = predict(mod),
      Residuals = residuals(mod)
    )
  })
  
  
  # Plot 1: Raw data, true relationship, and fitted linear model
  output$scatterPlot <- renderPlot({
    
    df <- sim_data()
    
    ggplot(df, aes(x = X, y = Y)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_line(
        aes(y = TrueMean),
        color = "darkgreen",
        linewidth = 1.2
      ) +
      geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        color = "blue",
        linewidth = 1.2
      ) +
      theme_minimal() +
      labs(
        title = paste("Fitting a Linear Model to", input$shape, "Data"),
        x = "X",
        y = "Observed Y",
        caption = "Green line = true relationship; blue line = fitted linear model"
      ) +
      theme(
        text = element_text(size = 14),
        plot.caption = element_text(size = 11)
      )
  })
  
  
  # Plot 2: Residual plot and LOESS curve
  output$residualPlot <- renderPlot({
    
    df <- sim_data()
    
    ggplot(df, aes(x = X, y = Residuals)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "black"
      ) +
      geom_smooth(
        method = "loess",
        formula = y ~ x,
        se = FALSE,
        color = "red",
        linewidth = 1.5
      ) +
      theme_minimal() +
      labs(
        title = "Residual Plot: Checking for Systematic Patterns",
        x = "X",
        y = "Residuals",
        caption = "Red line = estimated average trend of the residuals"
      ) +
      theme(
        text = element_text(size = 14),
        plot.caption = element_text(size = 11)
      )
  })
}


# --- 3. RUN THE APP ---

shinyApp(ui = ui, server = server)