# Install packages if you haven't already:
# install.packages(c("shiny", "ggplot2"))

library(shiny)
library(ggplot2)

# --- 1. USER INTERFACE (UI) ---

ui <- fluidPage(
  
  titlePanel("Homoscedasticity vs. Heteroscedasticity"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText(
        "Select the true variance structure of the data to see what happens 
        to the residuals when we fit an Ordinary Least Squares (OLS) model."
      ),
      
      selectInput(
        inputId = "variance_type",
        label = "Variance Structure:",
        choices = c(
          "Homoscedastic (Constant Spread)",
          "Heteroscedastic (Fan Shape)"
        )
      ),
      
      hr(),
      
      helpText(HTML(
        "<b>Note:</b> In both cases, the true average relationship 
        between X and Y is linear. The problem in the heteroscedastic case is 
        not the regression line itself, but the fact that the variability of the 
        errors changes across values of X."
      )),
      
      helpText(HTML(
        "The gray 95% prediction interval is based on a single residual variance 
        estimate from the OLS model. It does not explicitly model the increasing 
        spread of the data in the heteroscedastic case."
      ))
    ),
    
    mainPanel(
      plotOutput("scatterPlot", height = "350px"),
      plotOutput("residualPlot", height = "350px")
    )
  )
)


# --- 2. SERVER LOGIC ---

server <- function(input, output) {
  
  # Generate the dataset dynamically
  sim_data <- reactive({
    
    set.seed(42) # Keeps the random errors stable so the comparison is clearer
    
    N <- 300
    X <- seq(10, 100, length.out = N)
    
    # Generate errors with either constant or changing standard deviation
    if (input$variance_type == "Homoscedastic (Constant Spread)") {
      
      # Constant error standard deviation
      error_sd <- rep(20, N)
      
    } else {
      
      # Error standard deviation increases with X
      error_sd <- 0.6 * X
    }
    
    error <- rnorm(N, mean = 0, sd = error_sd)
    
    # True linear relationship
    TrueMean <- 50 + 3 * X
    Y <- TrueMean + error
    
    # Fit the OLS model
    mod <- lm(Y ~ X)
    
    # Standard OLS prediction intervals
    preds <- predict(mod, interval = "prediction")
    
    data.frame(
      X = X,
      Y = Y,
      TrueMean = TrueMean,
      Fitted = fitted(mod),
      Residuals = residuals(mod),
      Lwr = preds[, "lwr"],
      Upr = preds[, "upr"]
    )
  })
  
  
  # Plot 1: Scatterplot with true line, OLS regression line, and prediction interval
  output$scatterPlot <- renderPlot({
    
    df <- sim_data()
    
    ggplot(df, aes(x = X, y = Y)) +
      geom_ribbon(
        aes(ymin = Lwr, ymax = Upr),
        alpha = 0.2,
        fill = "gray50"
      ) +
      geom_point(alpha = 0.6, color = "steelblue", size = 2) +
      geom_line(
        aes(y = TrueMean),
        color = "darkgreen",
        linewidth = 1.2
      ) +
      geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        color = "red",
        linewidth = 1.2
      ) +
      theme_minimal() +
      labs(
        title = "OLS Regression with 95% Prediction Interval",
        subtitle = "The gray band is based on a single residual variance estimate from the OLS model.",
        x = "X",
        y = "Observed Y",
        caption = "Green line = true mean relationship; red line = fitted OLS regression line"
      ) +
      theme(
        text = element_text(size = 14),
        plot.caption = element_text(size = 11)
      )
  })
  
  
  # Plot 2: Residuals vs fitted values
  output$residualPlot <- renderPlot({
    
    df <- sim_data()
    
    ggplot(df, aes(x = Fitted, y = Residuals)) +
      geom_point(alpha = 0.6, color = "steelblue", size = 2) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "black",
        linewidth = 1
      ) +
      theme_minimal() +
      labs(
        title = "Residuals vs. Fitted Values",
        subtitle = "In the heteroscedastic case, the residuals show a fan-shaped pattern.",
        x = "Fitted Values",
        y = "Residuals"
      ) +
      theme(text = element_text(size = 14))
  })
}


# --- 3. RUN THE APP ---

shinyApp(ui = ui, server = server)