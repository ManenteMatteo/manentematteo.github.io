# Install packages if you haven't already:
# install.packages(c("shiny", "ggplot2"))

library(shiny)
library(ggplot2)

# --- 1. PRE-COMPUTE BASE DATA ---

set.seed(42)

base_X <- seq(1, 10, length.out = 30)
base_Y <- 5 + 2 * base_X + rnorm(30, mean = 0, sd = 2)

base_data <- data.frame(
  X = base_X,
  Y = base_Y,
  PointType = "Base"
)

base_model <- lm(Y ~ X, data = base_data)


# --- 2. USER INTERFACE (UI) ---

ui <- fluidPage(
  
  titlePanel("Influence, Leverage, and Outliers"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText(
        "Move the sliders to position the additional point. Watch how it can 
        affect the fitted regression line."
      ),
      
      sliderInput(
        inputId = "rogue_x",
        label = "Additional Point X Position (Leverage):",
        min = 0,
        max = 25,
        value = 5,
        step = 0.5
      ),
      
      sliderInput(
        inputId = "rogue_y",
        label = "Additional Point Y Position (Potential Outlier):",
        min = -15,
        max = 55,
        value = 15,
        step = 1
      ),
      
      hr(),
      
      h4("Real-Time Diagnostics"),
      
      htmlOutput("diagnostics_text"),
      
      hr(),
      
      helpText(HTML(
        "<b>Note:</b> Leverage depends on how far a point is from 
        the center of the X values. A point is influential when it combines 
        high leverage with a large residual, strongly changing the fitted model."
      ))
    ),
    
    mainPanel(
      plotOutput("influencePlot", height = "520px")
    )
  )
)


# --- 3. SERVER LOGIC ---

server <- function(input, output) {
  
  model_data <- reactive({
    
    # Create the movable additional point
    rogue_df <- data.frame(
      X = input$rogue_x,
      Y = input$rogue_y,
      PointType = "Additional point"
    )
    
    # Combine original data with the additional point
    combined_data <- rbind(base_data, rogue_df)
    
    # Fit the new model including the additional point
    new_model <- lm(Y ~ X, data = combined_data)
    
    # Add model diagnostics to the data
    combined_data$Fitted <- fitted(new_model)
    combined_data$Residuals <- residuals(new_model)
    combined_data$StdResiduals <- rstandard(new_model)
    combined_data$Leverage <- hatvalues(new_model)
    combined_data$CooksD <- cooks.distance(new_model)
    
    # Index of the additional point
    n <- nrow(combined_data)
    
    # Number of model parameters: intercept + slope
    p <- length(coef(new_model))
    
    # Common diagnostic cutoffs
    leverage_cutoff <- 2 * p / n
    cooks_cutoff <- 4 / n
    
    list(
      data = combined_data,
      model = new_model,
      leverage = combined_data$Leverage[n],
      raw_resid = combined_data$Residuals[n],
      std_resid = combined_data$StdResiduals[n],
      cooks_d = combined_data$CooksD[n],
      leverage_cutoff = leverage_cutoff,
      cooks_cutoff = cooks_cutoff
    )
  })
  
  
  output$diagnostics_text <- renderUI({
    
    res <- model_data()
    
    # Leverage message
    leverage_msg <- if (res$leverage > res$leverage_cutoff) {
      "<br><span style='color:orange;'><b>High leverage:</b> this point is far from the center of the X values.</span>"
    } else {
      "<br><span style='color:gray;'>Leverage is not especially high.</span>"
    }
    
    # Residual message
    residual_msg <- if (abs(res$std_resid) > 3) {
      "<br><span style='color:red;'><b>Very large standardized residual:</b> this point is a strong outlier in Y.</span>"
    } else if (abs(res$std_resid) > 2) {
      "<br><span style='color:orange;'><b>Large standardized residual:</b> this point may be an outlier in Y.</span>"
    } else {
      "<br><span style='color:gray;'>Standardized residual is not especially large.</span>"
    }
    
    # Cook's distance message
    cooks_msg <- if (res$cooks_d > 1) {
      "<br><span style='color:red;'><b>Very high Cook's distance:</b> this point is highly influential.</span>"
    } else if (res$cooks_d > res$cooks_cutoff) {
      "<br><span style='color:orange;'><b>Potentially influential:</b> Cook's distance is above the common 4/n guideline.</span>"
    } else {
      "<br><span style='color:gray;'>Cook's distance is relatively small.</span>"
    }
    
    HTML(paste0(
      "<b>Leverage:</b> ", round(res$leverage, 3), "<br>",

      
      
      "<b>Raw residual:</b> ", round(res$raw_resid, 3), "<br>",

      
      "<b>Standardized residual:</b> ", round(res$std_resid, 3), "<br>",

      
      
      "<b>Cook's distance:</b> ", round(res$cooks_d, 3),
      
      leverage_msg,
      residual_msg,
      cooks_msg
    ))
  })
  
  
  output$influencePlot <- renderPlot({
    
    res <- model_data()
    df <- res$data
    
    additional_point <- subset(df, PointType == "Additional point")
    
    # Mean of X in the current dataset
    mean_X <- mean(df$X)
    
    ggplot(df, aes(x = X, y = Y)) +
      
      # Mean of X: useful because leverage depends on distance from the center of X
      geom_vline(
        xintercept = mean_X,
        color = "darkgreen",
        linewidth = 0.6,
        alpha = 0.8
      ) +
      
      # Original base points
      geom_point(
        data = subset(df, PointType == "Base"),
        color = "grey50",
        size = 3,
        alpha = 0.6
      ) +
      
      # Original regression line before adding the movable point
      geom_abline(
        intercept = coef(base_model)[1],
        slope = coef(base_model)[2],
        color = "blue",
        linetype = "dashed",
        linewidth = 1.2
      ) +
      
      # New regression line after adding the movable point
      geom_abline(
        intercept = coef(res$model)[1],
        slope = coef(res$model)[2],
        color = "red",
        linewidth = 1.5
      ) +
      
      # Vertical segment showing the residual of the additional point
      geom_segment(
        data = additional_point,
        aes(
          x = X,
          xend = X,
          y = Fitted,
          yend = Y
        ),
        color = "purple",
        linewidth = 1.2
      ) +
      
      # Crosshairs for the additional point
      geom_vline(
        xintercept = input$rogue_x,
        color = "orange",
        linetype = "dotted",
        alpha = 0.5
      ) +
      geom_hline(
        yintercept = input$rogue_y,
        color = "orange",
        linetype = "dotted",
        alpha = 0.5
      ) +
      
      # Additional movable point
      geom_point(
        data = additional_point,
        color = "orange",
        size = 6
      ) +
      
      coord_cartesian(
        xlim = c(0, 26),
        ylim = c(-15, 55)
      ) +
      
      theme_minimal() +
      labs(
        title = "How a Single Data Point Can Influence a Regression Line",
        subtitle = "Blue dashed line = original model | Red line = model with additional point | Purple segment = residual",
        x = "Predictor (X)",
        y = "Outcome (Y)",
        caption = "Green vertical line = current mean of X. Leverage increases as a point moves farther away from this center."
      ) +
      theme(
        text = element_text(size = 15),
        plot.subtitle = element_text(color = "grey30", face = "italic"),
        plot.caption = element_text(size = 11, hjust = 0.5),
        plot.caption.position = "plot"
      )
  })
}


# --- 4. RUN THE APP ---

shinyApp(ui = ui, server = server)