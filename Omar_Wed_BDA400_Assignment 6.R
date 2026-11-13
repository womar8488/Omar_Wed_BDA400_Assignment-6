# --- Step 1: Data Collection and Setup ---
library(shiny)
library(ggplot2)
library(quantmod)

# Define target parameters and fetch historical stock data
stock_symbol <- "AAPL" 
start_date <- "2023-01-01" 
end_date <- "2023-07-01" 

stock_data <- getSymbols(stock_symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)


# --- Step 2, 3 & 4: UI and Server Logic ---

# Define UI components with interactive widgets and technical indicator checkboxes
ui <- fluidPage(
  dateRangeInput("date_range", "Select Date Range:", start = "2023-01-01", end = "2023-07-01"),
  selectInput("time_frame", "Select Time Frame:", choices = c("Daily", "Weekly", "Monthly")),
  checkboxGroupInput("technical_indicators", "Select Technical Indicators:",
                     choices = c("Moving Averages", "RSI", "MACD")),
  plotOutput("stock_chart")
)

# Define Server logic with reactive data filtering, indicators, and trading rules
server <- function(input, output) {
  output$stock_chart <- renderPlot({
    # Filter data based on date range
    filtered_data <- data.frame(
      Date = index(stock_data),
      Close = as.numeric(Cl(stock_data))
    )
    
    filtered_data <- subset(filtered_data, Date >= input$date_range[1] & Date <= input$date_range[2])
    
    # Create base ggplot
    p <- ggplot(data = filtered_data, aes(x = Date, y = Close)) +
      geom_line(color = "black") +
      labs(title = paste(stock_symbol, "Trading Dashboard"), x = "Date", y = "Close Price") +
      theme_minimal()
    
    # Add Moving Averages conditionally (Step 3)
    if ("Moving Averages" %in% input$technical_indicators) {
      sma20 <- TTR::SMA(filtered_data$Close, n = 20)
      sma50 <- TTR::SMA(filtered_data$Close, n = 50)
      
      p <- p + 
        geom_line(aes(y = sma20), color = "blue", na.rm = TRUE) +
        geom_line(aes(y = sma50), color = "orange", na.rm = TRUE)
      
      # Implement Trading Rules and Generate Signals (Step 4)
      signals <- ifelse(sma20 > sma50, "Buy", 
                        ifelse(sma20 < sma50, "Sell", "Hold"))
      
      filtered_data$Signal <- signals
      
      # Add Signal Annotations to the Chart (Step 4)
      p <- p + geom_text(data = filtered_data, aes(x = Date, y = Close, label = Signal), 
                         vjust = 1.5, color = "purple", check_overlap = TRUE)
    }
    
    # Return the ggplot object
    print(p)
  })
}

# Run the Shiny App
shinyApp(ui, server)